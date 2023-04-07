#' return_period_level_tibble
#' @description  calculate return period from data frame containing vector of daily or window aggregated rainfall.
#' @param df data.frame with date & value vectors.
#' @param value \code{character} name of value column to use. By default the data.frame we are providing (`zonal_stats_high_risk_hull`) has
#'  both the median and mean values. this argument allows user to specify which to use
#' @param date \code{character}  date column name (default = "date")
#' @param rp_year \code{integer} vector contain return periods used to calculate levels
#' @return The result of `extRemes::gevd()` followed by `extremes::return.level()` as a tidy tibble instead of a model "ci" object.
#'  as a tibble it is easier to incorporate into downstream plots/analysis. `broom::tidy` has a variety of s3 classes for
#'  model result classes, but doesn't seem to have one for this. Therefore, we have to make it custom.

return_period_level_tibble <- function(df, value = "mean", date = "date", rp_year = c(2, 3, 4, 5, 10)) {
  df_year_max <- df %>%
    # can remove governorate if its going to be split on governorate
    group_by(governorate_name, year = year(!!sym(date))) %>%
    summarise(
      across(c(value), ~ max(.x, na.rm = T)),
      .groups = "drop"
    )

  gev_fit <- fevd(df_year_max[[value]], type = "GEV")
  return_levels_ci <- return.level(
    x = gev_fit,
    return.period = rp_year,
    do.ci = TRUE,
    alpha = 0.05
  )
  tibble(
    RP = rp_year,
    estimate_low = xyz.coords(return_levels_ci)$x,
    estimate_upp = xyz.coords(return_levels_ci)$z,
    estimate = xyz.coords(return_levels_ci)$y
  )
}



#' plot_rainfall_rps_impact
#' @description A big wrapper to combine a bunch of analysis on CCCM data (reported floods/impact) and historical rainfall into plots.
#'
#' @param impact_data
#' @param historical_rainfall data set with historical rainfall
#' @param precip_regime \code{character} which precipitation regime to use. Options are:
#'  'precip_daily'
#'  'roll3'
#'  'roll5'
#'  'roll10' (default)
#'  'roll15'
#'  'roll20'
#'  'roll25'
#'  'roll30'
#' @param impact_var \code{character} specify name of which numeric impact variable to use
#' @param rp_year \code{integer} vector contain return periods used to calculate levels
#' @param scale \code{logical} whether to scale lat, lon, date when clustering (default = F)
#' @param k  \code{integer} number of clusters to create with kmeans (default = 5)
#' @param aggregate_impact \code{character} if NULL (default) events are not aggregated and points represent individual events
#'
#' @return dual-y axis plots. left y-axis rainfall quantity for specified rainfall regime (mm), right y-axis: specified imapct parameter number.
#'  Horizontal lines are aligned with left y-axis indicating return level for user specified return periods. Points align with right y-axis and represent either
#'  individual incidents and the user-specified impact parameter figure or grouped/aggregated events/impact.

plot_rainfall_rps_impact <- function(impact_data = cccm_flood_impact_data_w_coords,
                                     historical_rainfall = zonal_stats_high_risk_hull,
                                     precip_regime = "roll10",
                                     impact_var,
                                     rp_year,
                                     scale = F,
                                     k = 5,
                                     aggregate_impact = NULL,
                                     remove_wind=F) {
  historical_rainfall_df <- historical_rainfall[[precip_regime]]
  historical_rainfall_df$labels <- paste0(
    "<br> date: ", historical_rainfall_df$date,
    "<br> precip: ", round(historical_rainfall_df$mean, 1)
  ) %>% lapply(htmltools::HTML)

  historical_rainfall_list <- historical_rainfall_df %>%
    split(.$governorate_name)

  impact_data_filtered <- impact_data %>%
    group_by(governorate_name, site_id, lon, lat, date = ymd(date)) %>%
    # there are some cases where the same event was reported 2x by different
    # agencies -- therefore we should sum these figs
    summarise(
      across(starts_with("num_"), ~ mean(.x, na.rm = T)),
      .groups = "drop"
    ) %>%
    filter(governorate_name %in% c("Marib", "Hajjah"))

  pt_cluster_list <- spatial_pt_clusters(
    df = impact_data_filtered,
    date = "date",
    lon = "lon",
    lat = "lat",
    k = k,
    event = NULL, # all events in this dataset
    # impact_var = "num_shelters_affected",
    # rp_year = c(2,3,4,5,10),
    scale = scale
  ) %>%
    map(\(pt_sf){
        
      pt_df <- pt_sf %>%
        st_drop_geometry()
      
      if(remove_wind){
          pt_df <- pt_df %>% 
              filter(
                  # this whole cluster appears to be wind damage rather than rainfall
                  cluster !="Hajjah_2",
                  # confirmed that this particular event was wind rather than rainfall
                  !(site_id =="YE2613_1961"  & date =="2022-04-30")
              )
      }
      
      pt_df$pt_labels <- paste0(
        "Site_ID:", pt_df$site_id,
        "<br> date:", pt_df$date,
        "<br> cluter:", pt_df$cluster,
        "<br> Shelters affected: ", pt_df$num_shelters_affected,
        "<br> Number HHs verified: ", pt_df$num_verified_hhs
      ) %>% lapply(htmltools::HTML)
      return(pt_df)
    })
  

  cluster_palette <- RColorBrewer::brewer.pal(n = k, name = "Spectral")

  map2(pt_cluster_list, historical_rainfall_list, \(impact, rainfall){
    pt_size <- 2
    if (!is.null(aggregate_impact)) {
      impact <- impact %>%
        group_by(governorate_name, cluster) %>%
        summarise(
          min_date = min(date),
          max_date = max(date),
          date = median(date),
          across(starts_with("num_"), ~ sum(.x, na.rm = T)),
          num_events = n(),
          .groups = "drop"
        )
      impact$pt_labels <- paste0(
        "<br> cluter: ", impact$cluster,
        "<br> date: ", impact$date,
        "<br> First date: ", impact$min_date,
        "<br> Last date: ", impact$max_date,
        "<br> Shelters affected: ", impact$num_shelters_affected,
        "<br> Number HHs verified: ", impact$num_verified_hhs,
        "<br> Number events: ", impact$num_events
      ) %>% lapply(htmltools::HTML)

      impact_var <- ifelse(aggregate_impact == "num_events", "num_events", impact_var)
      pt_size <- 5
    }

    
    n_colors <- length(unique(impact$cluster))
    cluster_palette <- RColorBrewer::brewer.pal(n = n_colors, name = "Spectral")
    # to keep cluster colors from changing? - also need to set.seed in clustering
    named_cluster_palette <- cluster_palette %>%
      set_names(
        impact %>%
          mutate(cluster_num = parse_number(cluster)) %>%
          distinct(cluster_num, cluster) %>%
          arrange(cluster_num) %>%
          pull(cluster) %>%
          unique()
      )

    return_level_tbl <- return_period_level_tibble(df = rainfall, value = "mean", date = "date", rp_year = rp_year)
    return_level_tbl <- return_level_tbl %>%
      mutate(
        label = glue("{RP} year return period level: {round(estimate,1)} mm")
      )

    date_range <- range(impact$date)
    date_incl <- seq((date_range[1] - 30), (date_range[2] + 30), by = "day")
    date_center <- median(date_incl)
    rainfall_filt <- rainfall %>%
      mutate(date = ymd(date)) %>%
      filter(date %in% date_incl)
    max_rainfall <- max(rainfall_filt$mean, na.rm = T)
    max_impact <- max(impact[[impact_var]], na.rm = T)
    scale_y <- max_impact / max_rainfall
    nm <- unique(impact$governorate_name)

    if (is.null(aggregate_impact)) {
      p <- ggplot(
        data = rainfall_filt,
        aes(x = date, y = mean)
      ) +
        geom_line() +
        geom_point_interactive(aes(tooltip = labels), size = 0.05) +
        geom_hline(yintercept = return_level_tbl$estimate) +
        geom_text(
          data = return_level_tbl,
          aes(
            x = ymd(date_center),
            y = estimate + 1.5,
            label = label
          )
        ) +
        geom_point_interactive(
          data = impact,
          aes(
            x = date,
            y = !!sym(impact_var) * (1 / scale_y),
            color = cluster,
            tooltip = pt_labels
          ),
          size = pt_size
        ) +
        scale_color_manual(values = named_cluster_palette) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . * scale_y, name = impact_var)) +
        scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(
          y = glue("Precip ({precip_regime} mm)"),
          title = "Yemen Rainfall and Impact",
          subtitle = glue("Yemen - {nm}")
        ) +
        theme_hdx() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)
        )
    }
    if (!is.null(aggregate_impact)) {
      p <- ggplot(
        data = rainfall_filt,
        aes(x = date, y = mean)
      ) +
        geom_line() +
        geom_point_interactive(aes(tooltip = labels), size = 0.05) +
        geom_hline(yintercept = return_level_tbl$estimate) +
        geom_text(
          data = return_level_tbl,
          aes(
            x = ymd(date_center),
            y = estimate + 1.5,
            label = label
          )
        ) +
        geom_point_interactive(
          data = impact,
          aes(
            x = date,
            y = !!sym(impact_var) * (1 / scale_y),
            color = cluster,
            tooltip = pt_labels
          ),
          size = pt_size
        ) +
        scale_color_manual(values = named_cluster_palette) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ . * scale_y, name = impact_var)) +
        scale_x_date(breaks = "1 month", date_labels = "%b-%y") +
        labs(
          y = glue("Precip ({precip_regime} mm)"),
          title = "Yemen Rainfall and Impact",
          subtitle = glue("Yemen - {nm}")
        ) +
        theme_hdx() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)
        )
    }
    return(ggiraph::girafe(ggobj = p))
  }) %>%
    set_names(names(pt_cluster_list))
}
