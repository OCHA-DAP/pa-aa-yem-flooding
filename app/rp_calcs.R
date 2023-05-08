#' COPIED FROM R/ dir for shiny bundling

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
