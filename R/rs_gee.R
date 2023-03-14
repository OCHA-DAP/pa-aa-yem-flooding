#' get_geomorph_landforms
#'
#' @return image or tidyee with image
#' @export
#'
#' @examples
get_geomorph_landform_indicators <- function(return_tidyee = F) {
  srtm_landforms <- ee$Image("CSP/ERGo/1_0/Global/SRTM_landforms")
  alos_landforms <- ee$Image("CSP/ERGo/1_0/Global/ALOS_landforms")

  res <- srtm_landforms$rename("srtm_landforms")$
    addBands(alos_landforms$rename("alos_landforms"))
  if (return_tidyee) {
    res <- as_tidyee(res)
  }

  return(res)
}

extract_geomorph_landform_indicators <- function(geom_sf, img_scale) {
  geom_ee <- rgee::sf_as_ee(geom_sf)
  img <- get_geomorph_landform_indicators()
  fc_values <- img$sampleRegions(collection = geom_ee, scale = img_scale)
  if (nrow(geom_sf) > 5000) {
    df_values <- rgee::ee_as_sf(fc_values, via = "drive")
  }
  if (nrow(geom_sf) <= 5000) {
    df_values <- rgee::ee_as_sf(fc_values, maxFeatures = 5000)
  }
  return(df_values)
}



srtm_landform_lookup <- tibble::tribble(
  ~value, ~color, ~description,
  11L, "#141414", "Peak/ridge (warm)",
  12L, "#383838", "Peak/ridge",
  13L, "#808080", "Peak/ridge (cool)",
  14L, "#EBEB8F", "Mountain/divide",
  15L, "#F7D311", "Cliff",
  21L, "#AA0000", "Upper slope (warm)",
  22L, "#D89382", "Upper slope",
  23L, "#DDC9C9", "Upper slope (cool)",
  24L, "#DCCDCE", "Upper slope (flat)",
  31L, "#1C6330", "Lower slope (warm)",
  32L, "#68AA63", "Lower slope",
  33L, "#B5C98E", "Lower slope (cool)",
  34L, "#E1F0E5", "Lower slope (flat)",
  41L, "#a975ba", "Valley",
  42L, "#6f198c", "Valley (narrow)"
)

alos_landform_lookup <- tibble::tribble(
  ~value, ~color, ~description,
  11L, "#141414", "Peak/ridge (warm)",
  12L, "#383838", "Peak/ridge",
  13L, "#808080", "Peak/ridge (cool)",
  14L, "#EBEB8F", "Mountain/divide",
  15L, "#F7D311", "Cliff",
  21L, "#AA0000", "Upper slope (warm)",
  22L, "#D89382", "Upper slope",
  23L, "#DDC9C9", "Upper slope (cool)",
  24L, "#DCCDCE", "Upper slope (flat)",
  31L, "#1C6330", "Lower slope (warm)",
  32L, "#68AA63", "Lower slope",
  33L, "#B5C98E", "Lower slope (cool)",
  34L, "#E1F0E5", "Lower slope (flat)",
  41L, "#a975ba", "Valley",
  42L, "#6f198c", "Valley (narrow)"
)



recode_from_gee_lookup_table <- function(df, col, lookup) {
  lookup_dict <- lookup[["description"]] |>
    rlang::set_names(lookup[["value"]])
  df |>
    mutate(
      {{ col }} := recode({{ col }}, !!!lookup_dict)
    )
}

recode_srtm_alos_categorical <- function(df) {
  df |>
    recode_from_gee_lookup_table(col = alos_landforms, lookup = alos_landform_lookup) |>
    recode_from_gee_lookup_table(col = srtm_landforms, lookup = srtm_landform_lookup)
}




ee_dist_jrc_max_extent <- function(pt,
                                   boolean = "=",
                                   val = 2,
                                   scale,
                                   tidy_extract = T, via) {
  jrc <- ee$Image("JRC/GSW1_4/GlobalSurfaceWater")

  jrc_max <- jrc$select("max_extent")
  jrc_max_masked <- jrc_max$selfMask()
  jrc_max_masked <- jrc_max_masked$set("system:time_start", ee$Date("2022-02-24"))
  ee_closest_distance_to_val(x = jrc_max_masked, y = pt, boolean = boolean, val = val, scale = scale, tidy_extract = tidy_extract, via = via)
}


#' Title
#'
#' @param boolean
#' @param val
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(surveyGEER)
#' ee_bool <- switch_boolean(boolean = ">", val = 25)
#' }
switch_boolean <- function(boolean, val) {
  switch(boolean,
    ">" = function(x) x$gt(val),
    ">=" = function(x) x$gte(val),
    "<" = function(x) x$lt(val),
    "<=" = function(x) x$lte(val),
    "=" = function(x) x$eq(val),
    NULL
  )
}




#' ee_closest_distance_to_val
#'
#' @param x
#' @param y
#' @param boolean
#' @param val
#' @param scale
#' @param tidy_extract
#' @param via
#'
#' @return data.frame with closest distance value for each input feature
#' @export
#'
#' @examples
ee_closest_distance_to_val <- function(x,
                                       y,
                                       boolean = "=",
                                       val = 2,
                                       scale,
                                       tidy_extract = T, via) {
  UseMethod("ee_closest_distance_to_val")
}


#' @name ee_closest_distance_to_val
#' @export
ee_closest_distance_to_val.ee.imagecollection.ImageCollection <- function(x,
                                                                          y,
                                                                          boolean = "=",
                                                                          val = 2,
                                                                          scale,
                                                                          tidy_extract = T, via) {
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  boolean_mask_cond <- switch_boolean(boolean = boolean, val = val)

  x_masked <- x$map(
    function(image) {
      image_masked <- boolean_mask_cond(image)$
        selfMask()
      return(ee$Image(image_masked$copyProperties(image, image$propertyNames())))
    }
  )

  cat(crayon::green("Generating distance raster(s)\n"))
  euclidean_distance_to_x <- x_masked$
    map(
    function(image) {
      distances <- image$mask()$
        fastDistanceTransform({
        neighborhood <- 1024 * 3
      })$multiply(ee$Image$pixelArea())$sqrt()$rename("distance_to")$
        reproject(crs = "EPSG:4326", scale = scale)
      return(ee$Image(distances$copyProperties(image, image$propertyNames())))
    }
  )
  cat(crayon::green("Extracting distance raster values to y\n"))

  if (tidy_extract) {
    res <- tidyrgee::ee_extract_tidy(
      x = euclidean_distance_to_x$select("distance_to"),
      y = y,
      scale = scale, via = via
    )
  }

  return(res)
}

#' @name ee_closest_distance_to_val
#' @export
ee_closest_distance_to_val.ee.image.Image <- function(x,
                                                      y,
                                                      boolean = "=",
                                                      val = 2,
                                                      scale,
                                                      tidy_extract = T, via = via) {
  # stopifnot(!is.null(x), inherits(x, "ee.imagecollection.ImageCollection"))
  assertthat:::assert_that(!is.null(x), inherits(x, "ee.image.Image"))
  boolean_mask_cond <- switch_boolean(boolean = boolean, val = val)

  cat(crayon::green("masking x image/imageCollection\n"))
  x_masked <- ee$Image(
    boolean_mask_cond(x)$
      selfMask()$
      copyProperties(x, x$propertyNames())
  )
  cat(crayon::green("Generating distance raster(s)\n"))

  FDT <- x_masked$mask()$
    fastDistanceTransform({
    neighborhood <- 1024 * 3
  })

  distances <- FDT$
    multiply(ee$Image$pixelArea())$
    sqrt()$
    rename("distance_to")$
    reproject(crs = "EPSG:4326", scale = scale)

  euclidean_distance_to_x <- ee$Image(distances$copyProperties(x_masked, x_masked$propertyNames()))

  cat(crayon::green("Extracting distance raster values to y\n"))

  if (tidy_extract) {
    res <- ee_extract_tidy(x = euclidean_distance_to_x$select("distance_to"), y = y, scale = 30, via = via)
  }
  if (!tidy_extract) {
    geom_ee <- rgee::sf_as_ee(sf)
    res_fc <- euclidean_distance_to_x$
      select("distance_to")$
      sampleRegions({
      list(
        collection = geom_ee,
        scale = scale
      )
    })
    res <- rgee::ee_as_sf(res_fc)
  }

  return(res)
}
