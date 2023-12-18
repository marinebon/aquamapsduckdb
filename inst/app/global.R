# libraries ----
librarian::shelf(
  bslib,
  dplyr, DT, geojsonio, glue, here,
  leaflet,
  micahwilhelm/leaflet.extras, # addDrawToolbar()
  MarineSensitivity/msens,
  yogevherz/plotme,  # count_to_treemap()
  plotly,
  shiny,
  # shinydashboard,
  sf, tibble)

source(here("inst/app/functions.R"))

# TODO:
#  - [ ] copy duckdb to server
#  - [ ] migrate functions.R to aquamapsduckdb R package
#  - [ ] swap out shinydashboard for bslib
#        https://rstudio.github.io/bslib/articles/dashboards/

dir_data        <- here("inst/app/data")
sanctuaries_rds <- glue("{dir_data}/sanctuaries.rds")
nspp_tif        <- glue("{dir_data}/am_nspp.tif")
nspp_3857_tif   <- glue("{dir_data}/am_nspp_3857.tif")

if (!file.exists(sanctuaries_rds))
  download.file(
    "https://github.com/noaa-onms/climate-dashboard/raw/main/data/sanctuaries.rds",
    sanctuaries_rds)
sanctuaries <- readRDS(sanctuaries_rds) |>
  filter(nms != "TBNMS") # exclude Thunder Bay in Great Lakes

# con_am start/stop ----
message("connecting to AquaMaps database")
con_am <- dbConnect(
  duckdb(
    dbdir     = path_am,
    read_only = T))

onStop(function() {
  message("shutting down AquaMaps database")
  dbDisconnect(con_am, shutdown = TRUE)
})

if (!file.exists(nspp_3857_tif)){

  r <- am_rast_nspp()

  writeRaster(
    r,
    nspp_tif,
    overwrite = T,
    datatype  = "INT2U",
    gdal      = c(
      "TILED=YES",
      "COMPRESS=DEFLATE"))

  # trim since leaflet can't display at the poles
  e <- ext(r)
  e$ymin <- max(e$ymin, -89)  # -85
  e$ymax <- min(e$ymax,  89)  #  85
  r <- crop(r, e)
  r_nspp_3857 <- project(r, "epsg:3857")

  writeRaster(
    r_nspp_3857,
    nspp_3857_tif,
    overwrite = T,
    datatype  = "INT2U",
    gdal      = c(
      "TILED=YES",
      "COMPRESS=DEFLATE"))
}
r_nspp_3857 <- rast(nspp_3857_tif)

# global defaults ----

lst_sancts <- c(
  setNames("_NA_", "None"),
  with(
    sanctuaries,
    setNames(nms, sanctuary)))

# bounding box initially around sanctuaries
b <- st_bbox(sanctuaries) |> as.numeric()

# global defaults ----
ply_g <- ext(-180, 180, -90, 90) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf(crs = 4326)

ply_g <- ext(-180, 180, -90, 90) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf(crs = 4326)
st_geometry(ply_g) <- "geom"
attr(ply_g, 'name') <- "Globe"

d_spp_g <- am_spp_in_ply()
