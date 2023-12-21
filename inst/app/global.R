# libraries ----
librarian::shelf(
  bslib,
  dplyr, DT, fs, geojsonio, glue, here,
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
#  - [ ] add function to download and import if not exists. See https://blog.r-hub.io/2020/05/29/distribute-data/#data-outside-of-your-package

# duckdb connection to AquaMaps ----
dir_bigdata     <- ifelse(
  Sys.info()[["sysname"]] == "Linux",
  "/share/data/aquamapsduckdb",
  "/Users/bbest/My Drive/projects/msens/data")
path_am         <- glue("{dir_bigdata}/am.duckdb")
dir_data        <- here("inst/app/data")
nspp_tif        <- glue("{dir_data}/am_nspp.tif")
nspp_3857_tif   <- glue("{dir_data}/am_nspp_3857.tif")

if (!file.exists(path_am)){
  message("reimporting AquaMaps database")

  dir_dbexport <- glue("{dir_bigdata}/am.duckdb_export")

  stopifnot(dir.exists(dir_bigdata))
  stopifnot(dir.exists(dir_dbexport))

  con_am <- dbConnect(
    duckdb(
      dbdir     = path_am,
      read_only = F))

  dbExecute(con_am, glue("IMPORT DATABASE '{dir_dbexport}'"))
  dbDisconnect(con_am, shutdown = T)
}

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

# recreate other data files ----
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
ply_rgns <- msens::ply_rgns_s05

lst_rgns <- ply_rgns_s05 |>
  st_drop_geometry() |>
  arrange(shlf_name, rgn_name) |>
  group_by(shlf_name) |>
  summarize(
    rgns = list(setNames(rgn_key, rgn_name))) |>
  deframe()
lst_rgns <- c(
  list(Overview = setNames("_NA_", "None")),
  lst_rgns)

# bounding box initially around regions
b <- st_bbox(ply_rgns) |> as.numeric()

# global defaults ----
ply_g <- ext(-180, 180, -90, 90) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf(crs = 4326)
st_geometry(ply_g) <- "geom"
attr(ply_g, 'name') <- "Globe"

d_spp_g <- am_spp_in_ply()
