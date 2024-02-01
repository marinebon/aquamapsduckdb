# create am.duckdb from original aquamapsdata sqlite am.db
# motivation: queries in SQLite are quite slow compared to the new DuckDB.

# dependency for aquamapsdata:
#  - Terminal: brew install gnupg
#  - R: install.packages("rcrypt")
librarian::shelf(
  raquamaps/aquamapsdata,
  DBI, dplyr, DT, duckdb, fs, glue, here, janitor,
  leaflet, librarian, mapview,
  readr, sf, stringr, terra, tibble, zip,
  quiet = T)
# The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
# which was just loaded, will retire in October 2023.

dir_bigdata  <- glue("/Users/bbest/My Drive/projects/msens/data/derived/aquamaps")
path_duckdb  <- glue("{dir_bigdata}/am.duckdb")
dir_dbexport <- glue("{dir_bigdata}/am.duckdb_export")
dbexport_zip <- glue("{dir_dbexport}.zip")

# OLD sqlite db ----

# aquamapsdata::download_db() # downloads about 2 GB of data, approx 10 GB when unpacked

# data(package = "raquamaps")
con_sl <- default_db("sqlite") # /Users/bbest/Library/Application Support/aquamaps/am.db
file_size(aquamapsdata::am_db_sqlite()) # 11.2G

dbListTables(con_sl)
# [1] "fts"                 "fts_config"          "fts_content"
# [4] "fts_data"            "fts_docsize"         "fts_idx"
# [7] "hcaf_r"              "hcaf_species_native" "hspen_r"
# [10] "occurrencecells_r"   "speciesoccursum_r"

# label: list columns intbls
for (tbl in dbListTables(con_sl)){
  tbl(con_sl, tbl) |>
    print()
}

# NEW duck db ----

# file_delete(path_dd)
con_dd   <- dbConnect(
  duckdb(
    dbdir     = path_duckdb,
    read_only = T))
# dbDisconnect(con_dd, shutdown = T)

# transfer to duckdb ----

#| label: xfer to duckdb

# table rename from old sqlite (sl) to new duckdb (dd)
d_tbls <- tribble(
  ~tbl_sl,                ~tbl_dd,
  "hcaf_r",	              "cells",
  "hcaf_species_native",	"spp_cells",
  "hspen_r",	            "spp_prefs",
  "occurrencecells_r",	  "spp_occs",
  "speciesoccursum_r",	  "spp")
d_tbls

redo <- F
if (!all(d_tbls$tbl_dd %in% dbListTables(con_dd)) | redo){

  for (i in 1:nrow(d_tbls)){ # i = 1
    tbl_sl <- d_tbls$tbl_sl[i]
    tbl_dd <- d_tbls$tbl_dd[i]

    message(glue(
      "{i} of {nrow(d_tbls)} tbls: read sqlite.{tbl_sl} () ~ {Sys.time()}"))
    t0 <- Sys.time()

    d <- dbGetQuery(con_sl, glue(
      "SELECT * FROM {tbl_sl}")) |>
      clean_names() |>
      rename_with(
        \(x) x |>
          case_match(
            # cells
            "id"         ~ "cell_id",
            "slimit"     ~ "s_limit",
            # spp, spp_cells, spp_occs, spp_prefs
            "species_id" ~ "sp_key",
            "speccode"   ~ "sp_int",
            "spec_code"  ~ "sp_int",
            # spp_occs
            "record_id"  ~ "occ_id",
            # spp
            "f_bname"    ~ "common_name",
            .default = x))

    t1 <- Sys.time()
    message(paste(
      "    ", format(nrow(d), big.mark=','), "rows read in",
      round(difftime(t1, t0, units="mins"), 4), "mins"))

    message(glue(
      "  write duckdb.{tbl_dd} ~ {Sys.time()}",
      .trim = F))

    dbWriteTable(con_dd, tbl_dd, d, overwrite = T)

    t2 <- Sys.time()
    message(paste(
      "    ", format(nrow(d), big.mark=','), "rows written in",
      round(difftime(t2, t1, units="mins"), 4), "mins"))

  }
}

# 1 of 5 tbls: read sqlite.hcaf_r () ~ 2023-12-01 21:48:46.1486
# 177,869 rows read in 0.0264 mins
# write duckdb.cells ~ 2023-12-01 21:48:47.731827
# 177,869 rows written in 0.0099 mins
# 2 of 5 tbls: read sqlite.hcaf_species_native () ~ 2023-12-01 21:48:48.326646
# 118,249,855 rows read in 2.7802 mins
# write duckdb.spp_cells ~ 2023-12-01 21:51:35.139909
# 118,249,855 rows written in  1.2148  mins
# 3 of 5 tbls: read sqlite.hspen_r () ~ 2023-12-01 21:52:48.026475
# 23,699 rows read in 0.0039 mins
# write duckdb.spp_prefs ~ 2023-12-01 21:52:48.259237
# 23,699 rows written in 0.0011 mins
# 4 of 5 tbls: read sqlite.occurrencecells_r () ~ 2023-12-01 21:52:48.323373
# 2,908,181 rows read in 0.1266 mins
# write duckdb.spp_occs ~ 2023-12-01 21:52:55.917685
# 2,908,181 rows written in 0.0438 mins
# 5 of 5 tbls: read sqlite.speciesoccursum_r () ~ 2023-12-01 21:52:58.547241
# 23,699 rows read in 0.0022 mins
# write duckdb.spp ~ 2023-12-01 21:52:58.679833
# 23,699 rows written in 0.0015 mins


## rename fields

#| label: _tbl_fld_renames

file_size(path_dd) # 3.46G
renames_csv <- glue("{dir_bigdata}/am_tbl_fld_renames.csv")

dbListTables(con_dd)
# dbDisconnect(con_dd, shutdown = T)

if (!file.exists(renames_csv) | redo){
  for (i in 1:nrow(d_tbls)){ # i = 1
    tbl_sl <- d_tbls$tbl_sl[i]
    tbl_dd <- d_tbls$tbl_dd[i]

    d_sl <- dbGetQuery(con_sl, glue(
      "SELECT * FROM {tbl_sl} LIMIT 10"))
    d_dd <- dbGetQuery(con_dd, glue(
      "SELECT * FROM {tbl_dd} LIMIT 10"))

    d_r <- tibble(
      tbl_old = tbl_sl,
      tbl_new = tbl_dd,
      fld_old = names(d_sl),
      fld_new = names(d_dd))

    if (i == 1){
      d_renames <- d_r
    } else {
      d_renames <- d_renames |>
        bind_rows(d_r)
    }
  }
  write_csv(d_renames, renames_csv)
  dbWriteTable(con_dd, "_tbl_fld_renames", d_renames, overwrite = T)
}
d_renames <- read_csv(renames_csv)

datatable(d_renames)

# streamline `spp_cells`, `spp_occs` to use `cell_id` ----

if (F){
  d_spp_cells <- tbl(con_dd, "spp_cells") |>
    left_join(
      tbl(con_dd, "cells") |>
        select(cell_id, csquare_code),
      by = "csquare_code") |>
    select(-csquare_code, -center_lat, -center_long) |>
    collect()
  dbWriteTable(con_dd, "spp_cells", d_spp_cells, overwrite = T)

  d_spp_occs <- tbl(con_dd, "spp_occs") |>
    left_join(
      tbl(con_dd, "cells") |>
        select(cell_id, csquare_code),
      by = "csquare_code") |>
    select(-csquare_code) |>
    collect()
  dbWriteTable(con_dd, "spp_occs", d_spp_occs, overwrite = T)
}

# TODO: update _tbl_fld_renames to this streamlining
#   spp_cells.csquare_code|center_lat|center_long -> cell_id
#   spp_occs.csquare_code                         -> cell_id

# add indexes ----

create_index <- function(con, tbl, flds, is_unique = F){
  unq  <- {ifelse(is_unique, 'UNIQUE','')}
  idx  <- glue("{tbl}_{paste(flds, collapse='_')}_idx")
  flds <- glue("{paste(flds, collapse=',')}")
  sql  <- glue("CREATE {unq} INDEX {idx} ON {tbl} ({flds});")

  message(sql)
  dbExecute(con, sql)
}

if (F){
  create_index(con_dd, "cells",     "cell_id", is_unique = T)
  create_index(con_dd, "spp",       "sp_key",  is_unique = T)
  create_index(con_dd, "spp_cells", "cell_id")
  create_index(con_dd, "spp_cells", "sp_key")
  create_index(con_dd, "spp_prefs", "sp_key", is_unique = T)
  create_index(con_dd, "spp_occs",  "occ_id", is_unique = T)
  create_index(con_dd, "spp_occs",  "cell_id")
  create_index(con_dd, "spp_occs",  "sp_key")
}

# export/import db ----

# for:
# - version compatibility (since duckdb is not backwards compatible with itself before version 1.0); and
# - reducing file size

# * export duckdb ----

dbExecute(con_dd, glue("EXPORT DATABASE '{dir_dbexport}' (FORMAT PARQUET)"))

zip(
  zipfile           = dbexport_zip,
  files             = dir_dbexport,
  recurse           = T,
  compression_level = 9)


dbExecute(con_dd, glue("IMPORT DATABASE '{dir_dbexport}' (FORMAT PARQUET)"))
