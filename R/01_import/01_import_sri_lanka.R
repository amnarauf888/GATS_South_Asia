# ==============================================================================
# 01 IMPORT -- SRI LANKA (GATS 2019)
# ==============================================================================
# Standalone-runnable: reads the raw CSV and writes data/interim/raw_sri_lanka.rds.
# Run from the project root (working directory = repo root).

library(readr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

map <- variable_maps$sri_lanka

raw_path <- file.path(RAW_DATA_DIR, map$raw_file)
GATS_SL_raw <- read_csv(raw_path, show_col_types = FALSE)

cat(sprintf("Sri Lanka: read %d rows, %d columns from %s\n",
            nrow(GATS_SL_raw), ncol(GATS_SL_raw), raw_path))

saveRDS(GATS_SL_raw, file.path(INTERIM_DIR, "raw_sri_lanka.rds"))
