# ==============================================================================
# 01 IMPORT -- BANGLADESH (GATS 2017)
# ==============================================================================
# Standalone-runnable: reads the raw CSV and writes data/interim/raw_bangladesh.rds.
# Run from the project root (working directory = repo root).

library(readr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

map <- variable_maps$bangladesh

raw_path <- file.path(RAW_DATA_DIR, map$raw_file)
GATS_BA_raw <- read_csv(raw_path, show_col_types = FALSE)

cat(sprintf("Bangladesh: read %d rows, %d columns from %s\n",
            nrow(GATS_BA_raw), ncol(GATS_BA_raw), raw_path))

saveRDS(GATS_BA_raw, file.path(INTERIM_DIR, "raw_bangladesh.rds"))
