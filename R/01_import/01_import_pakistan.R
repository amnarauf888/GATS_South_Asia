# ==============================================================================
# 01 IMPORT -- PAKISTAN (GATS 2014)
# ==============================================================================
# Standalone-runnable: reads the raw CSV and writes data/interim/raw_pakistan.rds.
# Run from the project root (working directory = repo root).

library(readr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

map <- variable_maps$pakistan

raw_path <- file.path(RAW_DATA_DIR, map$raw_file)
GATS_PK_raw <- read_csv(raw_path, show_col_types = FALSE)

cat(sprintf("Pakistan: read %d rows, %d columns from %s\n",
            nrow(GATS_PK_raw), ncol(GATS_PK_raw), raw_path))

saveRDS(GATS_PK_raw, file.path(INTERIM_DIR, "raw_pakistan.rds"))
