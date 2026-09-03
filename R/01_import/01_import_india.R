# ==============================================================================
# 01 IMPORT -- INDIA (GATS 2016-2017)
# ==============================================================================
# Standalone-runnable: reads the raw CSV and writes data/interim/raw_india.rds.
# Run from the project root (working directory = repo root).

library(readr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

map <- variable_maps$india

raw_path <- file.path(RAW_DATA_DIR, map$raw_file)
GATS_IND_raw <- read_csv(raw_path, show_col_types = FALSE)

cat(sprintf("India: read %d rows, %d columns from %s\n",
            nrow(GATS_IND_raw), ncol(GATS_IND_raw), raw_path))

saveRDS(GATS_IND_raw, file.path(INTERIM_DIR, "raw_india.rds"))
