# ==============================================================================
# PATHS
# ==============================================================================
# Central place for filesystem locations so no other script hardcodes them.
# Every pipeline script assumes it is run with the project root (this repo's
# top-level directory, where GATS.Rproj lives) as the working directory --
# true automatically when run via RStudio with GATS.Rproj open, or via
# `Rscript R/0X_....R` invoked from the repo root.

# Raw GATS microdata is not tracked in this repo. Override the default with
# an environment variable if your copy lives somewhere else, e.g.:
#   Sys.setenv(GATS_RAW_DATA_DIR = "/path/to/data")
RAW_DATA_DIR <- Sys.getenv(
  "GATS_RAW_DATA_DIR",
  unset = "data"
)

# Intermediate .rds files passed between pipeline stages.
INTERIM_DIR <- "data/interim"
dir.create(INTERIM_DIR, recursive = TRUE, showWarnings = FALSE)

# PNG exports of every table and figure (see R/05, R/06's downstream R/07,
# and R/04a's QC figure).
OUTPUT_DIR <- "output"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Small helper so every stage fails with a clear message instead of a
# cryptic "cannot open file" error when run out of order.
read_interim <- function(name) {
  path <- file.path(INTERIM_DIR, name)
  if (!file.exists(path)) {
    stop(
      "Missing ", path, " -- run the earlier pipeline stage that produces it ",
      "(see R/run_all.R for the required order) before this script.",
      call. = FALSE
    )
  }
  readRDS(path)
}
