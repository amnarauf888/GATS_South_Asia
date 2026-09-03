# ==============================================================================
# RUN ALL -- full end-to-end pipeline, in true dependency order
# ==============================================================================
# Sources every stage in the order they actually depend on each other. This
# is NOT alphabetical/numeric filename order: 04b (post-model QC) depends on
# 06 (regression models), so it must run after 06 even though "04b" sorts
# before "05" and "06" as a filename. The stage numbers describe each
# script's PURPOSE (mirroring the pipeline's conceptual stages), not
# necessarily its position in a naive sort -- this file is what encodes the
# real execution order.
#
# Every script here is also independently runnable (`Rscript R/0X_....R`)
# against the data/interim/*.rds files left by the previous stage, provided
# working directory = project root.
#
# Run with: Rscript R/run_all.R   (from the project root)
# or source("R/run_all.R") from an R session with the project root as the
# working directory (e.g. RStudio with GATS.Rproj open).

repo_root <- getwd()
stopifnot(
  "Run this from the project root (where GATS.Rproj lives)." =
    file.exists(file.path(repo_root, "GATS.Rproj"))
)

pipeline <- c(
  "R/01_import/01_import_pakistan.R",
  "R/01_import/01_import_india.R",
  "R/01_import/01_import_bangladesh.R",
  "R/01_import/01_import_sri_lanka.R",
  "R/02_clean_recode.R",
  "R/03_derive_survey_design.R",
  "R/04a_qc_pre_model.R",
  "R/05_descriptive_tables.R",
  "R/06_regression_models.R",
  "R/04b_qc_post_model.R",
  "R/07_results_tables.R"
)

for (stage in pipeline) {
  cat("\n==============================================================\n")
  cat("Running", stage, "\n")
  cat("==============================================================\n")
  source(stage, chdir = FALSE)
}

cat("\nPipeline complete. Intermediate outputs are in data/interim/.\n")
