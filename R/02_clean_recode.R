# ==============================================================================
# 02 CLEAN / RECODE -- shared logic, applied identically to every country
# ==============================================================================
# Reads data/interim/raw_<country>.rds (produced by R/01_import/*.R) for all
# four countries, applies the ONE set of recoding functions defined in
# R/config/recode_functions.R -- parameterized per country by
# R/config/variable_maps.R -- and writes:
#   data/interim/clean_<country>.rds   (one per country, full column set)
#   data/interim/clean_pooled.rds      (stacked, harmonized core columns only)
#
# This is the file that makes "harmonization" real: the recoding functions
# are not copy-pasted per country. Only R/config/variable_maps.R should need
# to change to add a fifth country.
#
# Standalone-runnable from the project root; requires 01_import to have run.

library(dplyr)
library(purrr)

source("R/config/paths.R")
source("R/config/variable_maps.R")
source("R/config/recode_functions.R")

# ---- Apply identically to all four countries ---------------------------------

clean_by_country <- map(country_keys, function(ck) {
  raw_df <- read_interim(paste0("raw_", ck, ".rds"))
  clean_one_country(raw_df, variable_maps[[ck]])
}) %>% set_names(country_keys)

for (ck in country_keys) {
  saveRDS(clean_by_country[[ck]], file.path(INTERIM_DIR, paste0("clean_", ck, ".rds")))
  cat(sprintf("%s: %d rows after cleaning\n", variable_maps[[ck]]$label, nrow(clean_by_country[[ck]])))
}

# ---- Stack into one pooled, harmonized dataset -------------------------------

core_vars <- c(
  "gatscluster", "gatsstrata", "gatsweight",
  "quit_smoking", "quit_smokeless", "quit_smoking_intent", "quit_slt_intent",
  "user_type", "user3", "smoker", "smokeless", "dual_use",
  "anti_tob_expo", "anti_cig_expo", "anti_slt_expo",
  "age_group", "gender", "education", "employment", "residence_label",
  "country"
)

missing_report <- map_lgl(core_vars, function(v) all(map_lgl(clean_by_country, ~ v %in% names(.x))))
if (!all(missing_report)) {
  stop("Core pooling variable(s) missing from at least one country: ",
       paste(core_vars[!missing_report], collapse = ", "), call. = FALSE)
}

pooled_data <- bind_rows(lapply(clean_by_country, function(df) select(df, all_of(core_vars))))

cat(sprintf("Pooled: %d rows across %d countries\n", nrow(pooled_data), length(country_keys)))

saveRDS(pooled_data, file.path(INTERIM_DIR, "clean_pooled.rds"))
