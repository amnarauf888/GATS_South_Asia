# ==============================================================================
# 03 DERIVE SURVEY DESIGN -- one design object per country, plus pooled
# ==============================================================================
# Reads data/interim/clean_<country>.rds and clean_pooled.rds (from
# R/02_clean_recode.R). Because R/02 already renamed each country's raw
# cluster/strata/weight columns to gatscluster/gatsstrata/gatsweight, the
# svydesign() call itself is identical for every country -- what genuinely
# differs per country is the DATA (and, for the pooled design, that strata
# must be scoped within country via interaction()), not the construction
# logic. Each country still gets its own separate design object, since a
# survey design is tied to one sample's weights/clusters and can't be shared.
#
# Also builds, per country and pooled, the "current smokers" / "current SLT
# users" subsets with fixed factor reference levels -- both R/05 (descriptive
# tables) and R/06 (models) need these ready-made analysis-ready designs, so
# building them once here (rather than duplicating the subset+relevel step in
# both places) is "survey design," not modeling.
#
# Writes data/interim/design_<country>.rds and design_pooled.rds, each a
# list(design = <full design>, smokers = <subset>, slt = <subset>).
#
# Standalone-runnable from the project root; requires 02_clean_recode to have run.

library(survey)
library(srvyr)
library(dplyr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

options(survey.lonely.psu = "adjust")

SMOKER_TYPES <- c("Smoker only", "Dual: Smoker + Smokeless")
SLT_TYPES    <- c("Smokeless only", "Dual: Smoker + Smokeless")

# Fixes factor levels so the reference category in every svyglm() model is
# the same across countries (15-24 / Female / No Education / Rural / Not
# Working / [Pakistan]).
ensure_reference_levels <- function(design, include_country = FALSE) {
  design <- update(
    design,
    age_group       = factor(age_group, levels = c("15-24", "25-44", "45-64", "65+")),
    gender          = factor(gender, levels = c("Female", "Male")),
    education       = factor(education, levels = c("No Education", "Primary or Less", "Secondary or above")),
    residence_label = factor(residence_label, levels = c("Rural", "Urban")),
    employment      = factor(employment, levels = c("Not Working", "Employed"))
  )
  if (include_country) {
    design <- update(design, country = factor(country, levels = country_order))
  }
  design
}

build_country_designs <- function(ck) {
  clean_df <- read_interim(paste0("clean_", ck, ".rds"))

  design <- svydesign(
    id      = ~gatscluster,
    strata  = ~gatsstrata,
    weights = ~gatsweight,
    data    = clean_df,
    nest    = TRUE
  )

  smokers <- subset(design, user_type %in% SMOKER_TYPES) %>% ensure_reference_levels()
  slt     <- subset(design, user_type %in% SLT_TYPES)    %>% ensure_reference_levels()

  list(design = design, smokers = smokers, slt = slt)
}

# ---- Per-country designs ------------------------------------------------------

for (ck in country_keys) {
  d <- build_country_designs(ck)
  saveRDS(d, file.path(INTERIM_DIR, paste0("design_", ck, ".rds")))
  cat(sprintf(
    "%-10s full N=%d  smokers N=%d  SLT N=%d\n",
    variable_maps[[ck]]$label,
    nrow(d$design$variables), nrow(d$smokers$variables), nrow(d$slt$variables)
  ))
}

# ---- Pooled design (strata scoped within country) -----------------------------

pooled_data <- read_interim("clean_pooled.rds")

pooled_design <- svydesign(
  id      = ~gatscluster,
  strata  = ~interaction(country, gatsstrata),
  weights = ~gatsweight,
  data    = pooled_data,
  nest    = TRUE
) %>% ensure_reference_levels(include_country = TRUE)

smokers_pooled <- subset(pooled_design, user_type %in% SMOKER_TYPES) %>% ensure_reference_levels(include_country = TRUE)
slt_pooled     <- subset(pooled_design, user_type %in% SLT_TYPES)    %>% ensure_reference_levels(include_country = TRUE)

cat(sprintf(
  "Pooled     full N=%d  smokers N=%d  SLT N=%d\n",
  nrow(pooled_design$variables), nrow(smokers_pooled$variables), nrow(slt_pooled$variables)
))

saveRDS(
  list(design = pooled_design, smokers = smokers_pooled, slt = slt_pooled),
  file.path(INTERIM_DIR, "design_pooled.rds")
)

