# ==============================================================================
# 06 REGRESSION MODELS -- fit only, no formatting
# ==============================================================================
# Fits each country's smoking/SLT quit-attempt and quit-intention models
# ONCE and keeps them in a named list keyed by country (country_models), plus
# the pooled models with a `country` covariate (pooled_models) and the
# no-country sensitivity models (pooled_models_no_country).
#
# The "country-stratified" results the pooled analysis reports are NOT a
# separate fitting step -- they are exactly these same per-country model
# objects, formatted differently in R/07. Refitting them a second time inside
# a "pooled" step (as a naive port might) would risk two different fitted
# objects both claiming to be "Pakistan's model."
#
# Writes data/interim/models.rds: list(country_models, pooled_models,
# pooled_models_no_country).
#
# Standalone-runnable from the project root; requires 01-03 to have run.

library(survey)
library(dplyr)

source("R/config/paths.R")
source("R/config/variable_maps.R")

MODEL_RHS <- "dual_use + age_group + gender + education + residence_label + employment"

fit_country_models <- function(d) {
  list(
    smk_attempt = svyglm(as.formula(paste("quit_smoking ~ anti_cig_expo +", MODEL_RHS)),
                          design = d$smokers, family = binomial(link = "logit")),
    smk_intent  = svyglm(as.formula(paste("quit_smoking_intent ~ anti_cig_expo +", MODEL_RHS)),
                          design = d$smokers, family = binomial(link = "logit")),
    slt_attempt = svyglm(as.formula(paste("quit_smokeless ~ anti_slt_expo +", MODEL_RHS)),
                          design = d$slt, family = binomial(link = "logit")),
    slt_intent  = svyglm(as.formula(paste("quit_slt_intent ~ anti_slt_expo +", MODEL_RHS)),
                          design = d$slt, family = binomial(link = "logit"))
  )
}

# ---- Per-country models --------------------------------------------------------

country_models <- lapply(country_keys, function(ck) {
  d <- read_interim(paste0("design_", ck, ".rds"))
  m <- fit_country_models(d)
  cat(sprintf(
    "%-10s smk_attempt N=%d  smk_intent N=%d  slt_attempt N=%d  slt_intent N=%d\n",
    variable_maps[[ck]]$label,
    length(m$smk_attempt$y), length(m$smk_intent$y),
    length(m$slt_attempt$y), length(m$slt_intent$y)
  ))
  m
}) %>% setNames(country_keys)

# ---- Pooled models (country as covariate) --------------------------------------

pooled <- read_interim("design_pooled.rds")
POOLED_RHS <- paste(MODEL_RHS, "+ country")

pooled_models <- list(
  smk_attempt = svyglm(as.formula(paste("quit_smoking ~ anti_cig_expo +", POOLED_RHS)),
                        design = pooled$smokers, family = binomial(link = "logit")),
  smk_intent  = svyglm(as.formula(paste("quit_smoking_intent ~ anti_cig_expo +", POOLED_RHS)),
                        design = pooled$smokers, family = binomial(link = "logit")),
  slt_attempt = svyglm(as.formula(paste("quit_smokeless ~ anti_slt_expo +", POOLED_RHS)),
                        design = pooled$slt, family = binomial(link = "logit")),
  slt_intent  = svyglm(as.formula(paste("quit_slt_intent ~ anti_slt_expo +", POOLED_RHS)),
                        design = pooled$slt, family = binomial(link = "logit"))
)

cat(sprintf(
  "Pooled     smk_attempt N=%d  smk_intent N=%d  slt_attempt N=%d  slt_intent N=%d\n",
  length(pooled_models$smk_attempt$y), length(pooled_models$smk_intent$y),
  length(pooled_models$slt_attempt$y), length(pooled_models$slt_intent$y)
))

# ---- Pooled sensitivity models (country dropped, dual_use retained) -----------

pooled_models_no_country <- fit_country_models(pooled)

saveRDS(
  list(
    country_models            = country_models,
    pooled_models              = pooled_models,
    pooled_models_no_country   = pooled_models_no_country
  ),
  file.path(INTERIM_DIR, "models.rds")
)
