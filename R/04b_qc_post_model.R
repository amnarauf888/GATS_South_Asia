# ==============================================================================
# 04b QC CHECKS -- POST-MODEL
# ==============================================================================
# Diagnostics that need fitted model objects: cross-checks each model's
# actual analytic N (length(model$y)) against the N predicted in
# R/04a_qc_pre_model.R from the design object alone, and runs
# likelihood-ratio tests on whether `country` is a jointly significant
# predictor in the pooled models (the sensitivity check referenced by the
# pooled results).
#
# Standalone-runnable from the project root; requires 01-03, 04a, and 06 to
# have run (04a for the expected-N baseline, 06 for the fitted models).

library(survey)

source("R/config/paths.R")
source("R/config/variable_maps.R")

models      <- read_interim("models.rds")
expected_n  <- read_interim("qc_expected_n.rds")

cat("\n==== Model N vs. pre-model expectation ====\n")
mismatches <- 0
for (ck in country_keys) {
  m   <- models$country_models[[ck]]
  exp_row <- expected_n[expected_n$country == ck, ]
  actual <- c(
    smk_attempt = length(m$smk_attempt$y),
    smk_intent  = length(m$smk_intent$y),
    slt_attempt = length(m$slt_attempt$y),
    slt_intent  = length(m$slt_intent$y)
  )
  expected <- c(
    smk_attempt = exp_row$smk_attempt, smk_intent = exp_row$smk_intent,
    slt_attempt = exp_row$slt_attempt, slt_intent = exp_row$slt_intent
  )
  ok <- actual == expected
  if (!all(ok)) mismatches <- mismatches + sum(!ok)
  cat(sprintf("%-10s %s\n", variable_maps[[ck]]$label,
              paste(sprintf("%s: actual=%d expected=%d %s", names(actual), actual, expected,
                             ifelse(ok, "OK", "MISMATCH")), collapse = "  ")))
}
if (mismatches > 0) {
  warning(mismatches, " model(s) have an analytic N that doesn't match the pre-model expectation -- ",
          "check for upstream changes to filtering/derivation logic.")
} else {
  cat("All model N values match pre-model expectations.\n")
}

cat("\n==== Pooled models: likelihood ratio tests for `country` ====\n")
cat("Smoking Quit Attempts:\n")
print(anova(models$pooled_models_no_country$smk_attempt, models$pooled_models$smk_attempt))
cat("Smoking Quit Intentions:\n")
print(anova(models$pooled_models_no_country$smk_intent, models$pooled_models$smk_intent))
cat("SLT Quit Attempts:\n")
print(anova(models$pooled_models_no_country$slt_attempt, models$pooled_models$slt_attempt))
cat("SLT Quit Intentions:\n")
print(anova(models$pooled_models_no_country$slt_intent, models$pooled_models$slt_intent))
