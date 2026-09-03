# ==============================================================================
# 07 RESULTS TABLES -- formats fitted models from R/06 into publication tables
# ==============================================================================
# Tables 4 & 5 (adjusted OR for quit attempt/intention) per country, from
# country_models; Tables 1 & 2 pooled (with `country` covariate), from
# pooled_models; a country-stratified summary table built from the SAME
# country_models objects (not refit); and the with/without-country
# sensitivity comparison tables (the LR-test itself lives in
# R/04b_qc_post_model.R, since that's a model-fit diagnostic, not a
# results table).
#
# Standalone-runnable from the project root; requires 01-03 and 06 to have run.

library(dplyr)
library(broom)
library(knitr)
library(kableExtra)
library(here)

source("R/config/paths.R")
source("R/config/variable_maps.R")

models <- read_interim("models.rds")

# ==============================================================================
# TABLES 4 & 5 -- PER COUNTRY (adjusted OR, with p-values)
# ==============================================================================

tidy_or_stars <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      stars = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ""),
      `aOR (95% CI)` = ifelse(
        term == "(Intercept)", "",
        paste0(round(estimate, 2), " (", round(conf.low, 2), "–", round(conf.high, 2), ")", stars)
      ),
      p = ifelse(term == "(Intercept)", "", ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))
    ) %>%
    select(term, `aOR (95% CI)`, p)
}

recode_terms_country <- function(df, exposure_term, exposure_label) {
  df %>%
    mutate(
      Level = dplyr::recode(term,
        "(Intercept)"                  = "Intercept",
        !!exposure_term                := exposure_label,
        "age_group25-44"               = "25–44",
        "age_group45-64"               = "45–64",
        "age_group65+"                 = "65+",
        "genderMale"                   = "Male",
        "educationPrimary or Less"     = "Primary or Less",
        "educationSecondary or above"  = "Secondary or above",
        "residence_labelUrban"         = "Urban",
        "employmentEmployed"           = "Employed",
        "dual_use"                     = "Dual use",
        .default = term
      ),
      Predictor = case_when(
        Level == exposure_label                                ~ "Anti-Tobacco Exposure",
        Level %in% c("25–44", "45–64", "65+")         ~ "Age Group",
        Level == "Male"                                          ~ "Sex",
        Level %in% c("Primary or Less", "Secondary or above")  ~ "Education",
        Level == "Urban"                                         ~ "Residence",
        Level == "Employed"                                      ~ "Employment",
        Level == "Dual use"                                      ~ "Dual Use",
        TRUE ~ NA_character_
      )
    )
}

build_country_or_table <- function(model_attempt, model_intent, exposure_term, exposure_label,
                                    table_num, table_title, footnote_text) {
  attempt_or <- tidy_or_stars(model_attempt) %>%
    rename(`Quit Attempt aOR (95% CI)` = `aOR (95% CI)`, `Quit Attempt p` = p)
  intent_or <- tidy_or_stars(model_intent) %>%
    rename(`Quit Intention aOR (95% CI)` = `aOR (95% CI)`, `Quit Intention p` = p)

  combined <- attempt_or %>%
    full_join(intent_or, by = "term") %>%
    recode_terms_country(exposure_term, exposure_label)

  ref_rows <- tibble::tribble(
    ~Predictor,   ~Level,                     ~`Quit Attempt aOR (95% CI)`, ~`Quit Attempt p`, ~`Quit Intention aOR (95% CI)`, ~`Quit Intention p`,
    "Age Group",  "15–24 (Reference)",   "—", "", "—", "",
    "Sex",        "Female (Reference)",       "—", "", "—", "",
    "Education",  "No Education (Reference)", "—", "", "—", "",
    "Residence",  "Rural (Reference)",        "—", "", "—", "",
    "Employment", "Not Working (Reference)",  "—", "", "—", ""
  )

  final_tab <- bind_rows(
    ref_rows,
    combined %>% filter(term != "(Intercept)") %>%
      select(Predictor, Level, `Quit Attempt aOR (95% CI)`, `Quit Attempt p`,
             `Quit Intention aOR (95% CI)`, `Quit Intention p`)
  ) %>%
    mutate(Predictor = factor(Predictor, levels = c(
      "Anti-Tobacco Exposure", "Age Group", "Sex", "Education", "Residence", "Employment", "Dual Use"
    ))) %>%
    arrange(Predictor) %>%
    mutate(
      Level = if_else(is.na(Level), "", Level),
      across(c(`Quit Attempt aOR (95% CI)`, `Quit Attempt p`, `Quit Intention aOR (95% CI)`, `Quit Intention p`),
             ~ if_else(is.na(.), "", .))
    ) %>%
    group_by(Predictor) %>%
    mutate(Predictor_display = if_else(row_number() == 1, as.character(Predictor), "")) %>%
    ungroup() %>%
    select(Predictor = Predictor_display, Level, `Quit Attempt aOR (95% CI)`, `Quit Attempt p`,
           `Quit Intention aOR (95% CI)`, `Quit Intention p`)

  knitr::kable(final_tab, align = "llcccc", caption = paste0("Table ", table_num, ". ", table_title)) %>%
    kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("condensed"), font_size = 11) %>%
    kableExtra::column_spec(1, bold = TRUE, width = "3cm") %>%
    kableExtra::column_spec(2, width = "4cm") %>%
    kableExtra::column_spec(3, width = "3.5cm") %>%
    kableExtra::column_spec(4, width = "1.8cm") %>%
    kableExtra::column_spec(5, width = "3.5cm") %>%
    kableExtra::column_spec(6, width = "1.8cm") %>%
    kableExtra::footnote(general = footnote_text, footnote_as_chunk = TRUE)
}

country_or_tables <- lapply(country_keys, function(ck) {
  map <- variable_maps[[ck]]
  m   <- models$country_models[[ck]]

  tbl4 <- build_country_or_table(
    m$smk_attempt, m$smk_intent, "anti_cig_expo", "Exposed to Anti-Smoking Media/HWL",
    table_num = 4,
    table_title = sprintf("Adjusted odds ratios for quit attempt and quit intention among smokers, %s GATS %d", map$label, map$year),
    footnote_text = sprintf(
      "* p < 0.05; ** p < 0.01; *** p < 0.001. aOR = adjusted odds ratio from survey-weighted logistic regression. Quit attempt model N = %d; quit intention model N = %d. Reference categories: Age 15–24, Female, No Education, Rural, Not Working.",
      length(m$smk_attempt$y), length(m$smk_intent$y)
    )
  )
  tbl5 <- build_country_or_table(
    m$slt_attempt, m$slt_intent, "anti_slt_expo", "Exposed to Anti-SLT Media/HWL",
    table_num = 5,
    table_title = sprintf("Adjusted odds ratios for quit attempt and quit intention among SLT users, %s GATS %d", map$label, map$year),
    footnote_text = sprintf(
      "* p < 0.05; ** p < 0.01; *** p < 0.001. aOR = adjusted odds ratio from survey-weighted logistic regression. Quit attempt model N = %d; quit intention model N = %d. Reference categories: Age 15–24, Female, No Education, Rural, Not Working.",
      length(m$slt_attempt$y), length(m$slt_intent$y)
    )
  )
  print(tbl4); print(tbl5)
  save_kable(tbl4, here("output", sprintf("table4_smokers_aor_%s.png", ck)))
  save_kable(tbl5, here("output", sprintf("table5_slt_aor_%s.png", ck)))
  list(table4 = tbl4, table5 = tbl5)
}) %>% setNames(country_keys)

# ==============================================================================
# TABLES 1 & 2 -- POOLED (unadjusted-of-country-label OR, no p column, `country` covariate shown)
# ==============================================================================

tidy_or <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(`OR (95% CI)` = ifelse(
      term == "(Intercept)", "",
      paste0(round(estimate, 2), " (", round(conf.low, 2), "–", round(conf.high, 2), ")",
             ifelse(p.value < 0.05, "*", ""))
    )) %>%
    select(term, `OR (95% CI)`)
}

recode_terms_pooled <- function(df, exposure_term, exposure_label) {
  df %>%
    mutate(
      Level = dplyr::recode(term,
        "(Intercept)"                  = "Intercept",
        !!exposure_term                := exposure_label,
        "dual_use"                     = "Dual use",
        "age_group25-44"               = "25–44",
        "age_group45-64"               = "45–64",
        "age_group65+"                 = "65+",
        "genderMale"                   = "Male",
        "educationPrimary or Less"     = "Primary or Less",
        "educationSecondary or above"  = "Secondary or above",
        "residence_labelUrban"         = "Urban",
        "employmentEmployed"           = "Employed",
        "countryIndia"                 = "India",
        "countryBangladesh"            = "Bangladesh",
        "countrySri Lanka"             = "Sri Lanka",
        .default = term
      ),
      Predictor = case_when(
        Level == exposure_label                                ~ "Anti-Tobacco Exposure",
        Level == "Dual use"                                      ~ "Dual Use",
        Level %in% c("25–44", "45–64", "65+")         ~ "Age Group",
        Level == "Male"                                          ~ "Sex",
        Level %in% c("Primary or Less", "Secondary or above")  ~ "Education",
        Level == "Urban"                                         ~ "Residence",
        Level == "Employed"                                      ~ "Employment",
        Level %in% c("India", "Bangladesh", "Sri Lanka")        ~ "Country",
        TRUE ~ NA_character_
      )
    )
}

build_pooled_or_table <- function(model_attempt, model_intent, exposure_term, exposure_label,
                                   table_num, table_title, outcome_span, footnote_text) {
  attempt_or <- tidy_or(model_attempt) %>% rename(`Quit Attempt OR (95% CI)` = `OR (95% CI)`)
  intent_or  <- tidy_or(model_intent)  %>% rename(`Quit Intention OR (95% CI)` = `OR (95% CI)`)

  combined <- attempt_or %>%
    full_join(intent_or, by = "term") %>%
    recode_terms_pooled(exposure_term, exposure_label)

  ref_rows <- tibble::tribble(
    ~Predictor,   ~Level,                      ~`Quit Attempt OR (95% CI)`, ~`Quit Intention OR (95% CI)`,
    "Age Group",  "15–24 (Reference)",    "", "",
    "Sex",        "Female (Reference)",        "", "",
    "Education",  "No Education (Reference)",  "", "",
    "Residence",  "Rural (Reference)",         "", "",
    "Employment", "Not Working (Reference)",   "", "",
    "Country",    "Pakistan (Reference)",      "", ""
  )

  final_tab <- bind_rows(
    ref_rows,
    combined %>% filter(term != "(Intercept)") %>%
      select(Predictor, Level, `Quit Attempt OR (95% CI)`, `Quit Intention OR (95% CI)`)
  ) %>%
    mutate(Predictor = factor(Predictor, levels = c(
      "Anti-Tobacco Exposure", "Dual Use", "Age Group", "Sex", "Education", "Residence", "Employment", "Country"
    ))) %>%
    arrange(Predictor) %>%
    mutate(
      Level = if_else(is.na(Level), "", Level),
      Variable = case_when(
        grepl("\\(Reference\\)", Level) ~ paste0(as.character(Predictor), "  —  ", Level),
        Level == ""                     ~ as.character(Predictor),
        TRUE                            ~ paste0("    ", Level)
      ),
      `Quit Attempt OR (95% CI)`   = if_else(is.na(`Quit Attempt OR (95% CI)`), "", `Quit Attempt OR (95% CI)`),
      `Quit Intention OR (95% CI)` = if_else(is.na(`Quit Intention OR (95% CI)`), "", `Quit Intention OR (95% CI)`)
    ) %>%
    select(Variable, `Quit Attempt OR (95% CI)`, `Quit Intention OR (95% CI)`)

  knitr::kable(final_tab, align = "lcc", na = "", caption = paste0("Table ", table_num, ". ", table_title)) %>%
    kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("condensed"), font_size = 11) %>%
    kableExtra::column_spec(1, width = "9cm") %>%
    kableExtra::column_spec(2, width = "4.5cm") %>%
    kableExtra::column_spec(3, width = "4.5cm") %>%
    kableExtra::add_header_above(c(" " = 1, outcome_span = 2)) %>%
    kableExtra::footnote(general = footnote_text, footnote_as_chunk = TRUE)
}

POOLED_FOOTNOTE <- paste0(
  "* p < 0.05. Survey-weighted logistic regression. Dual use = current use of both smoking and smokeless tobacco. ",
  "Reference categories: Age 15–24, Female, No Education, Rural, Not Working, Pakistan. Pooled analysis ",
  "includes Pakistan, India, Bangladesh, and Sri Lanka."
)

pooled_table1 <- build_pooled_or_table(
  models$pooled_models$smk_attempt, models$pooled_models$smk_intent,
  "anti_cig_expo", "Exposed to Anti-Smoking Media/HWL",
  table_num = 1,
  table_title = "Associations with Quit Attempt and Quit Intention among Smokers — Pooled Analysis (4 Countries)",
  outcome_span = "Smoking Outcomes", footnote_text = POOLED_FOOTNOTE
)
pooled_table2 <- build_pooled_or_table(
  models$pooled_models$slt_attempt, models$pooled_models$slt_intent,
  "anti_slt_expo", "Exposed to Anti-SLT Media/HWL",
  table_num = 2,
  table_title = "Associations with Quit Attempt and Quit Intention among Smokeless Tobacco Users — Pooled Analysis (4 Countries)",
  outcome_span = "Smokeless Tobacco Outcomes", footnote_text = POOLED_FOOTNOTE
)
print(pooled_table1); print(pooled_table2)
save_kable(pooled_table1, here("output", "aor_smokers_pooled.png"))
save_kable(pooled_table2, here("output", "aor_slt_pooled.png"))

# ==============================================================================
# COUNTRY-STRATIFIED SUMMARY TABLE (reuses country_models -- not refit)
# ==============================================================================

extract_expo_or <- function(model, expo_term) {
  if (is.null(model)) return("—")
  tryCatch({
    est <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == expo_term)
    if (nrow(est) == 0) return("—")
    paste0(round(est$estimate, 2), " (", round(est$conf.low, 2), "–", round(est$conf.high, 2), ")",
           ifelse(est$p.value < 0.05, "*", ""))
  }, error = function(e) "—")
}

summary_tab <- purrr::map_dfr(country_keys, function(ck) {
  m <- models$country_models[[ck]]
  tibble::tibble(
    Country = variable_maps[[ck]]$label,
    `Smk Quit Attempt`   = extract_expo_or(m$smk_attempt, "anti_cig_expo"),
    `Smk Quit Intention` = extract_expo_or(m$smk_intent,  "anti_cig_expo"),
    `SLT Quit Attempt`   = extract_expo_or(m$slt_attempt, "anti_slt_expo"),
    `SLT Quit Intention` = extract_expo_or(m$slt_intent,  "anti_slt_expo")
  )
})

country_stratified_gt <- knitr::kable(
  summary_tab, align = "lcccc", na = "—",
  caption = "Adjusted OR (95% CI) for Anti-Tobacco Media Exposure on Quit Outcomes by Country"
) %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("condensed"), font_size = 11) %>%
  kableExtra::add_header_above(c(" " = 1, "Smoking" = 2, "Smokeless Tobacco" = 2)) %>%
  kableExtra::column_spec(1, bold = TRUE, width = "3cm") %>%
  kableExtra::column_spec(2:5, width = "4cm") %>%
  kableExtra::footnote(
    general = "* p < 0.05. Adjusted OR from survey-weighted logistic regression. Models adjusted for age, gender, education, residence, employment, and dual use.",
    footnote_as_chunk = TRUE
  )
print(country_stratified_gt)
save_kable(country_stratified_gt, here("output", "table_country_stratified_summary.png"))

# ==============================================================================
# SENSITIVITY: WITH vs WITHOUT COUNTRY (comparison tables; LR test is in 04b)
# ==============================================================================

compare_expo <- function(model_with, model_without, expo_term) {
  bind_rows(
    tidy(model_with, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == expo_term) %>% mutate(model = "With Country"),
    tidy(model_without, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == expo_term) %>% mutate(model = "Without Country")
  ) %>%
    mutate(
      OR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "–", round(conf.high, 2), ")"),
      p_value = ifelse(p.value < 0.001, "<0.001", ifelse(p.value < 0.05, paste0(round(p.value, 3), "*"), as.character(round(p.value, 3))))
    ) %>%
    select(model, OR_CI, p_value)
}

sensitivity_comparisons <- list(
  list(compare_expo(models$pooled_models$smk_attempt, models$pooled_models_no_country$smk_attempt, "anti_cig_expo"),
       "Anti-Smoking Exposure Effect on Quit Attempts"),
  list(compare_expo(models$pooled_models$smk_intent, models$pooled_models_no_country$smk_intent, "anti_cig_expo"),
       "Anti-Smoking Exposure Effect on Quit Intentions"),
  list(compare_expo(models$pooled_models$slt_attempt, models$pooled_models_no_country$slt_attempt, "anti_slt_expo"),
       "Anti-SLT Exposure Effect on Quit Attempts"),
  list(compare_expo(models$pooled_models$slt_intent, models$pooled_models_no_country$slt_intent, "anti_slt_expo"),
       "Anti-SLT Exposure Effect on Quit Intentions")
)

sensitivity_filenames <- c(
  "sensitivity_smk_attempt.png", "sensitivity_smk_intent.png",
  "sensitivity_slt_attempt.png", "sensitivity_slt_intent.png"
)

for (i in seq_along(sensitivity_comparisons)) {
  tbl <- sensitivity_comparisons[[i]]
  sensitivity_kbl <-
    kable(tbl[[1]], col.names = c("Model", "OR (95% CI)", "p-value"), align = c("l", "c", "c"),
          caption = paste0("Comparison: ", tbl[[2]], " (With vs Without Country)")) %>%
      kable_styling(full_width = FALSE, position = "left") %>%
      footnote(general = "* p < 0.05. Models adjusted for age, gender, education, residence, employment, and dual use.",
               footnote_as_chunk = TRUE)
  print(sensitivity_kbl)
  save_kable(sensitivity_kbl, here("output", sensitivity_filenames[i]))
}

saveRDS(
  list(
    country_or_tables = country_or_tables,
    pooled_table1 = pooled_table1, pooled_table2 = pooled_table2,
    country_stratified = country_stratified_gt,
    sensitivity_comparisons = sensitivity_comparisons
  ),
  file.path(INTERIM_DIR, "results_tables.rds")
)
