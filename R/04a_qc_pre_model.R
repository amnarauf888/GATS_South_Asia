# ==============================================================================
# 04a QC CHECKS -- PRE-MODEL
# ==============================================================================
# Diagnostics that only need cleaned data and survey design objects (no
# fitted models): user-type classification sanity checks, demographic
# missingness, quit-item missingness among smokers/SLT users, analytic-N
# expectations for each regression model (verified against actual fitted
# models in R/04b), and the CONSORT-style sample-flow numbers/figure.
#
# Writes data/interim/qc_expected_n.rds -- the analytic N each regression
# model in R/06 is expected to have -- for R/04b to cross-check once models
# are fit.
#
# Standalone-runnable from the project root; requires 01-03 to have run.

library(dplyr)
library(tidyr)
library(ggplot2)

source("R/config/paths.R")
source("R/config/variable_maps.R")
source("R/config/recode_functions.R")

SMOKER_TYPES <- c("Smoker only", "Dual: Smoker + Smokeless")
SLT_TYPES    <- c("Smokeless only", "Dual: Smoker + Smokeless")

MODEL_REQUIRED_VARS <- c("age_group", "gender", "education", "residence_label", "employment")

cat("\n==== User-type classification & demographic missingness ====\n")
for (ck in country_keys) {
  label    <- variable_maps[[ck]]$label
  clean_df <- read_interim(paste0("clean_", ck, ".rds"))

  cat("\n--", label, "--\n")
  print(clean_df %>% count(user_type, name = "n", sort = TRUE) %>%
          mutate(pct = round(100 * n / sum(n), 2)))

  n_na_user_type <- sum(is.na(clean_df$user_type))
  cat(sprintf(
    "Rows excluded as waterpipe-only/e-cigarette-only/non-users (NA user_type): %d (%.2f%% of analysis sample)\n",
    n_na_user_type, 100 * n_na_user_type / nrow(clean_df)
  ))

  print(clean_df %>% summarise(
    n = n(),
    n_gender     = n_distinct(gender, na.rm = TRUE),
    n_education  = n_distinct(education, na.rm = TRUE),
    n_residence  = n_distinct(residence_label, na.rm = TRUE),
    n_employment = n_distinct(employment, na.rm = TRUE)
  ))

  cat("Quit-item missingness among smokers:\n")
  print(clean_df %>% filter(user_type %in% SMOKER_TYPES) %>% summarise(
    n_smokers         = n(),
    miss_quit_attempt = sum(is.na(quit_smoking)),
    pct_quit_attempt  = round(100 * mean(is.na(quit_smoking)), 1),
    miss_intent       = sum(is.na(quit_smoking_intent)),
    pct_intent        = round(100 * mean(is.na(quit_smoking_intent)), 1)
  ))

  cat("Quit-item missingness among SLT users:\n")
  print(clean_df %>% filter(user_type %in% SLT_TYPES) %>% summarise(
    n_slt              = n(),
    miss_quit_attempt  = sum(is.na(quit_smokeless)),
    pct_quit_attempt   = round(100 * mean(is.na(quit_smokeless)), 1),
    miss_intent        = sum(is.na(quit_slt_intent)),
    pct_intent         = round(100 * mean(is.na(quit_slt_intent)), 1)
  ))
}

cat("\n==== Expected analytic N per regression model ====\n")
# Recomputes, from each country's design subset, how many rows have every
# required model variable present. R/06 fits the models; R/04b checks the
# fitted models' actual N against these expectations.
expected_n <- lapply(country_keys, function(ck) {
  d      <- read_interim(paste0("design_", ck, ".rds"))
  label  <- variable_maps[[ck]]$label

  smk_attempt_n <- d$smokers$variables %>%
    filter(!is.na(quit_smoking), !is.na(anti_cig_expo), if_all(all_of(MODEL_REQUIRED_VARS), ~ !is.na(.))) %>%
    nrow()
  smk_intent_n <- d$smokers$variables %>%
    filter(!is.na(quit_smoking_intent), !is.na(anti_cig_expo), if_all(all_of(MODEL_REQUIRED_VARS), ~ !is.na(.))) %>%
    nrow()
  slt_attempt_n <- d$slt$variables %>%
    filter(!is.na(quit_smokeless), !is.na(anti_slt_expo), if_all(all_of(MODEL_REQUIRED_VARS), ~ !is.na(.))) %>%
    nrow()
  slt_intent_n <- d$slt$variables %>%
    filter(!is.na(quit_slt_intent), !is.na(anti_slt_expo), if_all(all_of(MODEL_REQUIRED_VARS), ~ !is.na(.))) %>%
    nrow()

  cat(sprintf(
    "%-10s smk_attempt=%d  smk_intent=%d  slt_attempt=%d  slt_intent=%d\n",
    label, smk_attempt_n, smk_intent_n, slt_attempt_n, slt_intent_n
  ))

  tibble(
    country = ck, smk_attempt = smk_attempt_n, smk_intent = smk_intent_n,
    slt_attempt = slt_attempt_n, slt_intent = slt_intent_n
  )
}) %>% bind_rows()

saveRDS(expected_n, file.path(INTERIM_DIR, "qc_expected_n.rds"))

cat("\n==== Sample-flow diagram numbers ====\n")
flow_by_country <- lapply(country_keys, function(ck) {
  raw_df <- read_interim(paste0("raw_", ck, ".rds"))
  compute_flow_counts(raw_df, variable_maps[[ck]])
}) %>% bind_rows()

flow_restricted <- lapply(country_keys, function(ck) {
  clean_df <- read_interim(paste0("clean_", ck, ".rds"))
  tibble(country = variable_maps[[ck]]$label,
         n_restricted = clean_df %>% filter(!is.na(user_type)) %>% nrow())
}) %>% bind_rows()

flow_summary <- flow_by_country %>%
  left_join(flow_restricted, by = "country") %>%
  mutate(
    n_excluded_no_data   = n_tobacco_user - n_analytic,
    n_excluded_othertype = n_analytic - n_restricted
  )

print(flow_summary)

pooled_totals <- flow_summary %>%
  summarise(
    n_total        = sum(n_total),
    n_tobacco_user = sum(n_tobacco_user),
    n_analytic     = sum(n_analytic),
    n_restricted   = sum(n_restricted)
  )
print(pooled_totals)

saveRDS(flow_summary, file.path(INTERIM_DIR, "qc_flow_summary.rds"))

# ---- CONSORT-style flow diagram (ggplot boxes + arrows) -----------------------

box_labels <- c(
  sprintf("Total interviewed across 4 countries\nN = %s", format(pooled_totals$n_total, big.mark = ",")),
  sprintf("Current tobacco users\nN = %s", format(pooled_totals$n_tobacco_user, big.mark = ",")),
  sprintf("Analytic sample\n(non-missing quit/exposure data)\nN = %s", format(pooled_totals$n_analytic, big.mark = ",")),
  sprintf("Final pooled sample\n(Smoker only / Smokeless only / Dual)\nN = %s", format(pooled_totals$n_restricted, big.mark = ","))
)

excl_labels <- c(
  sprintf("Excluded: non-tobacco users\nn = %s",
          format(pooled_totals$n_total - pooled_totals$n_tobacco_user, big.mark = ",")),
  sprintf("Excluded: missing quit attempt/\nintention or exposure data\nn = %s",
          format(pooled_totals$n_tobacco_user - pooled_totals$n_analytic, big.mark = ",")),
  sprintf("Excluded: waterpipe-only,\ne-cigarette-only, non-users\nn = %s",
          format(pooled_totals$n_analytic - pooled_totals$n_restricted, big.mark = ","))
)

y_pos <- c(4, 3, 2, 1)
flow_boxes <- tibble(x = 1, y = y_pos, label = box_labels)
excl_boxes  <- tibble(x = 2.6, y = y_pos[1:3] - 0.5, label = excl_labels)

consort_plot <- ggplot() +
  geom_rect(data = flow_boxes, aes(xmin = x - 0.9, xmax = x + 0.9, ymin = y - 0.35, ymax = y + 0.35),
            fill = "#EAF2FA", color = "#2C5F8A", linewidth = 0.6) +
  geom_text(data = flow_boxes, aes(x = x, y = y, label = label), size = 3.1, lineheight = 0.95) +
  geom_rect(data = excl_boxes, aes(xmin = x - 0.95, xmax = x + 0.95, ymin = y - 0.3, ymax = y + 0.3),
            fill = "#FBEAEA", color = "#B33A3A", linewidth = 0.5) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label), size = 2.6, lineheight = 0.95) +
  geom_segment(data = tibble(y = y_pos[1:3]),
               aes(x = 1, xend = 1, y = y - 0.35, yend = y - 0.65),
               arrow = arrow(length = unit(0.15, "cm")), linewidth = 0.5) +
  geom_segment(data = tibble(y = y_pos[1:3] - 0.5),
               aes(x = 1.9, xend = 2.6, y = y, yend = y), linewidth = 0.4, linetype = "dashed") +
  scale_x_continuous(limits = c(-0.2, 4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.3, 4.6), expand = c(0, 0)) +
  labs(title = "Analytic Sample Flow — Pooled GATS Sample (4 South Asian Countries)") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)))

print(consort_plot)
saveRDS(consort_plot, file.path(INTERIM_DIR, "qc_consort_plot.rds"))
