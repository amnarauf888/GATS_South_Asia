# ==============================================================================
# 05 DESCRIPTIVE TABLES
# ==============================================================================
# Table 1 (demographics by tobacco user type) per country + a consolidated
# pooled-by-country version; Tables 2 & 3 (exposure prevalence by
# sociodemographic group) per country; the pooled exposure-prevalence-by-
# country table; and the two prevalence figures.
#
# In the original single R Markdown file, these table-building functions were
# already defined once (in the Pakistan section) and reused by every later
# country via normal R scoping -- they were not actually duplicated per
# country. This script preserves that: each helper is defined once below and
# looped over country_keys, rather than redefined per country.
#
# Standalone-runnable from the project root; requires 01-03 to have run.

library(dplyr)
library(tidyr)
library(survey)
library(gt)
library(ggplot2)

source("R/config/paths.R")
source("R/config/variable_maps.R")

demo_vars  <- c("gender", "age_group", "education", "employment", "residence_label")
var_labels <- c(
  gender = "Gender", age_group = "Age group", education = "Education",
  employment = "Employment", residence_label = "Residence"
)
cats <- c("Smoker", "Smokeless", "Dual")

fmt_pct_se <- function(p, s) ifelse(is.na(p), "", sprintf("%.1f (%.1f)", p, s))

# ---- Table 1 helpers (per country, grouped by user3) --------------------------

get_demo_counts_user3 <- function(df, var) {
  stopifnot(var %in% names(df))
  df %>%
    filter(!is.na(user3)) %>%
    mutate(`__cat__` = as.character(.data[[var]])) %>%
    filter(!is.na(`__cat__`)) %>%
    count(`__cat__`, user3, name = "n") %>%
    complete(`__cat__`, user3 = factor(cats, levels = cats), fill = list(n = 0)) %>%
    pivot_wider(names_from = user3, values_from = n) %>%
    mutate(Category_Total = rowSums(across(all_of(cats)), na.rm = TRUE), Variable = var) %>%
    rename(Category = `__cat__`) %>%
    select(Variable, Category, Category_Total, all_of(cats))
}

get_weighted_colpct_user3 <- function(design, var) {
  x     <- design$variables[[var]]
  x_fac <- factor(as.character(x))
  des_v <- do.call(update, c(list(design), setNames(list(x_fac), var)))
  levs  <- levels(x_fac)
  out   <- data.frame(Category = levs, stringsAsFactors = FALSE)

  for (cat in cats) {
    d_cat <- subset(des_v, user3 == cat)
    if (length(weights(d_cat)) == 0) {
      est_vec <- rep(NA_real_, length(levs)); se_vec <- rep(NA_real_, length(levs))
    } else {
      est <- svymean(as.formula(paste0("~", var)), d_cat, na.rm = TRUE)
      se  <- SE(est)
      est_vec <- as.numeric(est) * 100
      se_vec  <- as.numeric(se) * 100
      if (length(est_vec) != length(levs)) {
        nm <- names(est)
        lvl_from_names <- substring(nm, nchar(var) + 1)
        aligned <- rep(NA_real_, length(levs)); aligned_se <- rep(NA_real_, length(levs))
        for (i in seq_along(lvl_from_names)) {
          j <- match(lvl_from_names[i], levs)
          if (!is.na(j)) { aligned[j] <- est_vec[i]; aligned_se[j] <- se_vec[i] }
        }
        est_vec <- aligned; se_vec <- aligned_se
      }
    }
    out[[paste0(cat, "_pct")]] <- est_vec
    out[[paste0(cat, "_se")]]  <- se_vec
  }
  out$Variable <- var
  tibble::as_tibble(out)
}

# Design-adjusted (Rao-Scott) chi-square: smokers vs SLT users. Dual users
# excluded (sparse cells); one p-value per variable.
get_table1_pval <- function(design, var) {
  d <- subset(design, user3 %in% c("Smoker", "Smokeless") & !is.na(design$variables[[var]]))
  t <- svychisq(as.formula(paste0("~", var, " + user3")), d, statistic = "F")
  tibble::tibble(Variable = var, p = ifelse(t$p.value < 0.001, "<0.001", sprintf("%.3f", t$p.value)))
}

build_table1_gt <- function(clean_df, design, title, subtitle, source_note) {
  tbl1_data <- clean_df %>% mutate(user3 = factor(user3, levels = cats))
  stopifnot(all(na.omit(unique(as.character(tbl1_data$user3))) %in% cats))

  counts   <- bind_rows(lapply(demo_vars, function(v) get_demo_counts_user3(tbl1_data, v)))
  weighted <- bind_rows(lapply(demo_vars, function(v) get_weighted_colpct_user3(design, v)))
  pvals    <- bind_rows(lapply(demo_vars, function(v) get_table1_pval(design, v))) %>%
    mutate(Variable = dplyr::recode(Variable, !!!var_labels))

  joined <- counts %>%
    left_join(weighted, by = c("Variable", "Category")) %>%
    mutate(
      across(ends_with("_pct"), ~ replace_na(., 0)),
      across(ends_with("_se"),  ~ replace_na(., 0)),
      Variable = dplyr::recode(Variable, !!!var_labels)
    ) %>%
    arrange(factor(Variable, levels = unname(var_labels)), Category) %>%
    left_join(pvals, by = "Variable") %>%
    group_by(Variable) %>%
    mutate(p = if_else(row_number() == 1, p, ""), Variable = if_else(row_number() == 1, Variable, "")) %>%
    ungroup()

  disp <- joined %>%
    mutate(
      Smoker_pctSE    = fmt_pct_se(Smoker_pct, Smoker_se),
      Smokeless_pctSE = fmt_pct_se(Smokeless_pct, Smokeless_se),
      Dual_pctSE      = fmt_pct_se(Dual_pct, Dual_se)
    ) %>%
    select(Variable, Category, Category_Total, Smoker, Smoker_pctSE,
           Smokeless, Smokeless_pctSE, Dual, Dual_pctSE, p)

  disp %>%
    gt() %>%
    tab_header(title = title, subtitle = subtitle) %>%
    cols_label(
      Variable = "Variable", Category = "Category", Category_Total = "Row Total",
      Smoker = "n", Smoker_pctSE = "% (SE)", Smokeless = "n", Smokeless_pctSE = "% (SE)",
      Dual = "n", Dual_pctSE = "% (SE)", p = "p-value"
    ) %>%
    tab_spanner(label = "Smoker", id = "sp_Smoker", columns = c(Smoker, Smoker_pctSE)) %>%
    tab_spanner(label = "Smokeless", id = "sp_Smokeless", columns = c(Smokeless, Smokeless_pctSE)) %>%
    tab_spanner(label = "Dual", id = "sp_Dual", columns = c(Dual, Dual_pctSE)) %>%
    fmt_number(columns = c(Category_Total, Smoker, Smokeless, Dual), decimals = 0) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
    tab_source_note(source_note)
}

# ---- Tables 2 & 3 helper (exposure prevalence by demographic group) -----------

exposure_by_demo <- function(design, exposure_var, demo_vars, var_labels, min_n = 10) {
  get_p <- function(v) {
    d <- subset(design, !is.na(design$variables[[v]]) & !is.na(design$variables[[exposure_var]]))
    if (v == "employment") d <- subset(d, employment != "Self-Employed")
    t <- svychisq(as.formula(paste0("~", v, " + ", exposure_var)), d, statistic = "F")
    ifelse(t$p.value < 0.001, "<0.001", sprintf("%.3f", t$p.value))
  }

  purrr::map_dfr(demo_vars, function(v) {
    f_out <- as.formula(paste0("~", exposure_var))
    f_by  <- as.formula(paste0("~", v))
    des_v <- subset(design, !is.na(design$variables[[v]]) & !is.na(design$variables[[exposure_var]]))
    est <- svyby(f_out, f_by, des_v, svyciprop, vartype = "ci", method = "logit", na.rm = TRUE)
    ns  <- des_v$variables %>% count(.data[[v]], name = "n_unwt") %>% rename(Category = 1)
    p_val <- get_p(v)

    tibble::tibble(
      Variable = unname(var_labels[v]),
      Category = as.character(est[[1]]),
      pct      = as.numeric(est[[2]]) * 100,
      lo       = est$ci_l * 100,
      hi       = est$ci_u * 100
    ) %>%
      left_join(ns, by = "Category") %>%
      mutate(
        Estimate = if_else(n_unwt < min_n, "—", sprintf("%.1f%% (%.1f–%.1f)", pct, lo, hi)),
        p = if_else(row_number() == 1, p_val, "")
      ) %>%
      select(Variable, Category, Estimate, n_unwt, p)
  })
}

# ---- Survey-weighted prevalence with 95% CI ------------------------------------

rates <- function(design, var) {
  d <- subset(design, !is.na(design$variables[[var]]))
  e <- svyciprop(as.formula(paste0("~", var)), d, method = "logit")
  data.frame(variable = var, n = nrow(d$variables),
             pct = round(as.numeric(e) * 100, 1),
             lo  = round(confint(e)[1] * 100, 1),
             hi  = round(confint(e)[2] * 100, 1))
}

# ==============================================================================
# PER-COUNTRY: TABLE 1, TABLES 2 & 3, OVERALL RATES
# ==============================================================================

table1_by_country <- list()
table2_by_country <- list()
table3_by_country <- list()

for (ck in country_keys) {
  map      <- variable_maps[[ck]]
  clean_df <- read_interim(paste0("clean_", ck, ".rds"))
  d        <- read_interim(paste0("design_", ck, ".rds"))

  n_user3 <- clean_df %>% filter(!is.na(user3)) %>% nrow()

  table1_by_country[[ck]] <- build_table1_gt(
    clean_df, d$design,
    title = sprintf("Table 1. Demographic characteristics by tobacco user type for %s", map$label),
    subtitle = sprintf(
      "Unweighted counts and survey-weighted column percentages (SE). Analysis limited to non-missing user type (N = %s).",
      format(n_user3, big.mark = ",")
    ),
    source_note = paste0(
      "Note: Percentages are survey-weighted column percentages within each user type. 'Dual' refers to respondents ",
      "who currently use both smoked and smokeless tobacco. p-values from design-adjusted (Rao-Scott) chi-square ",
      "tests comparing smokers with smokeless tobacco users; dual users excluded owing to small cell sizes. ",
      sprintf("Data: GATS %s %d.", map$label, map$year)
    )
  )
  print(table1_by_country[[ck]])

  table2_by_country[[ck]] <- exposure_by_demo(d$smokers, "anti_cig_expo", demo_vars, var_labels)
  table3_by_country[[ck]] <- exposure_by_demo(d$slt,     "anti_slt_expo", demo_vars, var_labels)
  cat(sprintf("%s -- Table 2 N = %d, Table 3 N = %d\n", map$label,
              sum(!is.na(d$smokers$variables$anti_cig_expo)),
              sum(!is.na(d$slt$variables$anti_slt_expo))))
  print(table2_by_country[[ck]])
  print(table3_by_country[[ck]])

  cat(sprintf("%s -- overall exposure + quit rates:\n", map$label))
  print(bind_rows(
    rates(d$smokers, "anti_cig_expo"),
    rates(d$slt,     "anti_slt_expo"),
    rates(d$smokers, "quit_smoking"),
    rates(d$smokers, "quit_smoking_intent"),
    rates(d$slt,     "quit_smokeless"),
    rates(d$slt,     "quit_slt_intent")
  ))
}

saveRDS(list(table1 = table1_by_country, table2 = table2_by_country, table3 = table3_by_country),
        file.path(INTERIM_DIR, "descriptive_tables_by_country.rds"))

# ==============================================================================
# POOLED: CONSOLIDATED TABLE 1 BY COUNTRY (+ user type as its own block)
# ==============================================================================

pooled     <- read_interim("design_pooled.rds")
pooled_data <- read_interim("clean_pooled.rds")

demo_vars_pooled <- c("gender", "age_group", "education", "employment", "residence_label", "user3")
var_labels_pooled <- c(
  gender = "Gender", age_group = "Age Group", education = "Education",
  employment = "Employment", residence_label = "Residence", user3 = "Tobacco User Type"
)

get_demo_counts_country <- function(df, var) {
  stopifnot(var %in% names(df))
  df %>%
    filter(!is.na(country)) %>%
    mutate(`__cat__` = as.character(.data[[var]])) %>%
    filter(!is.na(`__cat__`)) %>%
    count(`__cat__`, country, name = "n") %>%
    complete(`__cat__`, country = factor(country_order, levels = country_order), fill = list(n = 0)) %>%
    pivot_wider(names_from = country, values_from = n) %>%
    mutate(Row_Total = rowSums(across(all_of(country_order)), na.rm = TRUE), Variable = var) %>%
    rename(Category = `__cat__`) %>%
    select(Variable, Category, Row_Total, all_of(country_order))
}

get_weighted_colpct_country <- function(design, var) {
  x     <- design$variables[[var]]
  x_fac <- factor(as.character(x))
  des_v <- do.call(update, c(list(design), setNames(list(x_fac), var)))
  levs  <- levels(x_fac)
  out   <- data.frame(Category = levs, stringsAsFactors = FALSE)

  for (ctry in country_order) {
    d_cat <- subset(des_v, country == ctry)
    if (length(weights(d_cat)) == 0) {
      est_vec <- rep(NA_real_, length(levs)); se_vec <- rep(NA_real_, length(levs))
    } else {
      est <- svymean(as.formula(paste0("~", var)), d_cat, na.rm = TRUE)
      se  <- SE(est)
      est_vec <- as.numeric(est) * 100
      se_vec  <- as.numeric(se) * 100
      if (length(est_vec) != length(levs)) {
        nm <- names(est)
        lvl_from_names <- substring(nm, nchar(var) + 1)
        aligned <- rep(NA_real_, length(levs)); aligned_se <- rep(NA_real_, length(levs))
        for (i in seq_along(lvl_from_names)) {
          j <- match(lvl_from_names[i], levs)
          if (!is.na(j)) { aligned[j] <- est_vec[i]; aligned_se[j] <- se_vec[i] }
        }
        est_vec <- aligned; se_vec <- aligned_se
      }
    }
    out[[paste0(ctry, "_pct")]] <- est_vec
    out[[paste0(ctry, "_se")]]  <- se_vec
  }
  out$Variable <- var
  tibble::as_tibble(out)
}

table1_counts_pooled   <- bind_rows(lapply(demo_vars_pooled, function(v) get_demo_counts_country(pooled_data, v)))
table1_weighted_pooled <- bind_rows(lapply(demo_vars_pooled, function(v) get_weighted_colpct_country(pooled$design, v)))

table1_joined_pooled <- table1_counts_pooled %>%
  left_join(table1_weighted_pooled, by = c("Variable", "Category")) %>%
  mutate(
    across(ends_with("_pct"), ~ replace_na(., 0)),
    across(ends_with("_se"),  ~ replace_na(., 0)),
    Variable = dplyr::recode(Variable, !!!var_labels_pooled)
  ) %>%
  arrange(factor(Variable, levels = unname(var_labels_pooled)), Category) %>%
  group_by(Variable) %>%
  mutate(Variable = if_else(row_number() == 1, Variable, "")) %>%
  ungroup()

n_pooled_total <- pooled_data %>% filter(!is.na(country)) %>% nrow()

table1_disp_pooled <- table1_joined_pooled %>%
  mutate(
    Pakistan_pctSE    = fmt_pct_se(Pakistan_pct, Pakistan_se),
    India_pctSE       = fmt_pct_se(India_pct, India_se),
    Bangladesh_pctSE  = fmt_pct_se(Bangladesh_pct, Bangladesh_se),
    `Sri Lanka_pctSE` = fmt_pct_se(`Sri Lanka_pct`, `Sri Lanka_se`)
  ) %>%
  select(Variable, Category, Row_Total,
         Pakistan, Pakistan_pctSE, India, India_pctSE,
         Bangladesh, Bangladesh_pctSE, `Sri Lanka`, `Sri Lanka_pctSE`)

table1_pooled_gt <- table1_disp_pooled %>%
  gt() %>%
  tab_header(
    title = "Table 1. Demographic Characteristics of the Pooled Analytic Sample, by Country",
    subtitle = paste0("Unweighted counts and survey-weighted column percentages (SE). N = ",
                       format(n_pooled_total, big.mark = ","), ".")
  ) %>%
  cols_label(
    Variable = "Variable", Category = "Category", Row_Total = "Row Total",
    Pakistan = "n", Pakistan_pctSE = "% (SE)", India = "n", India_pctSE = "% (SE)",
    Bangladesh = "n", Bangladesh_pctSE = "% (SE)", `Sri Lanka` = "n", `Sri Lanka_pctSE` = "% (SE)"
  ) %>%
  tab_spanner(label = "Pakistan (2014)", id = "sp_pk", columns = c(Pakistan, Pakistan_pctSE)) %>%
  tab_spanner(label = "India (2016)", id = "sp_in", columns = c(India, India_pctSE)) %>%
  tab_spanner(label = "Bangladesh (2017)", id = "sp_ba", columns = c(Bangladesh, Bangladesh_pctSE)) %>%
  tab_spanner(label = "Sri Lanka (2019)", id = "sp_sl", columns = c(`Sri Lanka`, `Sri Lanka_pctSE`)) %>%
  fmt_number(columns = c(Row_Total, Pakistan, India, Bangladesh, `Sri Lanka`), decimals = 0) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
  tab_source_note(paste0(
    "Note: Percentages are survey-weighted column percentages within each country. Analytic sample restricted to ",
    "Smoker only, Smokeless only, and Dual (Smoker + Smokeless) users. 'Tobacco User Type' reports the weighted ",
    "prevalence of each user category within each country."
  ))
print(table1_pooled_gt)

# ==============================================================================
# POOLED: EXPOSURE PREVALENCE BY COUNTRY (TABLE X)
# ==============================================================================

exposure_prevalence_table <- purrr::map_dfr(country_order, function(ctry) {
  bind_rows(
    rates(subset(pooled$smokers, country == ctry), "anti_cig_expo") %>% mutate(country = ctry, exposure = "Anti-Cigarette"),
    rates(subset(pooled$slt,     country == ctry), "anti_slt_expo") %>% mutate(country = ctry, exposure = "Anti-SLT")
  )
}) %>%
  mutate(cell = sprintf("%.1f%% (%.1f–%.1f)", pct, lo, hi)) %>%
  select(country, exposure, cell) %>%
  pivot_wider(names_from = exposure, values_from = cell)

exposure_prevalence_gt <- exposure_prevalence_table %>%
  gt() %>%
  tab_header(
    title = "Table X. Anti-Tobacco Media Exposure Prevalence by Country",
    subtitle = "Survey-weighted % exposed (95% CI), among current smokers (anti-cigarette) and current SLT users (anti-SLT)"
  ) %>%
  cols_label(country = "Country", `Anti-Cigarette` = "Anti-Cigarette Exposure", `Anti-SLT` = "Anti-SLT Exposure") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
  tab_source_note(paste0(
    "Note: Estimates are survey-weighted percentages with 95% confidence intervals. Anti-cigarette exposure ",
    "assessed among current smokers (smoker-only + dual users); anti-SLT exposure assessed among current SLT ",
    "users (SLT-only + dual users)."
  ))
print(exposure_prevalence_gt)

# ==============================================================================
# FIGURES: PREVALENCE OF TOBACCO USER TYPES
# ==============================================================================

country_year_labels <- setNames(
  sprintf("%s\n(%d)", country_order, vapply(variable_maps[country_keys], `[[`, numeric(1), "year")),
  country_order
)

# ---- Faceted bar chart: Smoker/Smokeless/Dual within each country -------------

user3_combined <- bind_rows(lapply(country_order, function(ctry) {
  des_sub <- subset(pooled$design, country == ctry & !is.na(user3))
  est <- svymean(~factor(user3), des_sub, na.rm = TRUE)
  data.frame(country = ctry, Category = gsub("factor\\(user3\\)", "", names(est)),
             Proportion = as.numeric(est), SE = as.numeric(SE(est)))
})) %>%
  mutate(
    LCL = pmax(0, Proportion - 1.96 * SE), UCL = pmin(1, Proportion + 1.96 * SE),
    Percent = round(100 * Proportion, 1),
    country_lbl = country_year_labels[country],
    Category = factor(Category, levels = c("Smoker", "Smokeless", "Dual"))
  )

prevalence_by_country_plot <- ggplot(user3_combined, aes(x = Category, y = Proportion, fill = Category)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, linewidth = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("Smoker" = "#E87722", "Smokeless" = "#90EE90", "Dual" = "#4472C4")) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~country_lbl, nrow = 1, strip.position = "bottom") +
  labs(title = "Weighted Prevalence of Tobacco User Types — South Asia (GATS)",
       x = NULL, y = "Weighted Prevalence", fill = "User Type",
       caption = "Note: Restricted to Smoker only, Smokeless only, and Dual (Smoker + Smokeless) users.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13), strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 10.5), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), panel.spacing = unit(1, "lines"),
        legend.position = "bottom", plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 8)))

print(prevalence_by_country_plot)

# ---- Stacked bar chart: user type composition, pooled + each country ----------

usertype_by_country <- bind_rows(lapply(country_order, function(ctry) {
  des_sub <- subset(pooled$design, country == ctry & !is.na(user3))
  est <- svymean(~factor(user3), des_sub, na.rm = TRUE)
  tibble(country = ctry, user_type = gsub("factor\\(user3\\)", "", names(est)), pct = round(as.numeric(est) * 100, 1))
}))

usertype_pooled <- {
  des_all <- subset(pooled$design, !is.na(user3))
  est <- svymean(~factor(user3), des_all, na.rm = TRUE)
  tibble(country = "Pooled Sample", user_type = gsub("factor\\(user3\\)", "", names(est)), pct = round(as.numeric(est) * 100, 1))
}

usertype_summary <- bind_rows(usertype_pooled, usertype_by_country) %>%
  mutate(
    country = factor(country, levels = c("Pooled Sample", "India", "Bangladesh", "Sri Lanka", "Pakistan")),
    user_type = factor(user_type, levels = c("Smokeless", "Smoker", "Dual"))
  )

usertype_composition_plot <- ggplot(usertype_summary, aes(x = country, y = pct, fill = user_type)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Smokeless" = "#E8A2A0", "Smoker" = "#C44E4E", "Dual" = "#6E1F1F")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Prevalence of Tobacco User Types", x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "bottom", axis.text.x = element_text(size = 11))

print(usertype_composition_plot)

saveRDS(
  list(table1_pooled = table1_pooled_gt, exposure_prevalence = exposure_prevalence_gt,
       prevalence_by_country_plot = prevalence_by_country_plot,
       usertype_composition_plot = usertype_composition_plot),
  file.path(INTERIM_DIR, "descriptive_tables_pooled.rds")
)
