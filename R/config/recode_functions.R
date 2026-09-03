# ==============================================================================
# RECODE FUNCTIONS -- shared logic, applied identically to every country
# ==============================================================================
# Pure function definitions (no file I/O, no side effects) so this file can
# be sourced both by R/02_clean_recode.R (the pipeline stage that runs them)
# and by R/04a_qc_pre_model.R (which needs derive_tobacco_use_flag()
# to recompute flow-diagram numbers directly from raw data) without either
# script accidentally re-running the other's work.
#
# Every function is parameterized by a country's entry in
# R/config/variable_maps.R -- this file is the ONLY place the recoding rules
# live; only the maps differ by country.

library(dplyr)

# Binary exposure indicator: 1 if any item == "Yes" (1), 0 if all valid items
# were answered but none were "Yes", NA if no valid response exists at all.
make_expo <- function(df, items) {
  any_yes   <- rowSums(across(all_of(items), ~ . == 1), na.rm = TRUE) > 0
  any_valid <- rowSums(across(all_of(items), ~ . %in% c(1, 2)), na.rm = TRUE) > 0
  dplyr::case_when(
    any_yes              ~ 1,
    any_valid & !any_yes ~ 0,
    TRUE                 ~ NA_real_
  )
}

# Current tobacco-use flag: TRUE if any "simple" column indicates current use
# (%in% c(1,2)), OR if any "item" column (a per-product probe used by
# India/Bangladesh/Sri Lanka's more granular modules) has a valid positive
# response (> 0 & < 888). Pakistan's simple B01/C01 check is the special
# case of this with item_cols = character(0).
derive_tobacco_use_flag <- function(df, simple_cols = character(0), item_cols = character(0)) {
  simple_hit <- if (length(simple_cols)) {
    Reduce(`|`, lapply(simple_cols, function(cl) df[[cl]] %in% c(1, 2)))
  } else {
    FALSE
  }
  item_hit <- if (length(item_cols)) {
    rowSums(sapply(item_cols, function(cl) df[[cl]] > 0 & df[[cl]] < 888), na.rm = TRUE) > 0
  } else {
    FALSE
  }
  dplyr::if_else(simple_hit | item_hit, 1, 0, missing = 0)
}

# Analysis-sample filter: keep rows with at least one non-missing outcome
# item (map$outcome_cols) AND at least one non-missing exposure/HWL item.
filter_analysis_sample <- function(df, map) {
  media_cols <- c(map$media_smoke_cols, map$hwl_smoke_cols,
                   map$media_slt_cols,  map$hwl_slt_cols)
  df %>%
    filter(
      !if_all(all_of(map$outcome_cols), is.na),
      !if_all(any_of(media_cols), is.na)
    )
}

derive_quit_outcomes <- function(df, map) {
  df %>%
    mutate(
      quit_smoking = case_when(
        D01 == 1 ~ 1,
        D01 == 2 ~ 0,
      ),
      quit_smokeless = case_when(
        D09 == 1 ~ 1,
        D09 == 2 ~ 0,
      ),
      quit_smoking_intent = case_when(
        D08 %in% c(1, 2) ~ 1,
        D08 %in% c(3, 4) ~ 0,
        D08 %in% c(7, 9) | is.na(D08) ~ NA_real_
      ),
      quit_slt_intent = case_when(
        D16 %in% c(1, 2) ~ 1,
        D16 %in% c(3, 4) ~ 0,
        D16 %in% c(7, 9) | is.na(D16) ~ NA_real_
      )
    )
}

# Restricted 3-category classification (Smoker only / Smokeless only / Dual).
# Waterpipe-only, e-cigarette-only, and non-users are excluded (NA) in every
# country for harmonization -- Pakistan's waterpipe module only captures past
# use and isn't comparable across countries.
derive_user_type <- function(df, map) {
  smoker    <- derive_tobacco_use_flag(df, map$smoker_flag$simple_cols,    map$smoker_flag$item_cols)
  smokeless <- derive_tobacco_use_flag(df, map$smokeless_flag$simple_cols, map$smokeless_flag$item_cols)

  df %>%
    mutate(
      smoker    = smoker,
      smokeless = smokeless,
      user_type = case_when(
        smoker == 1 & smokeless == 1 ~ "Dual: Smoker + Smokeless",
        smoker == 1                  ~ "Smoker only",
        smokeless == 1               ~ "Smokeless only",
        TRUE                         ~ NA_character_
      ),
      user3 = case_when(
        user_type == "Smoker only"              ~ "Smoker",
        user_type == "Smokeless only"           ~ "Smokeless",
        user_type == "Dual: Smoker + Smokeless" ~ "Dual"
      ),
      dual_use = if_else(user_type == "Dual: Smoker + Smokeless", 1L, 0L)
    )
}

derive_exposure <- function(df, map) {
  cig_cols      <- c(map$media_smoke_cols, map$hwl_smoke_cols)
  slt_cols      <- c(map$media_slt_cols,   map$hwl_slt_cols)
  combined_cols <- c(cig_cols, slt_cols, map$extra_combined_cols)

  df %>%
    mutate(
      anti_cig_expo = make_expo(., cig_cols),
      anti_slt_expo = make_expo(., slt_cols),
      anti_tob_expo = make_expo(., combined_cols)
    )
}

derive_demographics <- function(df, map) {
  df %>%
    mutate(
      age_group = case_when(
        .data[[map$age_col]] >= 15 & .data[[map$age_col]] <= 24 ~ "15-24",
        .data[[map$age_col]] >= 25 & .data[[map$age_col]] <= 44 ~ "25-44",
        .data[[map$age_col]] >= 45 & .data[[map$age_col]] <= 64 ~ "45-64",
        .data[[map$age_col]] >= 65                              ~ "65+",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("15-24", "25-44", "45-64", "65+")),

      gender = case_when(
        .data[[map$gender_col]] == 1 ~ "Male",
        .data[[map$gender_col]] == 2 ~ "Female",
        TRUE ~ NA_character_
      ),

      education = case_when(
        .data[[map$education_col]] == 1               ~ "No Education",
        .data[[map$education_col]] %in% c(2, 3, 4)     ~ "Primary or Less",
        .data[[map$education_col]] %in% c(5, 6, 7, 8)  ~ "Secondary or above",
        TRUE ~ NA_character_
      ),

      employment = case_when(
        .data[[map$employment_col]] %in% map$employment_employed_codes    ~ "Employed",
        .data[[map$employment_col]] %in% map$employment_not_working_codes ~ "Not Working",
        TRUE ~ NA_character_
      ),

      residence_label = case_when(
        .data[[map$residence_col]] == 1 ~ "Urban",
        .data[[map$residence_col]] == 2 ~ "Rural",
        TRUE ~ NA_character_
      )
    )
}

# Renames each country's raw cluster/strata/weight columns to the canonical
# names every downstream script (including pooling) assumes.
harmonize_design_cols <- function(df, map) {
  df %>%
    rename(
      gatscluster = !!map$cluster_col,
      gatsstrata  = !!map$strata_col,
      gatsweight  = !!map$weight_col
    )
}

clean_one_country <- function(raw_df, map) {
  raw_df %>%
    filter_analysis_sample(map) %>%
    derive_quit_outcomes(map) %>%
    derive_user_type(map) %>%
    derive_exposure(map) %>%
    derive_demographics(map) %>%
    harmonize_design_cols(map) %>%
    mutate(country = map$label)
}

# Raw-data flow-diagram counts: total interviewed -> current tobacco user ->
# analytic sample (non-missing quit/exposure data). Deliberately checks only
# D01/D09 (not D08/D16) for every country, including Pakistan -- the original
# analysis's flow-diagram diagnostics used this narrower filter even though
# Pakistan's actual analysis-sample filter (map$outcome_cols) also allows
# D08/D16. Preserved exactly; not a discrepancy we're silently resolving here.
compute_flow_counts <- function(raw_df, map) {
  smoker_flow    <- derive_tobacco_use_flag(raw_df, map$smoker_flag$simple_cols,    map$smoker_flag$item_cols)
  smokeless_flow <- derive_tobacco_use_flag(raw_df, map$smokeless_flag$simple_cols, map$smokeless_flag$item_cols)
  any_tobacco_user <- if_else(smoker_flow == 1 | smokeless_flow == 1, 1, 0)

  media_cols <- c(map$media_smoke_cols, map$hwl_smoke_cols,
                   map$media_slt_cols,  map$hwl_slt_cols)

  n_total     <- nrow(raw_df)
  n_any_user  <- sum(any_tobacco_user)
  n_analytic  <- raw_df %>%
    mutate(any_tobacco_user = any_tobacco_user) %>%
    filter(any_tobacco_user == 1) %>%
    filter(!is.na(D01) | !is.na(D09), !if_all(any_of(media_cols), is.na)) %>%
    nrow()

  tibble::tibble(
    country        = map$label,
    n_total        = n_total,
    n_tobacco_user = n_any_user,
    n_analytic     = n_analytic
  )
}
