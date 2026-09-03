# ==============================================================================
# VARIABLE MAPS -- what differs by country
# ==============================================================================
# One list per country capturing every raw-column-name / recode-rule
# difference between the four GATS instruments. R/02_clean_recode.R applies
# ONE set of shared recoding functions to every country, parameterized by
# the relevant entry here -- this file is the only place a new country's
# column names or codebook quirks need to be added.
#
# Fields:
#   raw_file                    -- CSV filename inside RAW_DATA_DIR
#   label, year                 -- for the "country" tag and table titles
#   media_smoke_cols/_slt_cols  -- anti-tobacco media exposure item columns
#   hwl_smoke_cols/_slt_cols    -- health warning label exposure column(s)
#   extra_combined_cols         -- columns that count toward anti_tob_expo
#                                  ONLY, not toward anti_cig_expo/anti_slt_expo
#                                  (Sri Lanka's bidi HWL item -- see note below)
#   quit_attempt_invalid_codes  -- D01/D09 codes treated as invalid/missing
#                                  (Pakistan's codebook uses 7 AND 9; the
#                                  other three surveys use 9 only)
#   smoker_flag / smokeless_flag -- current-use classification rule:
#       simple_cols: columns OR'd via `%in% c(1, 2)`
#       item_cols:   columns OR'd via `. > 0 & . < 888` (probe items used by
#                    India/Bangladesh/Sri Lanka's more granular product
#                    modules; Pakistan's B01/C01 alone are sufficient)
#   age_col, gender_col, education_col, employment_col, residence_col
#                                -- raw demographic column names
#   employment_employed_codes / _not_working_codes
#                                -- each survey's A05 codebook is a different
#                                   length, so the Employed/Not Working split
#                                   is data, not shared logic
#   cluster_col, strata_col, weight_col
#                                -- raw survey-design column names, renamed to
#                                   canonical gatscluster/gatsstrata/gatsweight
#                                   during cleaning so every downstream step
#                                   (including pooling) can assume one name
#   outcome_cols                -- columns checked for "at least one non-missing"
#                                   when filtering to the analysis sample.
#                                   Pakistan's original analysis included
#                                   D08/D16 (quit intention) alongside D01/D09
#                                   (quit attempt); the other three countries
#                                   never did. Preserved here exactly as-is --
#                                   NOT a bug we're silently fixing during the
#                                   restructure.

variable_maps <- list(

  pakistan = list(
    raw_file  = "PAKISTAN_2014.csv",
    label     = "Pakistan",
    year      = 2014,

    media_smoke_cols = c("G201a1", "G201b1", "G201c1", "G201d1", "G201e1"),
    media_slt_cols   = c("G201a2", "G201b2", "G201c2", "G201d2", "G201e2"),
    hwl_smoke_cols   = "G202",
    hwl_slt_cols     = "G202a",
    extra_combined_cols = character(0),

    quit_attempt_invalid_codes = list(D01 = c(7, 9), D09 = c(7, 9)),

    smoker_flag    = list(simple_cols = "B01", item_cols = character(0)),
    smokeless_flag = list(simple_cols = "C01", item_cols = character(0)),

    age_col = "age", gender_col = "A01", education_col = "A04",
    employment_col = "A05", residence_col = "residence",
    employment_employed_codes     = c(1, 2, 3),
    employment_not_working_codes  = c(4, 5, 6, 7, 8, 9),

    cluster_col = "gatscluster", strata_col = "gatsstrata", weight_col = "gatsweight",

    outcome_cols = c("D01", "D09", "D08", "D16")
  ),

  india = list(
    raw_file  = "INDIA_2016.csv",
    label     = "India",
    year      = 2016,

    media_smoke_cols = c("G01A","G01B","G01C","G01D","G01E","G01F","G01G","G01H","G01I"),
    media_slt_cols   = c("G201A","G201B","G201C","G201D","G201E","G201F","G201G","G201H","G201I"),
    hwl_smoke_cols   = "G02",
    hwl_slt_cols     = "G02A",
    extra_combined_cols = character(0),

    quit_attempt_invalid_codes = list(D01 = 9, D09 = 9),

    smoker_flag = list(
      simple_cols = c("B01", "B03"),
      item_cols   = c("B06A","B06B","B06C","B06E","B06F","B06G",
                       "B10A","B10B","B10C","B10E","B10F","B10G")
    ),
    smokeless_flag = list(
      simple_cols = c("C01", "C03"),
      item_cols   = c("C06A","C06B","C06C","C06D","C06E","C06F","C06G",
                       "C10A","C10B","C10C","C10D","C10E","C10F","C10G")
    ),

    age_col = "AGE", gender_col = "A01", education_col = "A04",
    employment_col = "A05", residence_col = "Residence",
    employment_employed_codes     = c(1, 2, 3, 4),
    employment_not_working_codes  = c(5, 6, 7, 8, 9),

    cluster_col = "gatscluster", strata_col = "GATSSTRATA", weight_col = "gatsweight",

    outcome_cols = c("D01", "D09")
  ),

  bangladesh = list(
    raw_file  = "Bangla_GATS_2017_Public_use_06Spe2018.csv",
    label     = "Bangladesh",
    year      = 2017,

    media_smoke_cols = c("G01AA1","G01AB1","G01B1","G01C1","G01D1","G01DD1","G01E1"),
    media_slt_cols   = c("G01AA3","G01AB3","G01B3","G01C3","G01D3","G01DD3","G01E3"),
    hwl_smoke_cols   = "G02",
    hwl_slt_cols     = "G02A",
    extra_combined_cols = character(0),

    quit_attempt_invalid_codes = list(D01 = 9, D09 = 9),

    smoker_flag = list(
      simple_cols = c("B01", "B03"),
      item_cols   = c("B06A","B06H","B06F","B06B","B06D","B06E","B06G",
                       "B10A","B10H","B10F","B10B","B10D","B10E","B10G")
    ),
    smokeless_flag = list(
      simple_cols = c("C01", "C03"),
      item_cols   = c("C06A","C06B","C06C","C06D","C06E","C06F","C06G",
                       "C10A","C10B","C10C","C10D","C10E","C10F","C10G")
    ),

    age_col = "AGE", gender_col = "A01", education_col = "A04",
    employment_col = "A05", residence_col = "RESIDENCE",
    employment_employed_codes     = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 12),
    employment_not_working_codes  = c(10, 11, 13, 14, 15, 16),

    cluster_col = "gatscluster", strata_col = "gatsstrata", weight_col = "gatsweight",

    outcome_cols = c("D01", "D09")
  ),

  sri_lanka = list(
    raw_file  = "LKA_GATS_2020_Public_Use.csv",
    label     = "Sri Lanka",
    year      = 2019,

    media_smoke_cols = c("G201A1","G201B1","G201C1","G201D1","G201E1","G201F1"),
    media_slt_cols   = c("G201A3","G201B3","G201C3","G201D3","G201E3","G201F3"),
    hwl_smoke_cols   = "G202A",
    hwl_slt_cols     = "G202C",
    # G202B (bidi HWL) counts toward "any anti-tobacco exposure" but was never
    # folded into the cigarette-specific exposure measure in the original
    # analysis -- preserved via extra_combined_cols rather than hwl_smoke_cols.
    extra_combined_cols = "G202B",

    quit_attempt_invalid_codes = list(D01 = 9, D09 = 9),

    smoker_flag = list(
      simple_cols = c("B01", "B03"),
      item_cols   = c("B06A","B06B","B06C","B06D","B10A","B10B","B10C","B10D")
    ),
    smokeless_flag = list(
      simple_cols = c("C01", "C03"),
      item_cols   = c("C06A","C06B","C06C","C06D","C10A","C10B","C10C","C10D")
    ),

    age_col = "AGE", gender_col = "A01", education_col = "A04",
    employment_col = "A05", residence_col = "residence",
    employment_employed_codes     = c(1, 2, 3),
    employment_not_working_codes  = c(4, 5, 6, 7, 8),

    cluster_col = "gatscluster", strata_col = "gatsstrata", weight_col = "gatsweight",

    outcome_cols = c("D01", "D09")
  )
)

# Countries listed in the fixed display/factor order used throughout the
# pooled analysis and every summary table. USE.NAMES = FALSE is deliberate:
# a *named* character vector used as factor()/pivot_wider() levels confuses
# tidyr's column-naming (it can pick up the vector's names instead of its
# values), so country_order must stay a plain unnamed vector.
country_keys  <- c("pakistan", "india", "bangladesh", "sri_lanka")
country_order <- vapply(variable_maps[country_keys], `[[`, character(1), "label", USE.NAMES = FALSE)
