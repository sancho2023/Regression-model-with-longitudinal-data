# =============================================================================
# Household Wealth and Objective Cognitive Function
# HAALSI (Agincourt, South Africa), Waves 1-3 longitudinal dataset
# =============================================================================
#
# Goal:     Estimate the association between household wealth quintile and
#           objective cognition (Z-score) at baseline (Wave 1, 2014-2015).
#
# Outputs:  Regression tables, forest plots and a descriptive Table 1.
#
# Data:     Download "HAALSI W1-3 Longitudinal Dataset.dta" from the HAALSI
#           Harvard Dataverse. The data are NOT included in this repository
#           and must not be shared (see README / .gitignore).
#
# How to run: open the script in RStudio and run it section by section
#             (Ctrl/Cmd + Enter), from top to bottom.
#
# Sections:
#   1. Setup (paths, packages, helper functions)
#   2. Data cleaning (builds a cleaned long-format .rds file)
#   3. Data overview and analysis datasets
#   4. Regression models (minimal and sensitivity adjustment)
#   5. Forest plot: minimal vs. sensitivity models
#   6. Forest plot: final publication figure
#   7. Table 1: descriptive statistics
# =============================================================================


# =============================================================================
# 1. SETUP ----
# =============================================================================

# --- 1.1 Paths ---------------------------------------------------------------
# Folder containing the original HAALSI file. Edit this line for your machine.
# Default: the folder containing this script (or the working directory).
DATA_DIR <- if (requireNamespace("rstudioapi", quietly = TRUE) &&
                rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  getwd()
}
# DATA_DIR <- "path/to/your/data/folder"   # <- or set it manually

# Exact name of the downloaded file (rename it, or change this line)
DATA_FILE <- "HAALSI W1-3 Longitudinal Dataset.dta"

# Folder where figures and tables are saved
OUT_DIR <- file.path(DATA_DIR, "output")
dir.create(OUT_DIR, showWarnings = FALSE)

dta_path <- file.path(DATA_DIR, DATA_FILE)
rds_path <- file.path(DATA_DIR, "HAALSI_W1-3_long.rds")  # cleaned file

# FALSE = reuse the existing .rds if available; TRUE = rebuild it from the .dta
REBUILD <- FALSE

# --- 1.2 Input checks --------------------------------------------------------
if (!dir.exists(DATA_DIR)) stop("Directory not found: ", DATA_DIR)

if (REBUILD || !file.exists(rds_path)) {
  if (!file.exists(dta_path)) {
    cat("Files available in the directory:\n")
    print(list.files(DATA_DIR))
    stop("The .dta file was not found. Check DATA_DIR and DATA_FILE.")
  }
  message("Found .dta file: ", normalizePath(dta_path))
}

# --- 1.3 Packages ------------------------------------------------------------
pkgs <- c("rstudioapi", "haven", "dplyr", "tidyr", "ggplot2",
          "broom", "knitr", "gtsummary")

missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(broom)
  library(knitr)
  library(gtsummary)
})

# --- 1.4 Helper: print linear-model coefficients as a table ------------------
# `pattern` is a regular expression selecting which terms to display.
show_lm <- function(model, pattern) {
  broom::tidy(model, conf.int = TRUE) |>
    filter(grepl(pattern, term)) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    knitr::kable()
}


# =============================================================================
# 2. DATA CLEANING ----
# =============================================================================
# Reads the HAALSI file, keeps the variables needed, recodes them and saves a
# cleaned file with one row per person per wave. It only runs if the cleaned
# file does not exist yet, or if REBUILD <- TRUE.
#
# Coding rules:
#   * -97 (refused), -98 (don't know) and -99 (empty) are treated as missing.
#   * Yes/no variables (1 = Yes, 2 = No) are recoded to 1/0.
#   * Sex: 1 = Male, 0 = Female.

if (REBUILD || !file.exists(rds_path)) {

  # --- 2.1 Variable dictionary ("?" is replaced by the wave number) ----------
  wave_vars <- c(
    age = "w?c_rage_calc", marital_status = "w?c_bd_mar", hhsize = "w?c_hhsize",
    hhnum = "w?c_hhnum", wealth = "w?c_wealthindex", asset = "w?c_hh_assetindex",
    employment = "w?c_ep_employed",
    memory = "w?cn001", concentration = "w?cn002", learning = "w?cn003",
    cesd = "w?c_cd_cesd", bmi = "w?c_bs_bmi", bmi_category = "w?c_bs_bmicat",
    hypertension_diag = "w?c_cm_hypertension_srdiag", htn_on_med_raw = "w?cm005",
    diabetes_diag = "w?c_cm_diabetes_srdiag", dm_on_med_raw = "w?cm010",
    adls_walk = "w?c_pf_diff_walk", adls_dress = "w?c_pf_diff_dress",
    adls_bath = "w?c_pf_diff_bath", adls_eat = "w?c_pf_diff_eat",
    adls_toilet = "w?c_pf_diff_toilet",
    systolic_bp = "w?c_bs_mean_sys", diastolic_bp = "w?c_bs_mean_dia",
    glucose = "w?c_bs_glucose",
    smoking = "w?cm059", alcohol = "w?cm068",
    status = "w?status", int_year = "w?c_int_year", int_month = "w?c_int_month",
    # Cognition: Waves 2-3 have summary scores; Wave 1 is built from item-level data
    imrec1 = "w?c_cn_imrec1", delrec = "w?c_cn_delrec", orient_sum = "w?cn214"
  )

  # Time-invariant variables and Wave 1 item-level cognition variables
  fixed_vars   <- c("w2c_rsex", "w1c_bd_educ4", "w1c_bd_country")
  w1_cog_items <- c("w1cn013", paste0("w1cn007s", 1:10), paste0("w1cn012s", 1:10),
                    paste0("w1cn01", 4:7))

  wanted <- c("prim_key", fixed_vars, w1_cog_items,
              unlist(lapply(1:3, function(w) sub("?", w, wave_vars, fixed = TRUE))))

  # --- 2.2 Read only the required columns ------------------------------------
  all_names <- names(read_dta(dta_path, n_max = 0))
  raw <- read_dta(dta_path, col_select = all_of(intersect(wanted, all_names)))
  raw <- zap_labels(zap_missing(raw))

  # --- 2.3 Recoding helpers --------------------------------------------------
  neg_to_na <- function(x) { x <- as.numeric(x); x[x < 0] <- NA; x }
  yes_no    <- function(x) {
    x <- as.numeric(x)
    case_when(x == 1 ~ 1, x %in% c(0, 2) ~ 0, TRUE ~ NA_real_)
  }

  # --- 2.4 Wave 1 cognition from item-level data -----------------------------
  # Word recall: one column per word; value > 0 means the word was recalled.
  # Wave 1 had a single immediate-recall trial, so trial 1 is used in all waves.
  w1_recall <- function(stem) {
    m <- as.matrix(raw[, paste0(stem, "s", 1:10)])
    m[is.na(m)] <- 0
    n <- rowSums(m > 0)
    n[rowSums(m == -97) > 0] <- NA   # refused
    n[is.na(raw$w1cn013)]    <- NA   # cognition section not done (e.g., proxy)
    n
  }

  # Orientation: 4 items, correct = 1; "don't know" counted as incorrect
  orient_w1 <- sapply(paste0("w1cn01", 4:7), function(v) {
    x <- as.numeric(raw[[v]])
    case_when(x == 1 ~ 1, x %in% c(2, -98) ~ 0, TRUE ~ NA_real_)
  })

  w1_cog <- tibble(
    prim_key  = raw$prim_key, wave = 1,
    imrec1_w1 = w1_recall("w1cn007"),
    delrec_w1 = w1_recall("w1cn012"),
    orient_w1 = rowSums(orient_w1)
  )

  # --- 2.5 Reshape wide -> long (one row per person per wave) ----------------
  long <- lapply(1:3, function(w) {
    cols <- sub("?", w, wave_vars, fixed = TRUE)
    out  <- tibble(prim_key = raw$prim_key, wave = w)
    for (nm in names(wave_vars)) {
      out[[nm]] <- if (cols[[nm]] %in% names(raw)) as.numeric(raw[[cols[[nm]]]]) else NA_real_
    }
    out
  }) |>
    bind_rows() |>
    left_join(w1_cog, by = c("prim_key", "wave")) |>
    mutate(imrec1     = if_else(wave == 1, imrec1_w1, imrec1),
           delrec     = if_else(wave == 1, delrec_w1, delrec),
           orient_sum = if_else(wave == 1, orient_w1, orient_sum)) |>
    select(-imrec1_w1, -delrec_w1, -orient_w1)

  # Time-invariant characteristics
  fixed <- tibble(
    prim_key = raw$prim_key,
    sex = case_when(raw$w2c_rsex == 1 ~ 1, raw$w2c_rsex == 2 ~ 0),  # 1 = Male, 0 = Female
    education_category = neg_to_na(raw$w1c_bd_educ4),
    born_moz = case_when(raw$w1c_bd_country == 2 ~ 1,               # 1 = born in Mozambique
                         raw$w1c_bd_country == 1 ~ 0)
  )

  # --- 2.6 Recode variables --------------------------------------------------
  long <- long |>
    left_join(fixed, by = "prim_key") |>
    mutate(
      status = if_else(wave == 1, 1, status),
      across(c(age, marital_status, hhsize, hhnum, wealth, employment, memory,
               concentration, learning, cesd, bmi, bmi_category, systolic_bp,
               diastolic_bp, glucose, imrec1, delrec, orient_sum), neg_to_na),
      employment = if_else(wave == 3 & employment == 0, 2, employment),
      across(c(hypertension_diag, diabetes_diag, smoking, alcohol,
               htn_on_med_raw, dm_on_med_raw), yes_no),
      across(starts_with("adls_"),
             ~ case_when(.x %in% c(1, 3) ~ 1, .x == 2 ~ 0, TRUE ~ NA_real_))
    )

  # --- 2.7 Derived variables -------------------------------------------------
  long <- long |>
    mutate(
      htn_group = factor(
        case_when(hypertension_diag == 0 ~ "No hypertension",
                  htn_on_med_raw == 1    ~ "Hypertension, treated",
                  hypertension_diag == 1 ~ "Hypertension, untreated"),
        levels = c("No hypertension", "Hypertension, untreated", "Hypertension, treated")),
      dm_group = factor(
        case_when(diabetes_diag == 0 ~ "No diabetes",
                  dm_on_med_raw == 1 ~ "Diabetes, treated",
                  diabetes_diag == 1 ~ "Diabetes, untreated"),
        levels = c("No diabetes", "Diabetes, untreated", "Diabetes, treated")),
      adl_count = adls_walk + adls_dress + adls_bath + adls_eat + adls_toilet,
      any_adl   = as.numeric(adl_count > 0),
      int_date  = int_year + (int_month - 0.5) / 12
    ) |>
    select(-htn_on_med_raw, -dm_on_med_raw)

  # --- 2.8 Objective cognition score -----------------------------------------
  # Each test is z-scored to Wave 1, the three z-scores are averaged, and the
  # average is re-standardized to Wave 1 (0 = Wave 1 mean, 1 = one Wave 1 SD).
  ref  <- filter(long, wave == 1)
  zref <- function(x, r) (x - mean(r, na.rm = TRUE)) / sd(r, na.rm = TRUE)

  long <- long |>
    mutate(cog_raw = (zref(orient_sum, ref$orient_sum) +
                      zref(imrec1,     ref$imrec1)     +
                      zref(delrec,     ref$delrec)) / 3) |>
    mutate(objective_cognition_z = zref(cog_raw, cog_raw[wave == 1])) |>
    select(-cog_raw)

  # Low cognition = below the Wave 1 20th percentile
  cut20 <- quantile(long$objective_cognition_z[long$wave == 1], 0.20, na.rm = TRUE)
  long  <- long |> mutate(low_cognition = as.numeric(objective_cognition_z < cut20))

  # --- 2.9 Wealth, depression and time variables -----------------------------
  # Asset index in SD units (Wave 1 scale); CES-D z-scored within wave
  long <- long |>
    mutate(asset_sd = zref(asset, asset[wave == 1])) |>
    group_by(wave) |>
    mutate(cesd_z = (cesd - mean(cesd, na.rm = TRUE)) / sd(cesd, na.rm = TRUE)) |>
    ungroup()

  # Years since the baseline (Wave 1) interview
  long <- long |>
    group_by(prim_key) |>
    mutate(years_since_bl = int_date - int_date[wave == 1][1]) |>
    ungroup()

  # --- 2.10 Labelled factors -------------------------------------------------
  long <- long |>
    mutate(
      sex_f          = factor(sex, 0:1, c("Female", "Male")),
      education_f    = factor(education_category, 1:4,
                              c("No formal", "Some primary", "Some secondary", "Secondary+")),
      marital_f      = factor(marital_status, 0:3,
                              c("Never married", "Separated/divorced", "Widowed", "Married")),
      hhsize_f       = factor(hhsize, 0:3, c("Alone", "2 people", "3-6 people", "7+ people")),
      wealth_f       = factor(wealth, 1:5, c("Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)")),
      employment_f   = factor(employment, 1:3, c("Employed", "Not working", "Homemaker")),
      bmi_category_f = factor(bmi_category, 0:3, c("Underweight", "Normal", "Overweight", "Obese"))
    ) |>
    arrange(prim_key, wave)

  saveRDS(long, rds_path)
}

# Load the cleaned long-format data
haalsi <- readRDS(rds_path)
cat(basename(rds_path), ":", nrow(haalsi), "person-waves,",
    n_distinct(haalsi$prim_key), "participants\n")


# =============================================================================
# 3. DATA OVERVIEW AND ANALYSIS DATASETS ----
# =============================================================================
# Notes on the data:
#   * objective_cognition_z is available in all three waves (orientation 0-4,
#     immediate recall 0-10, delayed recall 0-10), scaled to Wave 1.
#   * Cross-sectional questions use Wave 1, the largest sample with full covariates.
#   * wealth_f = household asset-index quintile (Q1 = poorest).

# Sample size and mean cognition by wave
haalsi |>
  group_by(wave) |>
  summarise(completed           = sum(status == 1, na.rm = TRUE),
            with_cognition      = sum(!is.na(objective_cognition_z)),
            mean_years_since_bl = round(mean(years_since_bl[status == 1], na.rm = TRUE), 1),
            mean_cognition      = round(mean(objective_cognition_z, na.rm = TRUE), 2)) |>
  knitr::kable()

# Wave 1 (baseline) analysis dataset
w1 <- filter(haalsi, wave == 1) |>
  mutate(bmi_category_f = relevel(bmi_category_f, ref = "Normal"))

cat("Wave 1 sample size:", nrow(w1), "\n")


# =============================================================================
# 4. REGRESSION MODELS: WEALTH AND OBJECTIVE COGNITION (WAVE 1) ----
# =============================================================================

# --- 4.1 Primary analysis: minimal adjustment --------------------------------
# Confounders: age, sex, education and country of birth.
# Reference category for wealth: Q1 (poorest).
m1a_minimal <- lm(
  objective_cognition_z ~ wealth_f + age + sex_f + education_f + born_moz,
  data = w1
)

show_lm(m1a_minimal, "wealth_f")   # wealth coefficients only
summary(m1a_minimal)               # full model summary

# All coefficients with 95% CIs
broom::tidy(m1a_minimal, conf.int = TRUE) |>
  mutate(across(c(estimate, conf.low, conf.high, p.value), ~ round(.x, 3))) |>
  knitr::kable()

# --- 4.2 Sensitivity analysis: additional confounders ------------------------
# Adds marital status, household size, smoking and alcohol use.
# Employment is NOT included because it may lie on the causal pathway between
# socioeconomic position and cognitive function.
m1a_sensitivity <- lm(
  objective_cognition_z ~ wealth_f + age + sex_f + education_f + born_moz +
    marital_f + hhsize_f + smoking + alcohol,
  data = w1
)

# Compare the wealth coefficients with the minimally adjusted model
show_lm(m1a_sensitivity, "wealth_f")


# =============================================================================
# 5. FOREST PLOT: MINIMAL vs. SENSITIVITY MODELS ----
# =============================================================================

# Extract wealth-quintile coefficients and 95% CIs from a model.
# Q1 is the reference category, so it has no estimate.
extract_wealth <- function(model, model_label) {
  broom::tidy(model, conf.int = TRUE) |>
    filter(grepl("^wealth_f", term)) |>
    mutate(model    = model_label,
           quintile = sub("^wealth_f", "", term))
}

forest_compare <- bind_rows(
  extract_wealth(m1a_minimal,     "Minimal adjustment"),
  extract_wealth(m1a_sensitivity, "Sensitivity analysis")
)

p_compare <- ggplot(forest_compare,
                    aes(x = estimate, y = quintile, color = model, shape = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y", width = 0.15,
                position = position_dodge(width = 0.55)) +
  geom_point(position = position_dodge(width = 0.55), size = 2.8) +
  labs(
    title    = "Household wealth and objective cognitive function",
    subtitle = "Wave 1 baseline, Agincourt, South Africa, 2014-2015",
    x        = "Adjusted mean difference in cognitive Z-score (95% CI)",
    y        = "Household wealth quintile (reference: Q1)",
    color    = "Model",
    shape    = "Model"
  ) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

p_compare

ggsave(file.path(OUT_DIR, "Figure_wealth_cognition_models_comparison.png"),
       plot = p_compare, width = 180, height = 115, units = "mm", dpi = 600, bg = "white")


# =============================================================================
# 6. FOREST PLOT: FINAL PUBLICATION FIGURE (MINIMAL MODEL) ----
# =============================================================================

# Prepare plotting data: estimates, CIs and a text label for each quintile
forest_data <- broom::tidy(m1a_minimal, conf.int = TRUE, conf.level = 0.95) |>
  filter(grepl("^wealth_f", term)) |>
  mutate(
    quintile = recode(term,
                      "wealth_fQ2"           = "Q2",
                      "wealth_fQ3"           = "Q3",
                      "wealth_fQ4"           = "Q4",
                      "wealth_fQ5 (richest)" = "Q5"),
    quintile = factor(quintile, levels = c("Q2", "Q3", "Q4", "Q5")),
    label    = sprintf("%.3f (%.3f-%.3f)", estimate, conf.low, conf.high)
  )

# Build the figure (estimates labelled on the right-hand side)
p_wealth <- ggplot(forest_data, aes(x = estimate, y = quintile)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "grey55") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                orientation = "y", width = 0.16, linewidth = 0.65) +
  geom_point(size = 3, shape = 16) +
  geom_text(aes(x = 0.405, label = label), hjust = 0, size = 3.5) +
  scale_y_discrete(limits = rev(c("Q2", "Q3", "Q4", "Q5"))) +
  scale_x_continuous(breaks = seq(-0.1, 0.4, by = 0.1),
                     expand = expansion(mult = c(0.03, 0.20))) +
  coord_cartesian(xlim = c(-0.1, 0.66), clip = "off") +
  labs(x = "Adjusted mean difference in objective cognition (Z-score)",
       y = "Household wealth quintile") +
  # Suggested caption: "Adjusted for age, sex, education and country of birth.
  # Points = adjusted mean differences; lines = 95% CIs. Reference: Q1."
  theme_classic(base_size = 11) +
  theme(axis.title  = element_text(size = 10),
        axis.text   = element_text(colour = "black", size = 10),
        plot.margin = margin(8, 75, 8, 8))

p_wealth

# Export a publication-quality PNG
ggsave(file.path(OUT_DIR, "Figure_wealth_cognition_forest.png"),
       plot = p_wealth, width = 180, height = 115, units = "mm", dpi = 600, bg = "white")


# =============================================================================
# 7. TABLE 1: DESCRIPTIVE STATISTICS ----
# =============================================================================

# Variables included in the minimally adjusted model
table1_data <- w1 |>
  select(objective_cognition_z, wealth_f, age, sex_f, education_f, born_moz)

# Mean (SD) for continuous variables; n (%) for categorical variables
table1 <- table1_data |>
  tbl_summary(
    type = list(
      objective_cognition_z ~ "continuous",
      age                   ~ "continuous",
      wealth_f              ~ "categorical",
      sex_f                 ~ "categorical",
      education_f           ~ "categorical",
      born_moz              ~ "categorical"
    ),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits  = all_continuous() ~ 2,
    missing = "ifany",
    label = list(
      objective_cognition_z ~ "Objective cognitive function (Z-score)",
      wealth_f              ~ "Household wealth quintile",
      age                   ~ "Age (years)",
      sex_f                 ~ "Sex",
      education_f           ~ "Educational attainment",
      born_moz              ~ "Country of birth (1 = Mozambique)"
    )
  ) |>
  bold_labels()

table1
