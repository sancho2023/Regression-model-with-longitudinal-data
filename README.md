
# Household Wealth and Cognitive Function (HAALSI)

R analysis of the association between **household wealth quintile** and **objective cognitive function**
(Z-score of orientation, immediate and delayed word recall) at baseline (Wave 1, 2014-2015) in the
HAALSI cohort (Agincourt, South Africa).

## Contents
- `haalsi_wealth_cognition.R`: full pipeline (setup, data cleaning, models, figures, Table 1)

## Analysis
1. **Minimal model:** wealth quintile adjusted for age, sex, education, country of birth
2. **Sensitivity model:** adds marital status, household size, smoking, alcohol
3. **Forest plots** of the wealth contrasts (reference: Q1, poorest)
4. **Table 1** with descriptive statistics

## Data (not included)
Download `HAALSI W1-3 Longitudinal Dataset.dta` from the HAALSI Harvard Dataverse and place it next to the
script (or set `DATA_DIR` at the top of the script). The data are restricted: **do not commit or share
the `.dta` / `.rds` files** (they are listed in `.gitignore`).

## Run
Open the script in RStudio and run it section by section. Required packages are installed automatically:
`haven, dplyr, tidyr, ggplot2, broom, knitr, gtsummary`. Figures are written to `output/`.
