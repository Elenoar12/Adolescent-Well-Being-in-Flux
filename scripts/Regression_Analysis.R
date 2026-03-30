### Regression & Sensitivity Analysis
# Requires hbsc_variables.csv and hbsc_labels.xlsx to be present in the working directory.
# Run Data_Processing.R first to generate hbsc_variables.csv.

library(tidyverse)
library(nnet)
library(lm.beta)
library(broom)
library(readr)
library(readxl)
library(glue)
library(tibble)

# Load data
hbsc_def <- read.csv("data/hbsc_variables.csv", header = TRUE)
hbsc_def <- tibble::rowid_to_column(hbsc_def, "ID")

# Removing Turkey (substance use not surveyed) and Kazakhstan (only C1 solution)
hbsc_def <- hbsc_def %>%
  filter(!countryname %in% c("Turkey", "Kazakhstan"))

# Load country labels and class solutions
hbsc_labels <- read_excel("data/hbsc_labels.xlsx")

# Define outcome and SES variables
outcome <- c("health", "lifesat", "feeling", "ache")
ses     <- c("age", "sex", "fas")

# ============================================================
# FUNCTIONS
# ============================================================

# Cross-sectional regressions (most frequent profile as reference)
run_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  df$profile <- as.factor(profiles)
  most_frequent_profile <- names(sort(table(df$profile), decreasing = TRUE))[1]
  df$profile <- relevel(df$profile, ref = most_frequent_profile)

  cat_vars <- c("sex")
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }

  # 1. Multinomial: Profile ~ SES
  multinom_formula <- as.formula(glue("profile ~ {paste(ses_vars, collapse = ' + ')}"))
  multinom_model   <- multinom(multinom_formula, data = df, trace = FALSE)
  multinom_summary <- tidy(multinom_model) %>%
    mutate(
      odds_ratio = exp(estimate),
      conf.low   = exp(estimate - 1.96 * std.error),
      conf.high  = exp(estimate + 1.96 * std.error)
    )
  write_csv(multinom_summary, file.path(save_dir, glue("{prefix}_multinom_profile~age+sex+ses.csv")))

  # 2. Linear regressions per outcome
  for (out in outcome_vars) {
    if (!(out %in% names(df))) next

    # (a) Outcome ~ Profile
    model_profile      <- lm(as.formula(glue("{out} ~ profile")), data = df)
    model_profile_beta <- lm.beta(model_profile)
    profile_summary    <- tidy(model_profile) %>%
      mutate(
        std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_profile_beta)[term]),
        std_conf.low  = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(profile_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile.csv")))

    # (b) Outcome ~ Profile + SES
    model_main      <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')}")), data = df)
    model_main_beta <- lm.beta(model_main)
    main_summary    <- tidy(model_main) %>%
      mutate(
        std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_main_beta)[term]),
        std_conf.low  = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(main_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses.csv")))

    # (c) Outcome ~ Profile + SES + Profile*Sex
    model_combined      <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')} + profile*sex")), data = df)
    model_combined_beta <- lm.beta(model_combined)
    combined_summary    <- tidy(model_combined) %>%
      mutate(
        std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_combined_beta)[term]),
        std_conf.low  = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(combined_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses+profilexsex.csv")))

    # (d) Males only
    df_males <- df[df$sex == 1, ]
    if (nrow(df_males) > 0) {
      ses_vars_no_sex <- setdiff(ses_vars, "sex")
      formula_males   <- if (length(ses_vars_no_sex) > 0)
        as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}"))
      else
        as.formula(glue("{out} ~ profile"))
      model_males      <- lm(formula_males, data = df_males)
      model_males_beta <- lm.beta(model_males)
      males_summary    <- tidy(model_males) %>%
        mutate(
          std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_males_beta)[term]),
          std_conf.low  = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(males_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+ses_MALES.csv")))
    }

    # (e) Females only
    df_females <- df[df$sex == 2, ]
    if (nrow(df_females) > 0) {
      ses_vars_no_sex  <- setdiff(ses_vars, "sex")
      formula_females  <- if (length(ses_vars_no_sex) > 0)
        as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}"))
      else
        as.formula(glue("{out} ~ profile"))
      model_females      <- lm(formula_females, data = df_females)
      model_females_beta <- lm.beta(model_females)
      females_summary    <- tidy(model_females) %>%
        mutate(
          std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_females_beta)[term]),
          std_conf.low  = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(females_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+ses_FEMALES.csv")))
    }
  }
}

# Longitudinal regression (profile × survey year interaction)
run_longitudinal_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  df$profile <- as.factor(profiles)
  most_frequent_profile <- names(sort(table(df$profile), decreasing = TRUE))[1]
  df$profile <- relevel(df$profile, ref = most_frequent_profile)

  earliest_year  <- sort(unique(df$surveyyear))[1]
  df$surveyyear  <- as.factor(df$surveyyear)
  df$surveyyear  <- relevel(df$surveyyear, ref = as.character(earliest_year))

  cat_vars <- c("sex")
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }

  for (out in outcome_vars) {
    if (!(out %in% names(df))) next
    model_formula       <- as.formula(glue("{out} ~ profile*surveyyear + {paste(ses_vars, collapse = ' + ')}"))
    model_longitudinal  <- lm(model_formula, data = df)
    model_long_beta     <- lm.beta(model_longitudinal)
    longitudinal_summary <- tidy(model_longitudinal) %>%
      mutate(
        std_estimate  = ifelse(term == "(Intercept)", estimate, coef(model_long_beta)[term]),
        std_conf.low  = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(longitudinal_summary, file.path(save_dir, glue("{prefix}_longitudinal_{out}_profile_x_surveyyear.csv")))
  }
}

# Regressions with each profile cycled as reference category
run_refcat_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  df$profile   <- as.factor(profiles)
  all_profiles <- levels(df$profile)

  cat_vars <- c("sex")
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }

  for (ref_profile in all_profiles) {
    ref_dir <- file.path(save_dir, paste0("ref_cat_", ref_profile))
    if (!dir.exists(ref_dir)) dir.create(ref_dir, recursive = TRUE)
    df$profile <- relevel(df$profile, ref = ref_profile)

    # Multinomial
    multinom_formula <- as.formula(glue("profile ~ {paste(ses_vars, collapse = ' + ')}"))
    multinom_model   <- multinom(multinom_formula, data = df, trace = FALSE)
    multinom_summary <- tidy(multinom_model) %>%
      mutate(
        odds_ratio = exp(estimate),
        conf.low   = exp(estimate - 1.96 * std.error),
        conf.high  = exp(estimate + 1.96 * std.error)
      )
    write_csv(multinom_summary, file.path(ref_dir, glue("{prefix}_multinom_profile~age+sex+ses_ref{ref_profile}.csv")))

    for (out in outcome_vars) {
      if (!(out %in% names(df))) next

      # (a) Outcome ~ Profile
      m <- lm(as.formula(glue("{out} ~ profile")), data = df)
      mb <- lm.beta(m)
      tidy(m) %>%
        mutate(std_estimate = ifelse(term == "(Intercept)", estimate, coef(mb)[term]),
               std_conf.low = std_estimate - 1.96 * std.error,
               std_conf.high = std_estimate + 1.96 * std.error) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high) %>%
        write_csv(file.path(ref_dir, glue("{prefix}_reg_{out}_profile_ref{ref_profile}.csv")))

      # (b) Outcome ~ Profile + SES
      m <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')}")), data = df)
      mb <- lm.beta(m)
      tidy(m) %>%
        mutate(std_estimate = ifelse(term == "(Intercept)", estimate, coef(mb)[term]),
               std_conf.low = std_estimate - 1.96 * std.error,
               std_conf.high = std_estimate + 1.96 * std.error) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high) %>%
        write_csv(file.path(ref_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses_ref{ref_profile}.csv")))

      # (c) Outcome ~ Profile + SES + Profile*Sex
      m <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')} + profile*sex")), data = df)
      mb <- lm.beta(m)
      tidy(m) %>%
        mutate(std_estimate = ifelse(term == "(Intercept)", estimate, coef(mb)[term]),
               std_conf.low = std_estimate - 1.96 * std.error,
               std_conf.high = std_estimate + 1.96 * std.error) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high) %>%
        write_csv(file.path(ref_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses+profilexsex_ref{ref_profile}.csv")))

      # (d) Males only
      df_males <- df[df$sex == 1, ]
      if (nrow(df_males) > 0) {
        ses_no_sex <- setdiff(ses_vars, "sex")
        formula_m  <- if (length(ses_no_sex) > 0)
          as.formula(glue("{out} ~ profile + {paste(ses_no_sex, collapse = ' + ')}"))
        else as.formula(glue("{out} ~ profile"))
        m <- lm(formula_m, data = df_males); mb <- lm.beta(m)
        tidy(m) %>%
          mutate(std_estimate = ifelse(term == "(Intercept)", estimate, coef(mb)[term]),
                 std_conf.low = std_estimate - 1.96 * std.error,
                 std_conf.high = std_estimate + 1.96 * std.error) %>%
          select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high) %>%
          write_csv(file.path(ref_dir, glue("{prefix}_reg_{out}_profile+ses_MALES_ref{ref_profile}.csv")))
      }

      # (e) Females only
      df_females <- df[df$sex == 2, ]
      if (nrow(df_females) > 0) {
        ses_no_sex <- setdiff(ses_vars, "sex")
        formula_f  <- if (length(ses_no_sex) > 0)
          as.formula(glue("{out} ~ profile + {paste(ses_no_sex, collapse = ' + ')}"))
        else as.formula(glue("{out} ~ profile"))
        m <- lm(formula_f, data = df_females); mb <- lm.beta(m)
        tidy(m) %>%
          mutate(std_estimate = ifelse(term == "(Intercept)", estimate, coef(mb)[term]),
                 std_conf.low = std_estimate - 1.96 * std.error,
                 std_conf.high = std_estimate + 1.96 * std.error) %>%
          select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high) %>%
          write_csv(file.path(ref_dir, glue("{prefix}_reg_{out}_profile+ses_FEMALES_ref{ref_profile}.csv")))
      }
    }
  }
}

# ============================================================
# MAIN REGRESSION (all countries, year-specific profiles)
# ============================================================

country_names  <- unique(hbsc_def$countryname)
regression_dir <- "data/Regression"
if (!dir.exists(regression_dir)) dir.create(regression_dir)

for (country in country_names) {
  cat("\nProcessing:", country, "\n")
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) dir.create(country_regression_dir)

  class_solution <- hbsc_labels %>% filter(Country == country) %>% pull(ClassSolution)

  # Country-level (all years pooled)
  cprob_path <- file.path("data/LPA", country, glue("c_prob_{country}_{class_solution}.csv"))
  if (file.exists(cprob_path)) {
    tryCatch({
      profiles <- read.csv(cprob_path, header = TRUE)
      if (nrow(profiles) > 0) {
        profiles   <- profiles$C
        df_country <- hbsc_def[hbsc_def$countryname == country, ] %>%
          filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
        available_outcomes <- outcome[sapply(outcome, function(o) !all(is.na(df_country[[o]])))]
        if (length(available_outcomes) > 0) {
          run_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
          run_longitudinal_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
        }
      }
    }, error = function(e) message(glue("ERROR - {country} (all years): {e$message}")))
  }

  # Year-level
  for (year in unique(hbsc_def$surveyyear[hbsc_def$countryname == country])) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    if (!dir.exists(year_regression_dir)) dir.create(year_regression_dir)

    cprob_path <- file.path("data/LPA", country, glue("{year}"), glue("c_prob_{country}_{year}_{class_solution}.csv"))
    if (file.exists(cprob_path)) {
      tryCatch({
        profiles <- read.csv(cprob_path, header = TRUE)
        if (nrow(profiles) > 0) {
          profiles <- profiles$C
          df_year  <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ] %>%
            filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
          available_outcomes <- outcome[sapply(outcome, function(o) !all(is.na(df_year[[o]])))]
          if (length(available_outcomes) > 0)
            run_regressions(df_year, profiles, ses, available_outcomes, year_regression_dir, glue("{country}_{year}"))
        }
      }, error = function(e) message(glue("ERROR - {country} {year}: {e$message}")))
    }
  }
}

# ============================================================
# SENSITIVITY ANALYSIS (country-level profiles applied per year)
# ============================================================

country_names  <- "Switzerland"   # change to unique(hbsc_def$countryname) for all countries
regression_dir <- "data/Sensitivity Analysis"
if (!dir.exists(regression_dir)) dir.create(regression_dir)

for (country in country_names) {
  cat("\nSensitivity —", country, "\n")
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) dir.create(country_regression_dir)

  class_solution <- hbsc_labels %>% filter(Country == country) %>% pull(ClassSolution)
  cprob_path     <- file.path("data/LPA", country, glue("c_prob_{country}_{class_solution}.csv"))

  for (year in unique(hbsc_def$surveyyear[hbsc_def$countryname == country])) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    if (!dir.exists(year_regression_dir)) dir.create(year_regression_dir)

    if (file.exists(cprob_path)) {
      tryCatch({
        profiles_full <- read.csv(cprob_path, header = TRUE)
        df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ] %>%
          filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
        profiles <- profiles_full$C[profiles_full$ID %in% df_year$ID]
        available_outcomes <- outcome[sapply(outcome, function(o) !all(is.na(df_year[[o]])))]
        if (length(available_outcomes) > 0)
          run_regressions(df_year, profiles, ses, available_outcomes, year_regression_dir, glue("{country}_{year}"))
      }, error = function(e) message(glue("ERROR - {country} {year}: {e$message}")))
    }
  }
}

# ============================================================
# REFERENCE CATEGORY REGRESSIONS (all profiles as reference)
# ============================================================

country_names  <- unique(hbsc_def$countryname)
regression_dir <- "data/Regression (ref_profile)"
if (!dir.exists(regression_dir)) dir.create(regression_dir)

for (country in country_names) {
  cat("\nRef-cat regressions —", country, "\n")
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) dir.create(country_regression_dir)

  class_solution <- hbsc_labels %>% filter(Country == country) %>% pull(ClassSolution)

  # Country-level
  cprob_path <- file.path("data/LPA", country, glue("c_prob_{country}_{class_solution}.csv"))
  if (file.exists(cprob_path)) {
    profiles   <- read.csv(cprob_path, header = TRUE)$C
    df_country <- hbsc_def[hbsc_def$countryname == country, ] %>%
      filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
    run_refcat_regressions(df_country, profiles, ses, outcome, country_regression_dir, glue("{country}_all"))
  }

  # Year-level
  for (year in unique(hbsc_def$surveyyear[hbsc_def$countryname == country])) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    if (!dir.exists(year_regression_dir)) dir.create(year_regression_dir)

    cprob_path <- file.path("data/LPA", country, glue("{year}"), glue("c_prob_{country}_{year}_{class_solution}.csv"))
    if (file.exists(cprob_path)) {
      profiles <- read.csv(cprob_path, header = TRUE)$C
      df_year  <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ] %>%
        filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
      run_refcat_regressions(df_year, profiles, ses, outcome, year_regression_dir, glue("{country}_{year}"))
    }
  }
}

# ============================================================
# SENSITIVITY ANALYSIS — REFERENCE CATEGORY (Switzerland only)
# ============================================================

country_names  <- "Switzerland"   # change to unique(hbsc_def$countryname) for all countries
regression_dir <- "data/Sensitivity Analysis (ref_profile)"
if (!dir.exists(regression_dir)) dir.create(regression_dir)

for (country in country_names) {
  cat("\nSensitivity ref-cat —", country, "\n")
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) dir.create(country_regression_dir)

  class_solution <- hbsc_labels %>% filter(Country == country) %>% pull(ClassSolution)
  cprob_path     <- file.path("data/LPA", country, glue("c_prob_{country}_{class_solution}.csv"))

  for (year in unique(hbsc_def$surveyyear[hbsc_def$countryname == country])) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    if (!dir.exists(year_regression_dir)) dir.create(year_regression_dir)

    if (file.exists(cprob_path)) {
      tryCatch({
        profiles_full <- read.csv(cprob_path, header = TRUE)
        df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ] %>%
          filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
        profiles <- profiles_full$C[profiles_full$ID %in% df_year$ID]
        run_refcat_regressions(df_year, profiles, ses, outcome, year_regression_dir, glue("{country}_{year}"))
      }, error = function(e) message(glue("ERROR - {country} {year}: {e$message}")))
    }
  }
}
