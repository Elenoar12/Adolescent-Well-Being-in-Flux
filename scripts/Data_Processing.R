##### Data merge HBSC

library(tidyverse)
library(lavaan)
library(haven)
library(psych)
library(skimr)
library(corrplot)
library(glue)
library(readxl)

# setwd(r"(U:\Datenanalyse\001_HBSC Daten)")
setwd(file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse"))

### read in data

hbsc01 <- read_sav("HBSC2001OAed1.0_F4.sav")
hbsc06 <- read_sav("HBSC2006OAed1.0_F1.sav")
hbsc10 <- read_sav("HBSC2010OAed1.0_F4.sav")
hbsc14 <- read_sav("HBSC2014OAed1.1_F1.sav")
hbsc18 <- read_sav("HBSC2018OAed1.1.sav")

### renames
hbsc10 <- hbsc10 %>%
  rename(mbmi = MBMI ) 

hbsc14 <- hbsc14 %>%
  rename(
    countryno = COUNTRYno,   
    uniqueid = UniqueID,      
    age = AGE,                
    agecat = AGECAT,          
    mbmi = MBMI,
    surveyyear=HBSC,
    monthcollect=month,
    yearcollect=year,
    sleepdifficulty=sleepdificulty,
    menarche=m136C,
    sampleweights=M137)

hbsc18 <- hbsc18 %>%
  rename(mbmi = MBMI,
         surveyyear=HBSC,
         monthcollect=month,
         yearcollect=year,
         sleepdifficulty=sleepdificulty,
         sampleweights=weight)  

### merge
overlapping_columns0106 <- intersect(names(hbsc01), names(hbsc06))
hbsc0106 <- full_join(hbsc01, hbsc06, by=overlapping_columns0106)

overlapping_columns010610 <- intersect(names(hbsc0106), names(hbsc10))
hbsc010610 <- full_join(hbsc0106, hbsc10, by=overlapping_columns010610)

overlapping_columns01061014 <- intersect(names(hbsc010610), names(hbsc14))
hbsc01061014 <- full_join(hbsc010610, hbsc14, by=overlapping_columns01061014)

overlapping_columns0106101418 <- intersect(names(hbsc01061014), names(hbsc18))
hbsc_all <- full_join(hbsc01061014, hbsc18, by=overlapping_columns0106101418)

names(hbsc_all)

hbsc_allrel <- hbsc_all %>%
  select(surveyyear,countryno,age,
         agecat,
         backache,
         bodyheight,
         bodyweight,
         countryborn,
         countrybornfa,
         countrybornmo,
         dizzy,
         headache,
         irritable,
         nervous,
         sex,
         stomachache,
         feellow,
         health,
         physact60,
         sleepdifficulty,
         toothbr,
         studaccept,
         studhelpful,
         studtogether,
         talkfather,
         talkmother,
         thinkbody,
         breakfastwe,
         lifesat,
         breakfastwd,
         talkstepfa,
         talkstepmo,
         hadsex,
         contraceptcondom,
         contraceptpill,
         agesex,
         mbmi,
         fruits,
         softdrinks,
         sweets,
         vegetables,
         welloff,
         ondiet,
         smoking,
         beer,
         drunk,
         spirits,
         wine,
         tvwd,
         compusewd,
         tvwe,
         compusewe,
         agealco,
         agecigarette,
         agedrunk,
         contraceptother,
         grade,
         alc30d_2,
         smok30d_2,
         fruits_2,
         softdrinks_2,
         sweets_2,
         vegetables_2,
         drunkltm,
         famcar,
         bedroom,
         computers,
         holidays,
         fasfamcar,
         fasbedroom,
         fascomputers,
         fasholidays)

### Rename countries
country_names <- c(
  "8000" = "Albania",
  "31000" = "Azerbaijan",
  "40000" = "Austria",
  "51000" = "Armenia",
  "56001" = "Belgium Flemish",
  "56002" = "Belgium French",
  "100000" = "Bulgaria",
  "124000" = "Canada",
  "191000" = "Croatia",
  "203000" = "Czechia",
  "208000" = "Denmark",
  "233000" = "Estonia",
  "246000" = "Finland",
  "250000" = "France",
  "268000" = "Georgia",
  "276000" = "Germany",
  "300000" = "Greece",
  "304000" = "Greenland",
  "348000" = "Hungary",
  "352000" = "Iceland",
  "372000" = "Ireland",
  "376000" = "Israel",
  "380000" = "Italy",
  "398000" = "Kazakhstan",
  "428000" = "Latvia",
  "440000" = "Lithuania",
  "442000" = "Luxembourg",
  "470000" = "Malta",
  "498000" = "Moldova",
  "528000" = "Netherlands",
  "578000" = "Norway",
  "616000" = "Poland",
  "620000" = "Portugal",
  "642000" = "Romania",
  "643000" = "Russia",
  "688000" = "Serbia",
  "703000" = "Slovakia",
  "705000" = "Slovenia",
  "724000" = "Spain",
  "752000" = "Sweden",
  "756000" = "Switzerland",
  "792000" = "Turkey",
  "804000" = "Ukraine",
  "807000" = "North Macedonia",
  "826001" = "England",
  "826002" = "Scotland",
  "826003" = "Wales",
  "826004" = "Northern Ireland",
  "840000" = "United States of America"
)

# Create a new column 'countryname' based on 'countryno'
hbsc_allrel <- hbsc_allrel %>%
  mutate(countryname = case_when(
    as.character(countryno) %in% names(country_names) ~ country_names[as.character(countryno)],
    TRUE ~ as.character(haven::as_factor(countryno))
  ))

##### Kodierung

### Health complaints variables
# Reverse code variables 'backache', 'headache', 'stomachache' (default code was 1 = every day, 5 = never)
hbsc_allrel$backache_rev <- ifelse(!is.na(hbsc_allrel$backache), 6 - hbsc_allrel$backache, NA)
hbsc_allrel$headache_rev <- ifelse(!is.na(hbsc_allrel$headache), 6 - hbsc_allrel$headache, NA)
hbsc_allrel$stomachache_rev <- ifelse(!is.na(hbsc_allrel$stomachache), 6 - hbsc_allrel$stomachache, NA)
# Ache variables
hbsc_allrel$ache <- rowMeans(hbsc_allrel[, c("backache_rev", "headache_rev", "stomachache_rev")], na.rm = TRUE)

# Reverse code variables 'dizzy', 'irritable', 'nervous', 'feellow' (default code was 1 = every day, 5 = never)
hbsc_allrel$dizzy_rev <- ifelse(!is.na(hbsc_allrel$dizzy), 6 - hbsc_allrel$dizzy, NA)
hbsc_allrel$irritable_rev <- ifelse(!is.na(hbsc_allrel$irritable), 6 - hbsc_allrel$irritable, NA)
hbsc_allrel$nervous_rev <- ifelse(!is.na(hbsc_allrel$nervous), 6 - hbsc_allrel$nervous, NA)
hbsc_allrel$feellow_rev <- ifelse(!is.na(hbsc_allrel$feellow), 6 - hbsc_allrel$feellow, NA)

# Feeling variables (explain why dizzy is aggregated into this variable)
hbsc_allrel$feeling <- rowMeans(hbsc_allrel[, c("dizzy_rev", "irritable_rev", "nervous_rev", "feellow_rev")], na.rm = TRUE)

# Reverse code 'health' variable (1 = Excellent, 4 = Poor)
hbsc_allrel$health <- ifelse(!is.na(hbsc_allrel$health), 5 - hbsc_allrel$health, NA)

# Sleep Problems
hbsc_allrel$sleepprob <- 5-hbsc_allrel$sleepdifficulty

### Physical Inactivity
hbsc_allrel$physinact <- 7-hbsc_allrel$physact60

### Risky sex variable
hbsc_allrel$riskysex <- ifelse(
  # First condition: didn't have sex or used any contraception
  hbsc_allrel$hadsex == 2 | (hbsc_allrel$contraceptcondom == 1 | hbsc_allrel$contraceptpill == 1 | hbsc_allrel$contraceptother == 1), 
  0,
  # Second condition: had sex and did not use any contraception (including "don't know" treated as "no")
  ifelse(
    hbsc_allrel$hadsex == 1 & (hbsc_allrel$contraceptcondom == 2 | hbsc_allrel$contraceptcondom == 3) &
      (hbsc_allrel$contraceptpill == 2 | hbsc_allrel$contraceptpill == 3) &
      (is.na(hbsc_allrel$contraceptother) | hbsc_allrel$contraceptother == 2 | hbsc_allrel$contraceptother == 3), 
    1, 
    NA))

### Unhealthy Diet variable
hbsc_allrel$fruits3 <- ifelse(is.na(hbsc_allrel$fruits), hbsc_allrel$fruits_2, hbsc_allrel$fruits)
hbsc_allrel$sweets3 <- ifelse(is.na(hbsc_allrel$sweets), hbsc_allrel$sweets_2, hbsc_allrel$sweets)
hbsc_allrel$vegetables3 <- ifelse(is.na(hbsc_allrel$vegetables), hbsc_allrel$vegetables_2, hbsc_allrel$vegetables)
hbsc_allrel$softdrinks3 <- ifelse(is.na(hbsc_allrel$softdrinks), hbsc_allrel$softdrinks_2, hbsc_allrel$softdrinks)

hbsc_allrel$fruits3r <- 7-hbsc_allrel$fruits3
hbsc_allrel$vegetables3r <- 7-hbsc_allrel$vegetables3

hbsc_allrel$undiet <- rowMeans(hbsc_allrel[, c("sweets3", "softdrinks3", "fruits3r", "vegetables3r")], na.rm = TRUE) 

### Alcohol variable
hbsc_allrel$drunk2 <- ifelse(is.na(hbsc_allrel$drunk), hbsc_allrel$drunkltm, hbsc_allrel$drunk)

# Reverse code variables 'beer', 'wine', 'spirits' (default code was 1 = every day, 5 = never)
hbsc_allrel$beer_rev <- ifelse(!is.na(hbsc_allrel$beer), 6 - hbsc_allrel$beer, NA)
hbsc_allrel$wine_rev <- ifelse(!is.na(hbsc_allrel$wine), 6 - hbsc_allrel$wine, NA)
hbsc_allrel$spirits_rev <- ifelse(!is.na(hbsc_allrel$spirits), 6 - hbsc_allrel$spirits, NA)

# Create a general variable alc from variables 'beer', 'wine', 'spirits' by choosing the maximum as default value
hbsc_allrel$alc <- pmax(hbsc_allrel$beer_rev, hbsc_allrel$wine_rev, hbsc_allrel$spirits_rev, na.rm = TRUE)

# Staunch variable alc30d_2 (1-7) for comparability with variable alc (1-5) == Rescaling (min-max normalization)
hbsc_allrel <- hbsc_allrel %>%
  mutate(alc30d_2_rs = ifelse(is.na(alc30d_2), NA, 1 + ((alc30d_2 - 1) / 6) * 4))

# Pull together variables alc and alc30d_2rs for general variable alc2
hbsc_allrel$alc2 <- ifelse(is.na(hbsc_allrel$alc), hbsc_allrel$alc30d_2_rs, hbsc_allrel$alc)

### Smoking variable
# Reverse code variable "smoking" (default code was 1 = every day, 4 = don't)
hbsc_allrel$smoking_rev <- 5-hbsc_allrel$smoking

# Staunch variable smok30d_2 (1-7) for comparability with variable smoking_rev (1-4) == Rescaling (min-max normalization)
hbsc_allrel <- hbsc_allrel %>%
  mutate(smok30d_2_rs = ifelse(is.na(smok30d_2), NA, 1 + ((smok30d_2 - 1) / 6) * 3))

hbsc_allrel$smoking2 <- as.double(ifelse(is.na(hbsc_allrel$smoking_rev), hbsc_allrel$smok30d_2_rs, hbsc_allrel$smoking_rev))

### Family affluence variables as SES proxy
hbsc_allrel <- hbsc_allrel %>%
  mutate(
    famcar3 = ifelse(is.na(famcar), fasfamcar, famcar),
    bedroom3 = ifelse(is.na(bedroom), fasbedroom, bedroom),
    computers3 = ifelse(is.na(computers), fascomputers, computers),
    holidays3 = ifelse(is.na(holidays), fasholidays, holidays)
  )

# Aggregate into fas variable, keep NA's intact (0 != NA)
hbsc_allrel$fas <- ifelse(
  rowSums(!is.na(hbsc_allrel[, c("famcar3", "bedroom3", "computers3", "holidays3")])) == 0,
  NA,
  rowSums(hbsc_allrel[, c("famcar3", "bedroom3", "computers3", "holidays3")], na.rm = TRUE)
)

### Correlations
# hbsc_allrel$toothbr <- as.numeric(hbsc_allrel$toothbr)
# 
# cor_matrix <- cor(hbsc_allrel[, c("physinact", "sleepprob", "riskysex","undiet","smoking2","alc2")], use = "pairwise.complete.obs")
# 
# corrplot(cor_matrix, method = "circle", addCoef.col = "black")
# 
# str(hbsc_allrel[, c("physinact", "sleepprob", "toothbr", "riskysex","undiet","smoking2","alc2")])
# 
# print(hbsc_allrel$riskysex)

### Exclusion of Russia from data (see exclusion from HBSC project)
hbsc_allrel <-  hbsc_allrel %>% filter(countryname != "Russia")

### Streamlined raw data for labelling and response plots

hbsc_raw <- hbsc_allrel %>%
  select(surveyyear,countryname,
         backache_rev, headache_rev, stomachache_rev,
         dizzy_rev, irritable_rev, nervous_rev, feellow_rev,
         health, lifesat,
         famcar3, bedroom3, computers3, holidays3,
         sleepprob,
         physinact,
         fruits3r, vegetables3r, sweets3, softdrinks3,
         beer_rev, wine_rev, spirits_rev, alc30d_2_rs,
         smoking_rev, smok30d_2_rs
         )

write.csv(hbsc_raw, file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_raw.csv"))

##### Definitive data file with only relevant HBs and aggregated variables

hbsc_def <- hbsc_allrel %>%
  select(surveyyear,countryno,countryname,age,
         agecat,
         bodyheight,
         bodyweight,
         countryborn,
         countrybornfa,
         countrybornmo,
         sex,
         health,
         toothbr,
         studaccept,
         studhelpful,
         studtogether,
         talkfather,
         talkmother,
         thinkbody,
         talkstepfa,
         talkstepmo,
         agesex,
         mbmi,
         welloff,
         grade,
         physinact, sleepprob, riskysex, undiet, smoking2, alc2, lifesat, feeling, ache, fas)

hbsc_def <- hbsc_def %>%
  rename(smoking = smoking2,
         alcohol = alc2)

hbsc_allrelCH <- subset(hbsc_def, countryno == 756000)

write_csv(hbsc_def, file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_allrel.csv"))
write_csv(hbsc_def, "U:/Datenanalyse/001_HBSC Daten/Processed_Data/hbsc_allrel.csv")

write_csv(hbsc_allrelCH, "U:/Datenanalyse/001_HBSC Daten/Processed_Data/hbsc_allrelCH.csv")
write_csv(hbsc_allrelCH, file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_allrelCH.csv"))

### Streamlined data frame for webapp

hbsc_def <- hbsc_allrel %>%
  select(surveyyear,
         countryno,
         countryname,
         age,
         agecat,
         sex,
         health,
         physinact,
         sleepprob,
         undiet,
         smoking = smoking2,
         alcohol = alc2,
         lifesat,
         feeling,
         ache,
         fas)

write_csv(hbsc_def, file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_variables.csv"))

### Regressions

library(nnet)
library(lm.beta)
library(broom)
library(readr)

# Define the run_regressions() function
run_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  # Set profile as factor and relevel it to the most frequent profile
  df$profile <- as.factor(profiles)
  most_frequent_profile <- names(sort(table(df$profile), decreasing = TRUE))[1]
  df$profile <- relevel(df$profile, ref = most_frequent_profile)
  
  # Z-standardize numeric SES and outcome variables (except categorical like sex)
  # Define categorical variables explicitly (e.g., sex is numerically coded)
  cat_vars <- c("sex")
  
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }
  
  # 1. Multinomial regression: Profile ~ SES
  multinom_formula <- as.formula(glue("profile ~ {paste(ses_vars, collapse = ' + ')}"))
  multinom_model <- multinom(multinom_formula, data = df, trace = FALSE)
  multinom_summary <- tidy(multinom_model) %>%
    mutate(
      odds_ratio = exp(estimate),
      conf.low = exp(estimate - 1.96 * std.error),
      conf.high = exp(estimate + 1.96 * std.error)
    )
  write_csv(multinom_summary, file.path(save_dir, glue("{prefix}_multinom_profile~age+sex+ses.csv")))
  
  # 2. Linear regressions: For each outcome
  for (out in outcome_vars) {
    if (!(out %in% names(df))) next
    
    # (a) Outcome ~ Profile
    model_profile <- lm(as.formula(glue("{out} ~ profile")), data = df)
    model_profile_beta <- lm.beta(model_profile)
    reg_summary_profile <- tidy(model_profile)
    std_coefs_profile <- coef(model_profile_beta)
    profile_summary <- reg_summary_profile %>%
      mutate(
        std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_profile[term]),
        std_conf.low = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(profile_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile.csv")))
    
    # (b) Outcome ~ Profile + SES_var (without interaction term)
    model_main <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')}")), data = df)
    model_main_beta <- lm.beta(model_main)
    reg_summary_main <- tidy(model_main)
    std_coefs_main <- coef(model_main_beta)
    main_summary <- reg_summary_main %>%
      mutate(
        std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_main[term]),
        std_conf.low = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(main_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses.csv")))
    
    # (c) Outcome ~ Profile + SES_var + Profile*Sex (with interaction term)
    model_combined <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')} + profile*sex")), data = df)
    model_combined_beta <- lm.beta(model_combined)
    reg_summary_combined <- tidy(model_combined)
    std_coefs_combined <- coef(model_combined_beta)
    combined_summary <- reg_summary_combined %>%
      mutate(
        std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_combined[term]),
        std_conf.low = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    write_csv(combined_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses+profilexsex.csv")))
    
    # (d) Stratified analyses by sex: Outcome ~ Profile + SES_var (males only)
    df_males <- df[df$sex == 1, ]  
    if (nrow(df_males) > 0) {
      # Remove sex from ses_vars for stratified analysis since we're subsetting
      ses_vars_no_sex <- setdiff(ses_vars, "sex")
      if (length(ses_vars_no_sex) > 0) {
        model_males <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}")), data = df_males)
      } else {
        model_males <- lm(as.formula(glue("{out} ~ profile")), data = df_males)
      }
      model_males_beta <- lm.beta(model_males)
      reg_summary_males <- tidy(model_males)
      std_coefs_males <- coef(model_males_beta)
      males_summary <- reg_summary_males %>%
        mutate(
          std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_males[term]),
          std_conf.low = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(males_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+ses_MALES.csv")))
    }
    
    # (e) Stratified analyses by sex: Outcome ~ Profile + SES_var (females only)
    df_females <- df[df$sex == 2, ]  
    if (nrow(df_females) > 0) {
      # Remove sex from ses_vars for stratified analysis since we're subsetting
      ses_vars_no_sex <- setdiff(ses_vars, "sex")
      if (length(ses_vars_no_sex) > 0) {
        model_females <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}")), data = df_females)
      } else {
        model_females <- lm(as.formula(glue("{out} ~ profile")), data = df_females)
      }
      model_females_beta <- lm.beta(model_females)
      reg_summary_females <- tidy(model_females)
      std_coefs_females <- coef(model_females_beta)
      females_summary <- reg_summary_females %>%
        mutate(
          std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_females[term]),
          std_conf.low = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(females_summary, file.path(save_dir, glue("{prefix}_reg_{out}_profile+ses_FEMALES.csv")))
    }
    
  }
}

# Function for longitudinal regression with profile*surveyyear interaction
run_longitudinal_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  # Set profile as factor and relevel it to the most frequent profile
  df$profile <- as.factor(profiles)
  most_frequent_profile <- names(sort(table(df$profile), decreasing = TRUE))[1]
  df$profile <- relevel(df$profile, ref = most_frequent_profile)
  
  # Set survey year as factor and relevel to earliest year
  earliest_year <- sort(unique(df$surveyyear))[1]
  df$surveyyear <- as.factor(df$surveyyear)
  df$surveyyear <- relevel(df$surveyyear, ref = as.character(earliest_year))
  
  # Z-standardize numeric SES and outcome variables (except categorical like sex)
  cat_vars <- c("sex")
  
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }
  
  # Linear regressions for each outcome
  for (out in outcome_vars) {
    if (!(out %in% names(df))) next
    
    # Longitudinal model: Outcome ~ Profile*SurveyYear + SES variables
    model_formula <- as.formula(glue("{out} ~ profile*surveyyear + {paste(ses_vars, collapse = ' + ')}"))
    
    model_longitudinal <- lm(model_formula, data = df)
    model_longitudinal_beta <- lm.beta(model_longitudinal)
    reg_summary_longitudinal <- tidy(model_longitudinal)
    std_coefs_longitudinal <- coef(model_longitudinal_beta)
    
    longitudinal_summary <- reg_summary_longitudinal %>%
      mutate(
        std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_longitudinal[term]),
        std_conf.low = std_estimate - 1.96 * std.error,
        std_conf.high = std_estimate + 1.96 * std.error
      ) %>%
      select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
    
    write_csv(longitudinal_summary, file.path(save_dir, glue("{prefix}_longitudinal_{out}_profile_x_surveyyear.csv")))
  }
}

# Set working directory to LPA Analysis folder

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uncomment if you want to use source file location as working directory
setwd(file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse"))

data_path = "hbsc_variables.csv"
hbsc_def <- read.csv(data_path, header=TRUE)

# Removing Turkey, because substance use was not surveyed / Kazakhstan because only C1 solution
hbsc_def <- hbsc_def %>% 
  filter(!countryname %in% c("Turkey", "Kazakhstan"))

# Loading Labels and ClassSolution
hbsc_labels <- read_excel("hbsc_labels.xlsx")

# Define variables
outcome <- c("health", "lifesat", "feeling", "ache")
ses <- c("age", "sex", "fas")

# country_names <- "Switzerland"  # or vector of countries
country_names <- unique(hbsc_def$countryname)
# country_names <- setdiff(unique(hbsc_def$countryname), "Switzerland")

# Create main Regression directory if it doesn't exist
regression_dir <- "Regression"
if (!dir.exists(regression_dir)) {
  dir.create(regression_dir)
}

# Main loop over countries
for (country in country_names) {
  # Create country directory within the Regression folder
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) {
    dir.create(country_regression_dir)
  }
  
  class_solution <- hbsc_labels %>% 
    filter(Country == country) %>% 
    pull(ClassSolution)
  
  # --- COUNTRY-LEVEL (All years together) ---
  cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
  
  if (file.exists(cprob_path)) {
    # Try to read the file and check if it's empty
    tryCatch({
      profiles <- read.csv(cprob_path, header = TRUE)
      
      if (nrow(profiles) == 0) {
        message(glue("Skipping {country} (all years): cprob file is empty"))
      } else {
        profiles <- profiles$C
        df_country <- hbsc_def[hbsc_def$countryname == country, ]
        # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
        df_country <- df_country %>% 
          filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
        
        # Check which outcomes are available and filter them
        available_outcomes <- outcome[sapply(outcome, function(out) !all(is.na(df_country[[out]])))]
        
        # Log skipped outcomes
        skipped_outcomes <- setdiff(outcome, available_outcomes)
        for (out in skipped_outcomes) {
          message(glue("Skipping {country} (all years), outcome '{out}': all values are NA"))
        }
        
        # Only run regressions if there are available outcomes
        if (length(available_outcomes) > 0) {
          run_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
          run_longitudinal_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
        } else {
          message(glue("Skipping {country} (all years): no available outcomes"))
        }
      }
    }, error = function(e) {
      message(glue("ERROR - {country} (all years): {e$message}"))
    })
  }
  
  data_country <- hbsc_def[hbsc_def$countryname == country, ]
  survey_years <- unique(data_country$surveyyear)
  
  # --- YEAR-LEVEL (separate regressions per year) ---
  for (year in survey_years) {
    year_dir <- file.path("LPA ID", country, glue("{year}"))
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    
    if (!dir.exists(year_regression_dir)) {
      dir.create(year_regression_dir)
    }
    
    cprob_path <- file.path(year_dir, glue("c_prob_{country}_{year}_{class_solution}.csv"))
    
    if (file.exists(cprob_path)) {
      # Try to read the file and check if it's empty
      tryCatch({
        profiles <- read.csv(cprob_path, header = TRUE)
        
        if (nrow(profiles) == 0) {
          message(glue("Skipping {country} {year}: cprob file is empty"))
        } else {
          profiles <- profiles$C
          df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ]
          # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
          df_year <- df_year %>% 
            filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
          
          # Check which outcomes are available and filter them
          available_outcomes <- outcome[sapply(outcome, function(out) !all(is.na(df_year[[out]])))]
          
          # Log skipped outcomes
          skipped_outcomes <- setdiff(outcome, available_outcomes)
          for (out in skipped_outcomes) {
            message(glue("Skipping {country} {year}, outcome '{out}': all values are NA"))
          }
          
          # Only run regressions if there are available outcomes
          if (length(available_outcomes) > 0) {
            run_regressions(df_year, profiles, ses, available_outcomes, year_regression_dir, glue("{country}_{year}"))
          } else {
            message(glue("Skipping {country} {year}: no available outcomes"))
          }
        }
      }, error = function(e) {
        message(glue("ERROR - {country} {year}: {e$message}"))
      })
    }
  }
}

"INSERTION: Sensitivity analysis"

# Set working directory to LPA Analysis folder

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # uncomment if you want to use source file location as working directory
setwd(file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse"))

data_path = "hbsc_variables.csv"
hbsc_def <- read.csv(data_path, header=TRUE)
hbsc_def <- tibble::rowid_to_column(hbsc_def, "ID")

# Removing Turkey, because substance use was not surveyed / Kazakhstan because only C1 solution
hbsc_def <- hbsc_def %>% 
  filter(!countryname %in% c("Turkey", "Kazakhstan"))

# Loading Labels and ClassSolution
hbsc_labels <- read_excel("hbsc_labels.xlsx")

# Define variables
outcome <- c("health", "lifesat", "feeling", "ache")
ses <- c("age", "sex", "fas")

country_names <- "Switzerland"  # or vector of countries
# country_names <- unique(hbsc_def$countryname)
# country_names <- setdiff(unique(hbsc_def$countryname), "Switzerland")

# Create main Regression directory if it doesn't exist
regression_dir <- "Sensitivity Analysis"
if (!dir.exists(regression_dir)) {
  dir.create(regression_dir)
}

# Main loop over countries
for (country in country_names) {
  # Create country directory within the Regression folder
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) {
    dir.create(country_regression_dir)
  }
  
  class_solution <- hbsc_labels %>% 
    filter(Country == country) %>% 
    pull(ClassSolution)
  
  # --- COUNTRY-LEVEL (All years together) ---
  cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
  
  if (file.exists(cprob_path)) {
    # Try to read the file and check if it's empty
    tryCatch({
      profiles <- read.csv(cprob_path, header = TRUE)
      
      if (nrow(profiles) == 0) {
        message(glue("Skipping {country} (all years): cprob file is empty"))
      } else {
        profiles <- profiles$C
        df_country <- hbsc_def[hbsc_def$countryname == country, ]
        # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
        df_country <- df_country %>% 
          filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
        
        # Check which outcomes are available and filter them
        available_outcomes <- outcome[sapply(outcome, function(out) !all(is.na(df_country[[out]])))]
        
        # Log skipped outcomes
        skipped_outcomes <- setdiff(outcome, available_outcomes)
        for (out in skipped_outcomes) {
          message(glue("Skipping {country} (all years), outcome '{out}': all values are NA"))
        }
        
        # Only run regressions if there are available outcomes
        if (length(available_outcomes) > 0) {
          # run_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
          # run_longitudinal_regressions(df_country, profiles, ses, available_outcomes, country_regression_dir, glue("{country}_all"))
        } else {
          message(glue("Skipping {country} (all years): no available outcomes"))
        }
      }
    }, error = function(e) {
      message(glue("ERROR - {country} (all years): {e$message}"))
    })
  }
  
  data_country <- hbsc_def[hbsc_def$countryname == country, ]
  survey_years <- unique(data_country$surveyyear)
  
  # --- YEAR-LEVEL (separate regressions per year) ---
  for (year in survey_years) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    
    if (!dir.exists(year_regression_dir)) {
      dir.create(year_regression_dir)
    }
    
    # Use country-level cprob file
    cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
    
    if (file.exists(cprob_path)) {
      tryCatch({
        profiles_full <- read.csv(cprob_path, header = TRUE)
        
        if (nrow(profiles_full) == 0) {
          message(glue("Skipping {country} {year}: cprob file is empty"))
        } else {
          # Get year-specific data
          df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ]
          
          # Filter out cases where ALL LPA variables are NA
          df_year <- df_year %>% 
            filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
          
          # Match profiles to year-specific data using ID
          profiles <- profiles_full$C[profiles_full$ID %in% df_year$ID]
          
          # Check which outcomes are available and filter them
          available_outcomes <- outcome[sapply(outcome, function(out) !all(is.na(df_year[[out]])))]
          
          # Log skipped outcomes
          skipped_outcomes <- setdiff(outcome, available_outcomes)
          for (out in skipped_outcomes) {
            message(glue("Skipping {country} {year}, outcome '{out}': all values are NA"))
          }
          
          # Only run regressions if there are available outcomes
          if (length(available_outcomes) > 0) {
            run_regressions(df_year, profiles, ses, available_outcomes, year_regression_dir, glue("{country}_{year}"))
          } else {
            message(glue("Skipping {country} {year}: no available outcomes"))
          }
        }
      }, error = function(e) {
        message(glue("ERROR - {country} {year}: {e$message}"))
      })
    }
  }
}

"INSERTION END: Sensitivity Analysis"

## Additional: Running regressions where each Profile is the reference category
# Define the run_refcat_regressions() function
run_refcat_regressions <- function(df, profiles, ses_vars, outcome_vars, save_dir, prefix) {
  # Set profile as factor
  df$profile <- as.factor(profiles)
  
  # Get all unique profiles
  all_profiles <- levels(df$profile)
  
  # Z-standardize numeric SES and outcome variables (except categorical like sex)
  # Define categorical variables explicitly (e.g., sex is numerically coded)
  cat_vars <- c("sex")
  
  vars_to_standardize <- setdiff(unique(c(ses_vars, outcome_vars)), cat_vars)
  for (var in vars_to_standardize) {
    if (var %in% names(df) && is.numeric(df[[var]])) {
      df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)[, 1]
    }
  }
  
  # Loop through each profile as reference category
  for (ref_profile in all_profiles) {
    # Create directory for this reference profile
    ref_dir <- file.path(save_dir, paste0("ref_cat_", ref_profile))
    if (!dir.exists(ref_dir)) {
      dir.create(ref_dir, recursive = TRUE)
    }
    
    # Set current profile as reference
    df$profile <- relevel(df$profile, ref = ref_profile)
    
    # 1. Multinomial regression: Profile ~ SES
    multinom_formula <- as.formula(glue("profile ~ {paste(ses_vars, collapse = ' + ')}"))
    multinom_model <- multinom(multinom_formula, data = df, trace = FALSE)
    multinom_summary <- tidy(multinom_model) %>%
      mutate(
        odds_ratio = exp(estimate),
        conf.low = exp(estimate - 1.96 * std.error),
        conf.high = exp(estimate + 1.96 * std.error)
      )
    write_csv(multinom_summary, file.path(ref_dir, glue("{prefix}_multinom_profile~age+sex+ses_ref{ref_profile}.csv")))
    
    # 2. Linear regressions: For each outcome
    for (out in outcome_vars) {
      if (!(out %in% names(df))) next
      
      # (a) Outcome ~ Profile
      model_profile <- lm(as.formula(glue("{out} ~ profile")), data = df)
      model_profile_beta <- lm.beta(model_profile)
      reg_summary_profile <- tidy(model_profile)
      std_coefs_profile <- coef(model_profile_beta)
      profile_summary <- reg_summary_profile %>%
        mutate(
          std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_profile[term]),
          std_conf.low = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(profile_summary, file.path(ref_dir, glue("{prefix}_reg_{out}_profile_ref{ref_profile}.csv")))
      
      # (b) Outcome ~ Profile + SES_var (without interaction term)
      model_main <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')}")), data = df)
      model_main_beta <- lm.beta(model_main)
      reg_summary_main <- tidy(model_main)
      std_coefs_main <- coef(model_main_beta)
      main_summary <- reg_summary_main %>%
        mutate(
          std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_main[term]),
          std_conf.low = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(main_summary, file.path(ref_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses_ref{ref_profile}.csv")))
      
      # (c) Outcome ~ Profile + SES_var + Profile*Sex (with interaction term)
      model_combined <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars, collapse = ' + ')} + profile*sex")), data = df)
      model_combined_beta <- lm.beta(model_combined)
      reg_summary_combined <- tidy(model_combined)
      std_coefs_combined <- coef(model_combined_beta)
      combined_summary <- reg_summary_combined %>%
        mutate(
          std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_combined[term]),
          std_conf.low = std_estimate - 1.96 * std.error,
          std_conf.high = std_estimate + 1.96 * std.error
        ) %>%
        select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
      write_csv(combined_summary, file.path(ref_dir, glue("{prefix}_reg_{out}_profile+age+sex+ses+profilexsex_ref{ref_profile}.csv")))
      
      # (d) Stratified analyses by sex: Outcome ~ Profile + SES_var (males only)
      df_males <- df[df$sex == 1, ]  # Boys
      if (nrow(df_males) > 0) {
        # Remove sex from ses_vars for stratified analysis since we're subsetting
        ses_vars_no_sex <- setdiff(ses_vars, "sex")
        if (length(ses_vars_no_sex) > 0) {
          model_males <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}")), data = df_males)
        } else {
          model_males <- lm(as.formula(glue("{out} ~ profile")), data = df_males)
        }
        model_males_beta <- lm.beta(model_males)
        reg_summary_males <- tidy(model_males)
        std_coefs_males <- coef(model_males_beta)
        males_summary <- reg_summary_males %>%
          mutate(
            std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_males[term]),
            std_conf.low = std_estimate - 1.96 * std.error,
            std_conf.high = std_estimate + 1.96 * std.error
          ) %>%
          select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
        write_csv(males_summary, file.path(ref_dir, glue("{prefix}_reg_{out}_profile+ses_MALES_ref{ref_profile}.csv")))
      }
      
      # (e) Stratified analyses by sex: Outcome ~ Profile + SES_var (females only)
      df_females <- df[df$sex == 2, ]  # Girls
      if (nrow(df_females) > 0) {
        # Remove sex from ses_vars for stratified analysis since we're subsetting
        ses_vars_no_sex <- setdiff(ses_vars, "sex")
        if (length(ses_vars_no_sex) > 0) {
          model_females <- lm(as.formula(glue("{out} ~ profile + {paste(ses_vars_no_sex, collapse = ' + ')}")), data = df_females)
        } else {
          model_females <- lm(as.formula(glue("{out} ~ profile")), data = df_females)
        }
        model_females_beta <- lm.beta(model_females)
        reg_summary_females <- tidy(model_females)
        std_coefs_females <- coef(model_females_beta)
        females_summary <- reg_summary_females %>%
          mutate(
            std_estimate = ifelse(term == "(Intercept)", estimate, std_coefs_females[term]),
            std_conf.low = std_estimate - 1.96 * std.error,
            std_conf.high = std_estimate + 1.96 * std.error
          ) %>%
          select(term, estimate, std.error, statistic, p.value, std_estimate, std_conf.low, std_conf.high)
        write_csv(females_summary, file.path(ref_dir, glue("{prefix}_reg_{out}_profile+ses_FEMALES_ref{ref_profile}.csv")))
      }
    }
  }
}

# Set working directory to LPA Analysis folder
setwd(file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse"))

# Load data
data_path = "hbsc_variables.csv"
hbsc_def <- read.csv(data_path, header=TRUE)
hbsc_def <- tibble::rowid_to_column(hbsc_def, "ID")

# Removing Turkey, because substance use was not surveyed / Kazakhstan because only C1 solution
hbsc_def <- hbsc_def %>% 
  filter(!countryname %in% c("Turkey", "Kazakhstan"))

# Load Labels and ClassSolution
hbsc_labels <- read_excel("hbsc_labels.xlsx")

# Define variables
outcome <- c("health", "lifesat", "feeling", "ache")
ses <- c("age", "sex", "fas")

# country_names <- "Switzerland"  # or vector of countries
country_names <- unique(hbsc_def$countryname)

# Create main Regression directory if it doesn't exist
regression_dir <- "Regression (ref_profile)"
if (!dir.exists(regression_dir)) {
  dir.create(regression_dir)
}

# Main loop over countries
for (country in country_names) {
  # Create country directory within the Regression folder
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) {
    dir.create(country_regression_dir)
  }
  
  class_solution <- hbsc_labels %>% 
    filter(Country == country) %>% 
    pull(ClassSolution)
  
  # --- COUNTRY-LEVEL (All years together) ---
  cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
  
  if (file.exists(cprob_path)) {
    profiles <- read.csv(cprob_path, header = TRUE)$C
    df_country <- hbsc_def[hbsc_def$countryname == country, ]
    # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
    df_country <- df_country %>% 
      filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
    run_refcat_regressions(df_country, profiles, ses, outcome, country_regression_dir, glue("{country}_all"))
  }
  
  data_country <- hbsc_def[hbsc_def$countryname == country, ]
  survey_years <- unique(data_country$surveyyear)
  
  # --- YEAR-LEVEL (separate regressions per year) ---
  for (year in survey_years) {
    year_dir <- file.path("LPA ID", country, glue("{year}"))
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    
    if (!dir.exists(year_regression_dir)) {
      dir.create(year_regression_dir)
    }
    
    cprob_path <- file.path(year_dir, glue("c_prob_{country}_{year}_{class_solution}.csv"))
    
    if (file.exists(cprob_path)) {
      profiles <- read.csv(cprob_path, header = TRUE)$C
      df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ]
      # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
      df_year <- df_year %>% 
        filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
      run_refcat_regressions(df_year, profiles, ses, outcome, year_regression_dir, glue("{country}_{year}"))
    }
  }
}

'''INSERTION: Sensitivity Analysis {ref profile}'''

# Set working directory to LPA Analysis folder
setwd(file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse"))

# Load data
data_path = "hbsc_variables.csv"
hbsc_def <- read.csv(data_path, header=TRUE)
hbsc_def <- tibble::rowid_to_column(hbsc_def, "ID")

# Removing Turkey, because substance use was not surveyed / Kazakhstan because only C1 solution
hbsc_def <- hbsc_def %>% 
  filter(!countryname %in% c("Turkey", "Kazakhstan"))

# Load Labels and ClassSolution
hbsc_labels <- read_excel("hbsc_labels.xlsx")

# Define variables
outcome <- c("health", "lifesat", "feeling", "ache")
ses <- c("age", "sex", "fas")

country_names <- "Switzerland"  # or vector of countries
# country_names <- unique(hbsc_def$countryname)

# Create main Regression directory if it doesn't exist
regression_dir <- "Sensitivity Analysis (ref_profile)"
if (!dir.exists(regression_dir)) {
  dir.create(regression_dir)
}

# Main loop over countries
for (country in country_names) {
  # Create country directory within the Regression folder
  country_regression_dir <- file.path(regression_dir, country)
  if (!dir.exists(country_regression_dir)) {
    dir.create(country_regression_dir)
  }
  
  class_solution <- hbsc_labels %>% 
    filter(Country == country) %>% 
    pull(ClassSolution)
  
  # --- COUNTRY-LEVEL (All years together) ---
  cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
  
  if (file.exists(cprob_path)) {
    profiles <- read.csv(cprob_path, header = TRUE)$C
    df_country <- hbsc_def[hbsc_def$countryname == country, ]
    # Get the row indices of cases where ALL LPA variables are NA to remove prior to regression calculation
    df_country <- df_country %>% 
      filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
    # run_refcat_regressions(df_country, profiles, ses, outcome, country_regression_dir, glue("{country}_all"))
  }
  
  data_country <- hbsc_def[hbsc_def$countryname == country, ]
  survey_years <- unique(data_country$surveyyear)
  
  # --- YEAR-LEVEL (separate regressions per year) ---
  for (year in survey_years) {
    year_regression_dir <- file.path(country_regression_dir, glue("{year}"))
    
    if (!dir.exists(year_regression_dir)) {
      dir.create(year_regression_dir)
    }
    
    # Use country-level cprob file
    cprob_path <- file.path("LPA ID", country, glue("c_prob_{country}_{class_solution}.csv"))
    
    if (file.exists(cprob_path)) {
      profiles_full <- read.csv(cprob_path, header = TRUE)
      
      # Get year-specific data
      df_year <- hbsc_def[hbsc_def$countryname == country & hbsc_def$surveyyear == year, ]
      
      # Filter out cases where ALL LPA variables are NA
      df_year <- df_year %>% 
        filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
      
      # Match profiles to year-specific data using ID
      profiles <- profiles_full$C[profiles_full$ID %in% df_year$ID]
      
      run_refcat_regressions(df_year, profiles, ses, outcome, year_regression_dir, glue("{country}_{year}"))
    }
  }
}

'''INSERTION END: Sensitivity Analysis {ref_profile}'''

### Replacement for .out file processing and package MplusAutomation in webapp

# Function to extract means and class proportions from a single Mplus output file
process_mplus_output <- function(file_path) {
  tryCatch({
    # Load the Mplus output containing the parameters
    allModelParameters <- readModels(file_path, what = "parameters")$parameters
    
    # 1. Navigate to the unstandardized parameters
    unstandardized_params <- allModelParameters$unstandardized
    
    # 2. Filter to only get the "Means" estimates for all latent classes
    means_estimates <- unstandardized_params[unstandardized_params$paramHeader == "Means", ]
    
    # 3. Prepare a dataframe for easier manipulation
    means_df <- data.frame(
      Variable = means_estimates$param,
      LatentClass = means_estimates$LatentClass,
      Estimate = means_estimates$est,
      SE = means_estimates$se,
      PValue = means_estimates$pval
    )
    
    # 4. Filter out the rows related to the categorical latent variables (C#1, C#2, C#3, etc.)
    means_df_filtered <- means_df[!means_df$Variable %in% c("C#1", "C#2", "C#3", "C#4", "C#5"), ]
    
    # 5. Extract class proportions
    output_enum <- readModels(dirname(file_path))
    output_file_name <- gsub(" ", ".", basename(file_path))
    
    if (!is.null(output_enum[[output_file_name]]$class_counts$modelEstimated$proportion)) {
      class_proportions <- as.data.frame(output_enum[[output_file_name]]$class_counts$modelEstimated$proportion)
      colnames(class_proportions) <- "Proportion"
      class_proportions$Proportion <- round(class_proportions$Proportion * 100, 1)  # Convert to percentage
    } else {
      stop("Class proportions not found in the output.")
    }
    
    # 6. Merge the means and class proportions into one dataframe
    # Assuming each class has 5 variables (PHY, SLE, UND, SM, ALC)
    means_df_filtered$Proportion <- rep(class_proportions$Proportion, each = 5)
    
    # 7. Ensure the `Variable` column is relabeled correctly
    means_df_filtered$Variable <- factor(means_df_filtered$Variable,
                                         levels = c("PHY", "SLE", "UND", "SM", "ALC"),
                                         labels = c("Physical Inactivity", "Sleep Problems", "Unhealthy Diet", "Smoking", "Alcohol"))
    
    return(means_df_filtered)
    
  }, error = function(e) {
    cat("Error processing file:", file_path, "\n")
    cat("Error message:", e$message, "\n")
    return(NULL)
  })
}

# Function to find Mplus files based on country and class selection
find_mplus_files <- function(base_path, hbsc_labels) {
  all_selected_files <- c()
  
  # Loop through each country in hbsc_labels
  for (i in 1:nrow(hbsc_labels)) {
    country <- hbsc_labels$Country[i]  # Adjust column name if different
    class_solution <- hbsc_labels$ClassSolution[i]
    
    # Extract the number from class selection (e.g., "C4" -> "4")
    class_num <- gsub("C", "", class_solution)
    
    # Create the full path to the country's directory
    country_path <- file.path(base_path, country)
    
    # Check if the country directory exists
    if (!dir.exists(country_path)) {
      cat("Warning: Directory not found for", country, "at", country_path, "\n")
      next
    }
    
    # Create the pattern for this country's file
    pattern <- paste0("_c", class_num, "\\.out$")
    
    # Find files matching this pattern in the country's directory
    country_files <- list.files(country_path, pattern = pattern, recursive = TRUE, full.names = TRUE)
    
    if (length(country_files) == 0) {
      cat("Warning: No files matching pattern", pattern, "found for", country, "\n")
    } else {
      cat("Found", length(country_files), "file(s) for", country, "with pattern", pattern, "\n")
    }
    
    all_selected_files <- c(all_selected_files, country_files)
  }
  
  return(all_selected_files)
}

# Main processing function
batch_process_mplus_files <- function(base_lpa_path, hbsc_labels) {
  # Find all Mplus output files based on class selection
  mplus_files <- find_mplus_files(base_lpa_path, hbsc_labels)
  
  cat("Found", length(mplus_files), "Mplus output files to process:\n")
  for(file in mplus_files) {
    cat(" -", file, "\n")
  }
  
  # Process each file
  for(file_path in mplus_files) {
    cat("\nProcessing:", file_path, "\n")
    
    # Process the Mplus output
    result_df <- process_mplus_output(file_path)
    
    if (!is.null(result_df)) {
      # Create CSV file path (same location as .out file, but with .csv extension)
      csv_file_path <- gsub("\\.out$", ".csv", file_path)
      
      # Write the CSV file
      write.csv(result_df, csv_file_path, row.names = FALSE)
      cat("Created CSV file:", csv_file_path, "\n")
      
      # Print summary
      cat("Summary: ", nrow(result_df), "rows,", length(unique(result_df$LatentClass)), "classes,", 
          length(unique(result_df$Variable)), "variables\n")
    } else {
      cat("Failed to process file:", file_path, "\n")
    }
  }
  
  cat("\n=== Batch processing completed ===\n")
}

# Set your base LPA directory path
base_lpa_path <- file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "LPA ID")

# Run the batch processing with hbsc_labels
batch_process_mplus_files(base_lpa_path, hbsc_labels)

### Enum summary batch processing for class selection based on threshold (ClassSizes_n_Percent >= 2.0)

library(readxl)
library(openxlsx)

base_enum_path <- file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "LPA ID")

# Initialize empty list to store all dataframes
class_df <- list()

for (country in unique(hbsc_def$countryname)) {
  # Read country-level file
  file_name <- paste0(country, "_enum_summary.xlsx")
  country_df <- read_excel(file.path(base_enum_path, country, file_name))
  
  # Process ClassSizes_n_Percent and create Label column
  country_df$Label <- sapply(country_df$ClassSizes_n_Percent, function(x) {
    # Extract all percentages from the string
    percentages <- str_extract_all(x, "\\d+\\.?\\d*")[[1]]
    percentages <- as.numeric(percentages)
    
    # Check if any percentage >= 2.0
    ifelse(all(percentages >= 2.0), 1, 0)
  })
  
  # Add to list
  class_df[[length(class_df) + 1]] <- country_df
  
  # Read year-specific files for this country
  for (year in unique(hbsc_def %>% filter(countryname == country) %>% pull(surveyyear))) {
    file_name <- paste0(country, "_", year, "_enum_summary.xlsx")
    year_df <- read_excel(file.path(base_enum_path, country, year, file_name))
    
    # Process ClassSizes_n_Percent and create Label column
    year_df$Label <- sapply(year_df$ClassSizes_n_Percent, function(x) {
      # Extract all percentages from the string
      percentages <- str_extract_all(x, "\\d+\\.?\\d*")[[1]]
      percentages <- as.numeric(percentages)
      
      # Check if any percentage >= 2.0
      ifelse(all(percentages >= 2.0), 1, 0)
    })
    
    # Add to list
    class_df[[length(class_df) + 1]] <- year_df
  }
}

# Combine all dataframes
class_selection <- bind_rows(class_df)

write.xlsx(class_selection, file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "class_selection.xlsx"))


### Profile distribution consistency check
# Function to check profile consistency for HBSC Switzerland data
check_hbsc_profile_consistency <- function() {
  
  # Load the data
  data_path <- "hbsc_variables.csv"
  hbsc_def <- read.csv(data_path, header = TRUE)
  
  # Filter for Switzerland
  swiss_data <- hbsc_def[hbsc_def$countryname == "Switzerland", ]
  
  # Remove cases where ALL LPA variables are NA (same filtering as in main analysis)
  swiss_data <- swiss_data %>% 
    filter(!(is.na(physinact) & is.na(sleepprob) & is.na(undiet) & is.na(smoking) & is.na(alcohol)))
  
  # Get survey years
  survey_years <- unique(swiss_data$surveyyear)
  survey_years <- survey_years[!is.na(survey_years)]
  survey_years <- sort(survey_years)
  
  cat("=== HBSC SWITZERLAND PROFILE CONSISTENCY CHECK ===\n")
  cat("Survey years available:", paste(survey_years, collapse = ", "), "\n")
  cat("Total Swiss observations:", nrow(swiss_data), "\n\n")
  
  # Initialize results storage
  results <- list()
  
  # 1. OVERALL ANALYSIS (All years combined)
  cat("=== 1. OVERALL ANALYSIS (ALL YEARS COMBINED) ===\n\n")
  
  # Load overall profiles
  cprob_path_overall <- file.path("LPA", "Switzerland", "c_prob_Switzerland_C4.csv")
  
  if (file.exists(cprob_path_overall)) {
    profiles_overall <- read.csv(cprob_path_overall, header = TRUE)$C
    
    # Create analysis dataframe
    analysis_df_overall <- data.frame(
      profile = as.factor(profiles_overall),
      sex = swiss_data$sex,
      surveyyear = swiss_data$surveyyear
    ) %>%
      filter(!is.na(profile) & !is.na(sex))
    
    # Overall summary by gender
    overall_summary <- analysis_df_overall %>%
      group_by(sex, profile) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(sex) %>%
      mutate(
        percentage = round(count / sum(count) * 100, 1),
        total_n = sum(count)
      ) %>%
      arrange(sex, desc(count))
    
    cat("Profile distribution by gender:\n")
    print(overall_summary)
    
    # Find most frequent profile by gender
    most_frequent_overall <- overall_summary %>%
      group_by(sex) %>%
      slice_max(count, n = 1) %>%
      select(sex, profile, count, percentage, total_n)
    
    cat("\nMost frequent profile by gender:\n")
    print(most_frequent_overall)
    
    # Check consistency
    unique_profiles_overall <- unique(most_frequent_overall$profile)
    
    if (length(unique_profiles_overall) == 1) {
      cat("\n✓ OVERALL CONSISTENT: Profile", as.character(unique_profiles_overall), "is most frequent for both genders\n")
      overall_consistent <- TRUE
    } else {
      cat("\n✗ OVERALL INCONSISTENT: Different most frequent profiles by gender\n")
      for (i in 1:nrow(most_frequent_overall)) {
        row <- most_frequent_overall[i, ]
        cat("  Sex", row$sex, ": Profile", as.character(row$profile), 
            "(", row$count, "/", row$total_n, " = ", row$percentage, "%)\n")
      }
      overall_consistent <- FALSE
    }
    
    results$overall <- list(
      consistent = overall_consistent,
      most_frequent = unique_profiles_overall,
      summary = most_frequent_overall
    )
    
  } else {
    cat("Overall profile file not found:", cprob_path_overall, "\n")
    results$overall <- list(consistent = NA, most_frequent = NA, summary = NA)
  }
  
  # 2. YEAR-BY-YEAR ANALYSIS
  cat("\n\n=== 2. YEAR-BY-YEAR ANALYSIS ===\n\n")
  
  year_results <- list()
  year_consistent_vec <- c()
  
  for (year in survey_years) {
    cat("--- Survey Year:", year, "---\n")
    
    # Load year-specific profiles
    cprob_path_year <- file.path("LPA", "Switzerland", as.character(year), 
                                 paste0("c_prob_Switzerland_", year, "_C4.csv"))
    
    if (file.exists(cprob_path_year)) {
      profiles_year <- read.csv(cprob_path_year, header = TRUE)$C
      
      # Filter data for this year
      swiss_year <- swiss_data[swiss_data$surveyyear == year, ]
      
      # Create analysis dataframe
      analysis_df_year <- data.frame(
        profile = as.factor(profiles_year),
        sex = swiss_year$sex
      ) %>%
        filter(!is.na(profile) & !is.na(sex))
      
      cat("Year", year, "- Total observations:", nrow(analysis_df_year), "\n")
      
      # Year summary by gender
      year_summary <- analysis_df_year %>%
        group_by(sex, profile) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(sex) %>%
        mutate(
          percentage = round(count / sum(count) * 100, 1),
          total_n = sum(count)
        ) %>%
        arrange(sex, desc(count))
      
      cat("Profile distribution by gender:\n")
      print(year_summary)
      
      # Find most frequent profile by gender for this year
      most_frequent_year <- year_summary %>%
        group_by(sex) %>%
        slice_max(count, n = 1) %>%
        select(sex, profile, count, percentage, total_n)
      
      cat("Most frequent profile by gender:\n")
      print(most_frequent_year)
      
      # Check consistency for this year
      unique_profiles_year <- unique(most_frequent_year$profile)
      
      if (length(unique_profiles_year) == 1) {
        cat("✓ Year", year, "CONSISTENT: Profile", as.character(unique_profiles_year), "\n\n")
        year_consistent <- TRUE
      } else {
        cat("✗ Year", year, "INCONSISTENT: Different profiles by gender\n")
        for (i in 1:nrow(most_frequent_year)) {
          row <- most_frequent_year[i, ]
          cat("  Sex", row$sex, ": Profile", as.character(row$profile), 
              "(", row$count, "/", row$total_n, " = ", row$percentage, "%)\n")
        }
        cat("\n")
        year_consistent <- FALSE
      }
      
      year_results[[as.character(year)]] <- list(
        consistent = year_consistent,
        most_frequent = unique_profiles_year,
        summary = most_frequent_year
      )
      year_consistent_vec <- c(year_consistent_vec, year_consistent)
      
    } else {
      cat("Profile file not found for year", year, ":", cprob_path_year, "\n\n")
      year_results[[as.character(year)]] <- list(consistent = NA, most_frequent = NA, summary = NA)
      year_consistent_vec <- c(year_consistent_vec, NA)
    }
  }
  
  # 3. OVERALL SUMMARY
  cat("=== 3. FINAL SUMMARY ===\n")
  all_years_consistent <- all(year_consistent_vec, na.rm = TRUE)
  
  cat("Overall consistency (all years combined):", 
      ifelse(results$overall$consistent, "✓ CONSISTENT", "✗ INCONSISTENT"), "\n")
  cat("Year-by-year consistency:", 
      ifelse(all_years_consistent, "✓ ALL YEARS CONSISTENT", "✗ SOME YEARS INCONSISTENT"), "\n")
  
  if (!all_years_consistent) {
    inconsistent_years <- survey_years[!year_consistent_vec]
    inconsistent_years <- inconsistent_years[!is.na(inconsistent_years)]
    if (length(inconsistent_years) > 0) {
      cat("Inconsistent years:", paste(inconsistent_years, collapse = ", "), "\n")
    }
  }
  
  # 4. RECOMMENDATIONS
  cat("\n=== 4. RECOMMENDATIONS FOR REGRESSION ANALYSIS ===\n")
  
  if (results$overall$consistent && all_years_consistent) {
    cat("✓ RECOMMENDED: Use current regression approach with Profile", 
        as.character(results$overall$most_frequent), "as reference\n")
    cat("  The most frequent profile is consistent across genders and years.\n")
  } else if (results$overall$consistent && !all_years_consistent) {
    cat("⚠ CAUTION: Overall consistent but some years inconsistent\n")
    cat("  Consider using Profile", as.character(results$overall$most_frequent), 
        "as reference, but be aware of year-specific differences.\n")
  } else {
    cat("⚠ CONSIDER ALTERNATIVES:\n")
    cat("  1. Use overall most frequent profile despite gender differences\n")
    cat("  2. Run separate analyses by gender with gender-specific reference profiles\n")
    cat("  3. Choose a theoretically meaningful reference profile\n")
  }
  
  # Return comprehensive results
  results$years <- year_results
  results$all_years_consistent <- all_years_consistent
  results$survey_years <- survey_years
  
  return(invisible(results))
}

# Run the analysis
results <- check_hbsc_profile_consistency()

### Preparation for AI-based taxology of profiles

library(tidyxl)
library(MplusAutomation)
library(dplyr)
library(tidyr)

# First, get the formatted titles
file_path <- r"(C:\Users\hanst\Downloads\class_selection.xlsx)"
cells <- xlsx_cells(file_path, sheets = "Overview")
title_cells <- cells[cells$col == 1 & cells$row > 1 & !is.na(cells$character), ]
formatted_cells <- title_cells[title_cells$local_format_id > 1, ]
formatted_titles <- formatted_cells$character

print(paste("Processing", length(formatted_titles), "formatted titles"))

# Function to parse title and create file path
file_path_from_title <- function(title) {
  # Remove trailing semicolon if present
  title <- gsub(";$", "", title)
  
  # Parse the title
  if (grepl("_[0-9]{4}_C[0-9]+", title)) {
    # Format: Country_Year_C#
    parts <- strsplit(title, "_")[[1]]
    country <- parts[1]
    year <- parts[2]
    class_num <- parts[3]
  } else if (grepl("_C[0-9]+", title)) {
    # Format: Country_C#
    parts <- strsplit(title, "_C")[[1]]
    country <- parts[1]
    year <- ""
    class_num <- paste0("C", parts[2])
  } else {
    return(NULL)  # Skip if doesn't match expected pattern
  }
  
  # Create file path
  if (year != "") {
    file_path <- file.path(Sys.getenv("USERPROFILE"), 
                           "OneDrive - Universität Zürich UZH", 
                           "Datenanalyse", 
                           "LPA ID", 
                           country, 
                           year, 
                           paste0(tolower(country), "_", year, "_", tolower(class_num), ".out"))
  } else {
    file_path <- file.path(Sys.getenv("USERPROFILE"), 
                           "OneDrive - Universität Zürich UZH", 
                           "Datenanalyse", 
                           "LPA ID", 
                           country, 
                           paste0(tolower(country), "_", tolower(class_num), ".out"))
  }
  
  return(list(
    country = country,
    year = year,
    class_num = class_num,
    file_path = file_path
  ))
}

# Function to process a single Mplus output file
mplus_file_to_means <- function(file_info) {
  tryCatch({
    # Check if file exists
    if (!file.exists(file_info$file_path)) {
      cat("File not found:", file_info$file_path, "\n")
      return(NULL)
    }
    
    # Load the Mplus output
    allModelParameters <- readModels(file_info$file_path, what = "parameters")$parameters
    
    # Navigate to the unstandardized parameters
    unstandardized_params <- allModelParameters$unstandardized
    
    # Filter to only get the "Means" estimates for all latent classes
    means_estimates <- unstandardized_params[unstandardized_params$paramHeader == "Means", ]
    
    if (nrow(means_estimates) == 0) {
      cat("No means estimates found in:", file_info$file_path, "\n")
      return(NULL)
    }
    
    # EXCLUDE unwanted rows
    means_estimates <- means_estimates[
      means_estimates$LatentClass != "Categorical.Latent.Variables" & 
        !grepl("^C#[0-9]+$", means_estimates$param), 
    ]
    
    if (nrow(means_estimates) == 0) {
      cat("No valid means estimates after filtering in:", file_info$file_path, "\n")
      return(NULL)
    }
    
    # Create dataframe with metadata
    means_df <- data.frame(
      Country = file_info$country,
      SurveyYear = ifelse(file_info$year == "", NA, file_info$year),
      ClassSolution = file_info$class_num,
      Variable = means_estimates$param,
      LatentClass = means_estimates$LatentClass,
      Estimate = means_estimates$est,
      stringsAsFactors = FALSE
    )
    
    return(means_df)
    
  }, error = function(e) {
    cat("Error processing file:", file_info$file_path, "\nError:", e$message, "\n")
    return(NULL)
  })
}

# Process all formatted titles
all_results <- list()
processed_count <- 0

for (i in 1:length(formatted_titles)) {
  title <- formatted_titles[i]
  cat("Processing", i, "of", length(formatted_titles), ":", title, "\n")
  
  # Parse title and create file path
  file_info <- file_path_from_title(title)
  
  if (is.null(file_info)) {
    cat("Could not parse title:", title, "\n")
    next
  }
  
  # Process the Mplus file
  result <- mplus_file_to_means(file_info)
  
  if (!is.null(result)) {
    all_results[[length(all_results) + 1]] <- result
    processed_count <- processed_count + 1
  }
}

# Combine all results
if (length(all_results) > 0) {
  final_df <- do.call(rbind, all_results)
  
  # Convert to wide format (variables as columns)
  wide_df <- final_df %>%
    select(Country, SurveyYear, ClassSolution, Variable, LatentClass, Estimate) %>%
    pivot_wider(
      names_from = Variable,
      values_from = Estimate,
      id_cols = c(Country, SurveyYear, ClassSolution, LatentClass)
    )
  
  # RENAME VARIABLES before saving
  variable_names <- c(
    "ALC" = "Alcohol Consumption",
    "SM" = "Smoking", 
    "PHY" = "Physical Inactivity",
    "UND" = "Unhealthy Diet",
    "SLE" = "Sleep Problems"
  )
  
  # Rename columns if they exist
  for (old_name in names(variable_names)) {
    if (old_name %in% colnames(wide_df)) {
      colnames(wide_df)[colnames(wide_df) == old_name] <- variable_names[old_name]
    }
  }
  
  # Save to CSV
  output_file <- file.path(Sys.getenv("USERPROFILE"), 
                           "OneDrive - Universität Zürich UZH",
                           "Datenanalyse",
                           "AI_Profile_VarMeans_Compilation.csv")
  
  write.csv(wide_df, output_file, row.names = FALSE)
  
  cat("Successfully processed", processed_count, "files\n")
  cat("Results saved to:", output_file, "\n")
  cat("Final dataset dimensions:", nrow(wide_df), "rows,", ncol(wide_df), "columns\n")
  
  # Show preview
  print("Preview of results:")
  print(head(wide_df))
  
} else {
  cat("No files were successfully processed.\n")
}

# Show summary of what was attempted
cat("\nSummary:\n")
cat("Total formatted titles:", length(formatted_titles), "\n")
cat("Successfully processed:", processed_count, "\n")
cat("Failed to process:", length(formatted_titles) - processed_count, "\n")

### Cronbach alpha calculation

data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_raw.csv")
hbsc_alpha <- read.csv(data_path, header=TRUE)
hbsc_alpha_CH <- hbsc_alpha %>% filter(countryname == 'Switzerland')

# Load required packages
library(psych)
library(knitr)

# Define your variables and their items
variables <- list(
  "Alcohol" = c("beer_rev", "wine_rev", "spirits_rev", "alc30d_2_rs"),
  "Smoking" = c("smoking_rev", "smok30d_2_rs"),
  "Physical Inactivity" = "physinact",
  "Sleep Problems" = "sleepprob",
  "Unhealthy Diet" = c("fruits3r", "vegetables3r", "sweets3", "softdrinks3"),
  "Feeling" = c("dizzy_rev", "irritable_rev", "nervous_rev", "feellow_rev"),
  "Ache" = c("backache_rev", "headache_rev", "stomachache_rev")
)

# Get unique survey years
survey_years <- unique(hbsc_alpha_CH$surveyyear)
survey_years <- sort(survey_years)

# Create empty dataframe to store results
results_by_year <- data.frame()

# Calculate alpha for each variable within each survey year
for (year in survey_years) {
  # Subset data for this year
  year_data <- hbsc_alpha_CH[hbsc_alpha_CH$surveyyear == year, ]
  
  for (var_name in names(variables)) {
    items <- variables[[var_name]]
    n_items <- length(items)
    
    # Check if single item
    if (n_items == 1) {
      results_by_year <- rbind(results_by_year, data.frame(
        SurveyYear = year,
        Variable = var_name,
        Items = n_items,
        N = sum(!is.na(year_data[, items])),
        Alpha = "N/A"
      ))
    } else {
      # Get the subset of items for this year
      item_data <- year_data[, items]
      
      # Check if all values are NA for each item
      all_na <- apply(item_data, 2, function(x) all(is.na(x)))
      
      # Remove items that are completely NA
      valid_items <- items[!all_na]
      n_valid_items <- length(valid_items)
      
      # Count complete cases for ALL original items
      complete_cases <- sum(complete.cases(item_data))
      
      # Only calculate alpha if we have at least 2 valid items
      if (n_valid_items < 2) {
        results_by_year <- rbind(results_by_year, data.frame(
          SurveyYear = year,
          Variable = var_name,
          Items = n_items,
          N = complete_cases,
          Alpha = "No data"
        ))
      } else {
        # Use only the valid items for alpha calculation
        valid_item_data <- year_data[, valid_items]
        
        # Try to calculate alpha, catch any errors
        alpha_result <- tryCatch({
          suppressWarnings(alpha(valid_item_data, check.keys = TRUE))
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(alpha_result)) {
          results_by_year <- rbind(results_by_year, data.frame(
            SurveyYear = year,
            Variable = var_name,
            Items = n_items,
            N = complete_cases,
            Alpha = "Error"
          ))
        } else {
          results_by_year <- rbind(results_by_year, data.frame(
            SurveyYear = year,
            Variable = var_name,
            Items = n_items,
            N = sum(complete.cases(valid_item_data)),
            Alpha = sprintf("%.3f", alpha_result$total$raw_alpha)
          ))
        }
      }
    }
  }
}

# Display results
print(results_by_year)

# Create wide format table (years as columns)
results_wide <- results_by_year %>%
  select(SurveyYear, Variable, Alpha) %>%
  pivot_wider(names_from = SurveyYear, 
              values_from = Alpha,
              names_prefix = "Year_")

# Add number of items column
items_count <- data.frame(
  Variable = names(variables),
  Items = sapply(variables, length)
)

results_wide <- left_join(results_wide, items_count, by = "Variable")

# Reorder columns to have Items first, then years
year_cols <- grep("Year_", names(results_wide), value = TRUE)
results_wide <- results_wide[, c("Variable", "Items", year_cols)]

# Display wide format table
cat("\n=== CRONBACH'S ALPHA BY SURVEY YEAR (WIDE FORMAT) ===\n")
print(results_wide)

# Export both formats
write.csv(results_wide, "hbsc_alpha_by_year_wide.csv", row.names = FALSE)

# Optional: Detailed analysis for each year
cat("\n\n=== DETAILED ALPHA ANALYSIS BY YEAR ===\n\n")

for (year in survey_years) {
  year_data <- hbsc_alpha_CH[hbsc_alpha_CH$surveyyear == year, ]
  
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("SURVEY YEAR:", year, "\n")
  cat(rep("=", 70), "\n", sep = "")
  
  for (var_name in names(variables)) {
    items <- variables[[var_name]]
    
    if (length(items) > 1) {
      cat("\n", var_name, "\n")
      cat(rep("-", 50), "\n", sep = "")
      
      alpha_result <- alpha(year_data[, items], check.keys = TRUE)
      print(alpha_result)
      cat("\n")
    }
  }
}

# Optional: Get detailed output for each multi-item scale
cat("\n\n=== DETAILED ALPHA ANALYSIS ===\n\n")

for (var_name in names(variables)) {
  items <- variables[[var_name]]
  
  if (length(items) > 1) {
    cat("\n", rep("=", 60), "\n", sep = "")
    cat(var_name, "\n")
    cat(rep("=", 60), "\n", sep = "")
    
    alpha_result <- alpha(hbsc_alpha_CH[, items], check.keys = TRUE)
    print(alpha_result)
    cat("\n")
  }
}

# Create summary dataframe
item_availability <- data.frame()

for (year in survey_years) {
  year_data <- hbsc_alpha_CH[hbsc_alpha_CH$surveyyear == year, ]
  
  for (var_name in names(variables)) {
    items <- variables[[var_name]]
    
    for (item in items) {
      # Check if item has any non-NA values
      has_data <- any(!is.na(year_data[[item]]))
      
      item_availability <- rbind(item_availability, data.frame(
        Variable = var_name,
        Item = item,
        SurveyYear = year,
        Available = ifelse(has_data, "Yes", "No")
      ))
    }
  }
}

# Create wide format (items as rows, years as columns)
item_wide <- item_availability %>%
  select(Variable, Item, SurveyYear, Available) %>%
  pivot_wider(names_from = SurveyYear,
              values_from = Available,
              names_prefix = "Year_")

# Arrange by variable
item_wide <- item_wide %>%
  arrange(Variable, Item)

# Display table
print(item_wide)

### Multilevel analysis data processing (per survey year basis)

library(readxl)

data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_labels.xlsx")
hbsc_labels <- read_excel(data_path)

class_solution <- hbsc_labels$ClassSolution
hbsc_variables <- read.csv("C:/Users/hanst/OneDrive - Universität Zürich UZH/Datenanalyse/hbsc_variables.csv")
hbsc_variables <- tibble::rowid_to_column(hbsc_variables, "ID")

# Initialize empty list to store data frames
df_list <- list()

# Get unique countries from hbsc_labels
unique_countries <- unique(hbsc_labels$Country)

# Loop through each country
for (current_country in unique_countries) {
  
  # Get ClassSolution for this country
  class_solution <- hbsc_labels %>%
    filter(Country == current_country) %>%
    pull(ClassSolution) %>%
    unique()
  
  # Get unique survey years for this country from hbsc_variables
  survey_years <- hbsc_variables %>%
    filter(countryname == current_country) %>%
    pull(surveyyear) %>%
    unique()
  
  # Loop through each survey year
  for (current_year in survey_years) {
    
    # Skip problematic country-year combinations
    if ((current_country == "Spain" && current_year == 2006) ||
        (current_country == "Greenland" && current_year == 2018) ||
        (current_country == "Belgium French" && current_year == 2002) ||
        (current_country == "North Macedonia" && current_year == 2018)) {
      cat("Skipping:", current_country, "-", current_year, "(unsuitable data)\n")
      next
    }
    
    # Construct filepath
    filepath <- file.path(
      Sys.getenv("USERPROFILE"), 
      "OneDrive - Universität Zürich UZH", 
      "Datenanalyse",
      "LPA ID",
      current_country, 
      as.character(current_year), 
      paste0("c_prob_", current_country, "_", current_year, "_", class_solution, ".csv")
    )
    
    # Check if file exists
    if (file.exists(filepath)) {
      
      # Read the CSV file
      c_prob_data <- read.csv(filepath)
      
      # Get relevant variables from hbsc_variables for this country and year
      hbsc_subset <- hbsc_variables %>%
        filter(countryname == current_country, surveyyear == current_year) %>%
        select(ID, sex, age, fas, lifesat, ache, feeling, health, countryname, surveyyear)
      
      # Merge data frames by ID
      merged_data <- c_prob_data %>%
        inner_join(hbsc_subset, by = "ID") %>%
        select(ID, countryname, surveyyear, sex, age, fas, lifesat, ache, feeling, health, PHY, SLE, UND, SM, ALC, C)
      
      # Add to list
      df_list[[paste(current_country, current_year, sep = "_")]] <- merged_data
      
      cat("Processed:", current_country, "-", current_year, "\n")
      
    } else {
      cat("File not found:", filepath, "\n")
    }
  }
}

# Combine all data frames into one
multi_lvl_data_sy <- bind_rows(df_list)

### Multilevel indices

library(tidyr)

## HDR containing Gender Inequality Index and Human Development Index
hdr <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\HDR25_Composite_indices_complete_time_series.csv)") %>%
  select(country, hdi_2002, hdi_2006, hdi_2010, hdi_2014, hdi_2018,
                  ihdi_2010, ihdi_2014, ihdi_2018, 
                  gii_2002, gii_2006, gii_2010, gii_2014, gii_2018) %>%
  pivot_longer(
    cols = -country,
    names_to = c(".value", "year"),
    names_pattern = "(.+)_(\\d{4})"
  )

hdr <- hdr %>%
  mutate(year = as.integer(year))

# Rename countries before merging
multi_lvl_data_sy <- multi_lvl_data_sy %>%
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Moldova" ~ "Moldova (Republic of)",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_sy <- multi_lvl_data_sy %>%
  left_join(hdr, 
            by = c("countryname_merge" = "country", 
                   "surveyyear" = "year"))

## GINI
gini <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\swiid9_9_summary.csv)")

multi_lvl_data_sy <- multi_lvl_data_sy %>%
  select(-countryname_merge) %>%        # unselect previous country merging column
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Czechia" ~ "Czech Republic",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_sy <- multi_lvl_data_sy %>%
  left_join(gini %>% select(country, year, gini_disp),
            by = c("countryname_merge" = "country",
                   "surveyyear" = "year"))

## GDP
gdp <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5354.csv)", skip = 4) %>% 
  select(Country.Name, X2002, X2006, X2010, X2014, X2018) %>%
  pivot_longer(
    cols = -Country.Name,
    names_prefix = "X",                # remove the "X" prefix from column names. Year columns start with "X" because the column names would be numerical otherwise
    names_to = "year",
    values_to = "gdp"
  )

gdp <- gdp %>% 
  mutate(year = as.integer(year))

multi_lvl_data_sy <- multi_lvl_data_sy %>%
  select(-countryname_merge) %>%        # unselect previous country merging column
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Slovakia" ~ "Slovak Republic",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_sy <- multi_lvl_data_sy %>%
  left_join(gdp,
            by = c("countryname_merge" = "Country.Name",
                   "surveyyear" = "year")) %>% 
  select(-countryname_merge)

## Manually clean labels for merger with multi level data

# Load labels
data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_labels.xlsx")
hbsc_labels <- read_excel(data_path)

# # Check unique labels in each column
# for(i in 3:(ncol(hbsc_labels)-1)) {
#   print(colnames(hbsc_labels)[i])
#   print(table(hbsc_labels[,i], useNA = "ifany"))
# }

# Pivot longer per survey year

labels_sy <- hbsc_labels %>%
  pivot_longer(
    cols = matches("\\d{4}_Profile_\\d+"),  # Select columns matching the pattern YYYY_Profile_N
    names_to = "original_column",
    values_to = "Label"
  ) %>%
  mutate(
    surveyyear = as.integer(sub("(\\d{4})_Profile_\\d+", "\\1", original_column)),
    Class = as.integer(sub("\\d{4}_Profile_(\\d+)", "\\1", original_column))
  ) %>%
  select(-c(original_column, matches("Overall_Profile_\\d+"))) %>%  # Remove the temporary column and profile labels from overall data set
  filter(!is.na(Label))

# Merge labels to multi level analysis data
multi_lvl_data_sy <- multi_lvl_data_sy %>% 
  left_join(labels_sy,
            by = c("countryname" = "Country",
                   "surveyyear" = "surveyyear", 
                   "C" = "Class"))
multi_lvl_data_sy <- multi_lvl_data_sy %>% 
  select(ID, countryname, surveyyear, ClassSolution, sex, age, fas, lifesat, ache, feeling, health, PHY, SLE, UND, SM, ALC, C, Label, hdi, ihdi, gii, gini_disp, gdp)

# Write .csv file
write.csv(multi_lvl_data_sy, "hbsc_mlvl_data_per_sy.csv")

### Multilevel analysis data processing (on overall basis)

data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_labels.xlsx")
hbsc_labels <- read_excel(data_path)

class_solution <- hbsc_labels$ClassSolution
hbsc_variables <- read.csv("C:/Users/hanst/OneDrive - Universität Zürich UZH/Datenanalyse/hbsc_variables.csv")
hbsc_variables <- tibble::rowid_to_column(hbsc_variables, "ID")

# Initialize empty list to store data frames
df_list <- list()

# Get unique countries from hbsc_labels
unique_countries <- unique(hbsc_labels$Country)

# Loop through each country
for (current_country in unique_countries) {
  
  # Get ClassSolution for this country
  class_solution <- hbsc_labels %>%
    filter(Country == current_country) %>%
    pull(ClassSolution) %>%
    unique()
  
  # Construct filepath
  filepath <- file.path(
    Sys.getenv("USERPROFILE"), 
    "OneDrive - Universität Zürich UZH", 
    "Datenanalyse",
    "LPA ID",
    current_country, 
    paste0("c_prob_", current_country, "_", class_solution, ".csv")
  )
  
  # Check if file exists
  if (file.exists(filepath)) {
    
    # Read the CSV file
    c_prob_data <- read.csv(filepath)
    
    # Get relevant variables from hbsc_variables for this country
    hbsc_subset <- hbsc_variables %>%
      filter(countryname == current_country) %>%
      select(ID, sex, age, fas, lifesat, ache, feeling, health, countryname, surveyyear)
    
    # Merge data frames by ID
    merged_data <- c_prob_data %>%
      inner_join(hbsc_subset, by = "ID") %>%
      select(ID, countryname, surveyyear, sex, age, fas, lifesat, ache, feeling, health, PHY, SLE, UND, SM, ALC, C)
    
    # Add to list
    df_list[[paste(current_country, sep = "_")]] <- merged_data
    
    cat("Processed:", current_country, "\n")
    
  } else {
    cat("File not found:", filepath, "\n")
  }
}

# Combine all data frames into one
multi_lvl_data_ovr <- bind_rows(df_list)

### Multilevel indices

## HDR containing Gender Inequality Index and Human Development Index
hdr <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\HDR25_Composite_indices_complete_time_series.csv)") %>%
  select(country, hdi_2002, hdi_2006, hdi_2010, hdi_2014, hdi_2018,
         ihdi_2010, ihdi_2014, ihdi_2018, 
         gii_2002, gii_2006, gii_2010, gii_2014, gii_2018) %>%
  pivot_longer(
    cols = -country,
    names_to = c(".value", "year"),
    names_pattern = "(.+)_(\\d{4})"
  )

hdr <- hdr %>%
  mutate(year = as.integer(year))

# Rename countries before merging
multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Moldova" ~ "Moldova (Republic of)",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  left_join(hdr, 
            by = c("countryname_merge" = "country", 
                   "surveyyear" = "year"))

## GINI
gini <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\swiid9_9_summary.csv)")

multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  select(-countryname_merge) %>%        # unselect previous country merging column
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Czechia" ~ "Czech Republic",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  left_join(gini %>% select(country, year, gini_disp),
            by = c("countryname_merge" = "country",
                   "surveyyear" = "year"))

## GDP
gdp <- read.csv(r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\Indices Data\API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5354.csv)", skip = 4) %>% 
  select(Country.Name, X2002, X2006, X2010, X2014, X2018) %>%
  pivot_longer(
    cols = -Country.Name,
    names_prefix = "X",                # remove the "X" prefix from column names. Year columns start with "X" because the column names would be numerical otherwise
    names_to = "year",
    values_to = "gdp"
  )

gdp <- gdp %>% 
  mutate(year = as.integer(year))

multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  select(-countryname_merge) %>%        # unselect previous country merging column
  mutate(countryname_merge = case_when(
    countryname %in% c("Belgium Flemish", "Belgium French") ~ "Belgium",
    countryname %in% c("England", "Scotland", "Wales") ~ "United Kingdom",
    countryname == "Slovakia" ~ "Slovak Republic",
    countryname == "United States of America" ~ "United States",
    TRUE ~ countryname  # keep all other names as they are
  ))

multi_lvl_data_ovr <- multi_lvl_data_ovr %>%
  left_join(gdp,
            by = c("countryname_merge" = "Country.Name",
                   "surveyyear" = "year")) %>% 
  select(-countryname_merge)

## Manually clean labels for merger with multi level data

# Load labels
data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_labels.xlsx")
hbsc_labels <- read_excel(data_path)

# # Check unique labels in each column
# for(i in 3:(ncol(hbsc_labels)-1)) {
#   print(colnames(hbsc_labels)[i])
#   print(table(hbsc_labels[,i], useNA = "ifany"))
# }

# Pivot longer per survey year

labels_ovr <- hbsc_labels %>%
  pivot_longer(
    cols = matches("Overall_Profile_\\d+"),  # Select columns matching the pattern YYYY_Profile_N
    names_to = "original_column",
    values_to = "Label"
  ) %>%
  mutate(
    Class = as.integer(sub("Overall_Profile_(\\d+)", "\\1", original_column))
  ) %>%
  select(-c(original_column, matches("(\\d{4})_Profile_\\d+"))) %>%  # Remove the temporary column and profile labels from per survey year data set
  filter(!is.na(Label))

# Merge labels to multi level analysis data
multi_lvl_data_ovr <- multi_lvl_data_ovr %>% 
  left_join(labels_ovr,
            by = c("countryname" = "Country",
                   "C" = "Class"))
multi_lvl_data_ovr <- multi_lvl_data_ovr %>% 
  select(ID, countryname, surveyyear, ClassSolution, sex, age, fas, lifesat, ache, feeling, health, PHY, SLE, UND, SM, ALC, C, Label, hdi, ihdi, gii, gini_disp, gdp)

# Write .csv file
write.csv(multi_lvl_data_ovr, "hbsc_mlvl_data_ovr.csv")