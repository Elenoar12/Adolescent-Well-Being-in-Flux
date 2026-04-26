##### Data merge HBSC

library(tidyverse)
library(lavaan)
library(haven)
library(psych)
library(skimr)
library(corrplot)
library(glue)
library(readxl)

### Read in data

# NOTE: The raw HBSC data files (.sav) are not included in this repository.
# Access to the data must be requested from the HBSC Data Management Centre.
# Once obtained, place the files in the working directory and update the paths below.
hbsc01 <- read_sav("../data/HBSC2001OAed1.0_F4.sav")
hbsc06 <- read_sav("../data/HBSC2006OAed1.0_F1.sav")
hbsc10 <- read_sav("../data/HBSC2010OAed1.0_F4.sav")
hbsc14 <- read_sav("../data/HBSC2014OAed1.1_F1.sav")
hbsc18 <- read_sav("../data/HBSC2018OAed1.1.sav")

### Rename
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

### Merge
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

write.csv(hbsc_raw, "data/hbsc_raw.csv")

##### Definitive data file with only relevant HBs and aggregated variables

# hbsc_def <- hbsc_allrel %>%
#   select(surveyyear,countryno,countryname,age,
#          agecat,
#          bodyheight,
#          bodyweight,
#          countryborn,
#          countrybornfa,
#          countrybornmo,
#          sex,
#          health,
#          toothbr,
#          studaccept,
#          studhelpful,
#          studtogether,
#          talkfather,
#          talkmother,
#          thinkbody,
#          talkstepfa,
#          talkstepmo,
#          agesex,
#          mbmi,
#          welloff,
#          grade,
#          physinact, sleepprob, riskysex, undiet, smoking2, alc2, lifesat, feeling, ache, fas)
# 
# hbsc_def <- hbsc_def %>%
#   rename(smoking = smoking2,
#          alcohol = alc2)
# 
# hbsc_allrelCH <- subset(hbsc_def, countryno == 756000)
# 
# write_csv(hbsc_def, "data/hbsc_allrel.csv")
# 
# write_csv(hbsc_allrelCH, "data/hbsc_allrelCH.csv")

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

write_csv(hbsc_def, "data/hbsc_variables.csv")


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
base_lpa_path <- "data/LPA"
data_path = "data/hbsc_labels.xlsx"
hbsc_labels <- read_excel(data_path)

# Run the batch processing with hbsc_labels
batch_process_mplus_files(base_lpa_path, hbsc_labels)

### Enum summary batch processing for class selection based on threshold (ClassSizes_n_Percent >= 2.0)

library(readxl)
library(openxlsx)

base_enum_path <- "data/LPA"

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

write.xlsx(class_selection, "data/class_selection.xlsx")


### Profile distribution consistency check
# Function to check profile consistency for HBSC Switzerland data
check_hbsc_profile_consistency <- function() {
  
  # Load the data
  data_path <- "data/hbsc_variables.csv"
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
file_path <- "data/class_selection.xlsx"
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
    file_path <- file.path("data/LPA",
                           country,
                           year,
                           paste0(tolower(country), "_", year, "_", tolower(class_num), ".out"))
  } else {
    file_path <- file.path("data/LPA",
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
  output_file <- "data/AI_Profile_VarMeans_Compilation.csv"
  
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

### Multilevel analysis data processing (per survey year basis)

library(readxl)

data_path = "data/hbsc_labels.xlsx"
hbsc_labels <- read_excel(data_path)

class_solution <- hbsc_labels$ClassSolution
hbsc_variables <- read.csv("data/hbsc_variables.csv")
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
      "data/LPA",
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
hdr <- read.csv("data/Indices Data/HDR25_Composite_indices_complete_time_series.csv") %>%
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
gini <- read.csv("data/Indices Data/swiid9_9_summary.csv")

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
gdp <- read.csv("data/Indices Data/API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5354.csv", skip = 4) %>% 
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
data_path = "data/hbsc_labels.xlsx"
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
write.csv(multi_lvl_data_sy, "data/hbsc_mlvl_data_per_sy.csv")

### Multilevel analysis data processing (on overall basis)

data_path = "data/hbsc_labels.xlsx"
hbsc_labels <- read_excel(data_path)

class_solution <- hbsc_labels$ClassSolution
hbsc_variables <- read.csv("data/hbsc_variables.csv")
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
    "data/LPA",
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
hdr <- read.csv("data/Indices Data/HDR25_Composite_indices_complete_time_series.csv") %>%
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
gini <- read.csv("data/Indices Data/swiid9_9_summary.csv")

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
gdp <- read.csv("data/Indices Data/API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5354.csv", skip = 4) %>% 
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
data_path = "data/hbsc_labels.xlsx"
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
write.csv(multi_lvl_data_ovr, "data/hbsc_mlvl_data_ovr.csv")