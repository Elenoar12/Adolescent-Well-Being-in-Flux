library(gt)
library(tidyverse)
library(dplyr)
library(haven)
library(stargazer)
library(writexl)
library(MplusAutomation)
library(glue)

df <- read.csv("data/hbsc_variables.csv", header=TRUE)

# Shorten the variable names for Mplus
df <- df %>%
  rename(
    phy = physinact,
    sle = sleepprob,
    und = undiet,
    sm = smoking,
    alc = alcohol
  )

df <- tibble::rowid_to_column(df, "ID")

# Function to extract the lowest and highest diagonal classification probabilities
extract_classification_probabilities <- function(output) {
  if (!is.null(output$class_counts$classificationProbs.mostLikely)) {
    class_probs <- output$class_counts$classificationProbs.mostLikely
    
    # Extract diagonal elements (classification probabilities for the most likely class)
    diag_probs <- diag(as.matrix(class_probs))
    
    # Find the minimum and maximum probabilities on the diagonal
    min_prob <- min(diag_probs)
    max_prob <- max(diag_probs)
    
    # Return the range formatted as "Min - Max"
    prob_range <- paste0(round(min_prob, 4), " - ", round(max_prob, 4))
    return(prob_range)
  } else {
    return(NA)
  }
}

# Function to extract class sizes and proportions from Mplus output
extract_class_sizes_proportions <- function(output) {
  if (!is.null(output$class_counts$modelEstimated)) {
    class_sizes <- output$class_counts$modelEstimated$latentClassCounts
    class_proportions <- output$class_counts$modelEstimated$proportion
    
    # Format sizes and proportions as "class_size (percentage%)"
    formatted_classes <- paste0(class_sizes, " (", round(class_proportions * 100, 1), "%)")
    
    # Combine all class sizes and proportions into a single string
    combined_info <- paste(formatted_classes, collapse = ", ")
    return(combined_info)
  } else {
    return(NA)
  }
}

################################################################################

### Specify data for LPA
lpa_data <- df

### Define model syntax (adjusted from original analysis function)
# Uncomment original function generate_model_syntax() above for class-specific inputs
model_syntax <- "%OVERALL%\n
  [phy sle und sm alc];\n
  phy sle und sm alc;\n"

### For z-standardization (change to FALSE if not used)
z_standard <-  TRUE
pred_vars <- c("phy", "sle", "und", "sm", "alc")
# Process data - standardize if requested
z_standardize <- function(data, standardize = FALSE) {
  if (standardize == TRUE) {
    data %>% 
      group_by(countryname) %>% 
      mutate(across(all_of(pred_vars),
                    ~ scale(.) %>% 
                      as.vector())) %>%
      ungroup()
  } else {
    # Return original data if standardization not requested
    data
  }
}

################################################################################

# Uses data with or without z-standardization depending on toggle above
lpa_data <- z_standardize(lpa_data, z_standard)

# Shorten / Clean countrynames to avoid 90-character limit or () issues with Mplus
lpa_data <- lpa_data %>%
  mutate(countryname = recode(countryname,
                              "Belgium (Flemish)" = "Belgium Flemish",
                              "Belgium (French)"  = "Belgium French"
 ))

excluded_countries <- c("Austria", "Belgium Flemish", "Belgium French", "Canada", "Croatia", 
                        "Czechia", "Denmark", "Estonia", "Finland", "France", 
                        "Germany", "Greenland", "Hungary")

lpa_data <- lpa_data %>% filter(!countryname %in% excluded_countries)

country_names = unique(lpa_data$countryname)
# country_names <- c("Spain", "Greenland", "Turkey", "Belgium French", "North Macedonia")


# Create LPA results directory or different directory. Keep name short to avoid 90-character limit issue
LPA_dir <- "LPA"
if (!dir.exists(LPA_dir)) {
  dir.create(LPA_dir)
}

for (country in country_names) {
  # Base directory for the country
  country_dir <- file.path(LPA_dir, country)
  dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Define specific .xlsx file name
  file_name <- glue("{country}_enum_summary.xlsx")
  file_path <- file.path(country_dir, file_name)
  
  # if (file.exists(file_path)) {
  #   cat("File already exists, skipping:", file_path, "\n")
  #   next  # Skip this iteration if the file already exists
  # }
  
  # Run the analysis for each class from 1 to 6 in Mplus
  lpa <- lapply(1:6, function(k) {
    
    lpa_enum <- mplusObject(
      TITLE = glue("{country}_C{k};"),
      VARIABLE = glue("USEVAR = phy sle und sm alc;
                       CLASSES = C({k});
                       IDVARIABLE IS ID;"),
      ANALYSIS = "ESTIMATOR = MLR;
                TYPE = MIXTURE;
                STARTS = 2000 500;
                STITERATIONS = 100;",
      MODEL = model_syntax,
      OUTPUT = "TECH11 TECH14;",
      SAVEDATA = glue("FILE = CPROB_{country}_C{k}.dat;
                   SAVE = CPROB;"),
      PLOT = "TYPE = PLOT3;
            SERIES = phy(1) sle(2) und(3) sm(4) alc(5);",
      usevariables = c("ID", "phy", "sle", "und", "sm", "alc"),
      rdata = lpa_data %>% filter(countryname == country)
    )
    
    # Specify output paths directly in the temporary output directory
    dataout <- file.path(country_dir, glue("{country}_C{k}.dat"))
    modelout <- file.path(country_dir, glue("{country}_C{k}.inp"))
    
    tryCatch({
      lpa_enum_fit <- mplusModeler(lpa_enum,
                                   dataout = dataout,
                                   modelout = modelout,
                                   check = TRUE,
                                   run = TRUE,
                                   hashfilename = FALSE,
                                   writeData = 'always')
      
      # Extract c_prob and write to CSV
      c_prob <- readModels(file.path(country_dir, glue("{country}_C{k}.out")), quiet = TRUE)$savedata
      write.csv(c_prob, file = file.path(country_dir, glue("c_prob_{country}_C{k}.csv")))
      
      return(lpa_enum_fit)
    }, error = function(e) {
      message(glue("Error in model {k}: ", e$message))
      return(NULL)
    })
  })
  
  # Read the Mplus output directory
  output_enum <- readModels(country_dir, quiet = TRUE)
  
  # Generate the summary table for the LPA models
  enum_summary <- LatexSummaryTable(
    output_enum,
    keepCols = c("Title", "LL", "BIC", "aBIC", "AIC", "Entropy", "BLRT_PValue", "T11_VLMR_PValue", "T11_LMR_PValue", "Observations"),
    sortBy = "Title"
  )
  
  # Extract classification probabilities (range of lowest and highest) for each model
  classification_probs_list <- sapply(output_enum, extract_classification_probabilities)
  
  # Extract class sizes and proportions for each model
  class_sizes_proportions_list <- sapply(output_enum, extract_class_sizes_proportions)
  
  # Add classification probabilities (range of lowest and highest) and class sizes/proportions to the summary table
  enum_summary <- cbind(enum_summary, ClassificationProbRange = classification_probs_list, ClassSizes_n_Percent = class_sizes_proportions_list)
  
  # Display the summary as a table using the `gt` package
  enum_summary %>%
    gt() %>%
    tab_header(title = "LPA summary")
  
  # Convert the enum_summary to a data frame
  enum_summary_df <- as.data.frame(enum_summary)
  
  # Write the data frame to an Excel file
  write_xlsx(enum_summary_df, file_path)
  
  # Loop through country-specific survey years
  country_data <- lpa_data[lpa_data$countryname == country, ]
  
  for (year in unique(country_data$surveyyear)) {
    # Create survey year directory
    year_dir <- file.path(country_dir, as.character(year))
    dir.create(year_dir, showWarnings = FALSE)
    
    # Define specific .xlsx file name
    file_name <- glue("{country}_{year}_enum_summary.xlsx")
    file_path <- file.path(year_dir, file_name)
    
    # if (file.exists(file_path)) {
    #   cat("File already exists, skipping:", file_path, "\n")
    #   next  # Skip this iteration if the file already exists
    # }
    
    # Run the analysis for each class from 1 to 6 in Mplus
    lpa <- lapply(1:6, function(k) {
      #model_syntax <- generate_model_syntax(k)

      lpa_enum <- mplusObject(
        TITLE = glue("{country}_{year}_C{k};"),
        VARIABLE = glue("USEVAR = phy sle und sm alc;
                         CLASSES = C({k});
                         IDVARIABLE IS ID;"),
        ANALYSIS = "ESTIMATOR = MLR;
                TYPE = MIXTURE;
                STARTS = 2000 500;
                STITERATIONS = 100;",
        MODEL = model_syntax,
        OUTPUT = "TECH11 TECH14;",
        SAVEDATA = glue("FILE = CPROB_{country}_{year}_C{k}.dat;
                   SAVE = CPROB;"),
        PLOT = "TYPE = PLOT3;
            SERIES = phy(1) sle(2) und(3) sm(4) alc(5);",
        usevariables = c("ID", "phy", "sle", "und", "sm", "alc"),
        rdata = lpa_data %>% filter(countryname == country & surveyyear == year)
      )

      # Specify output paths directly in the temporary output directory
      dataout <- file.path(year_dir, glue("{country}_{year}_C{k}.dat"))
      modelout <- file.path(year_dir, glue("{country}_{year}_C{k}.inp"))

      tryCatch({
        lpa_enum_fit <- mplusModeler(lpa_enum,
                                     dataout = dataout,
                                     modelout = modelout,
                                     check = TRUE,
                                     run = TRUE,
                                     hashfilename = FALSE,
                                     writeData = 'always')
        
        # Extract c_prob for each surveyyear and write to CSV
        c_prob <- readModels(file.path(year_dir, glue("{country}_{year}_C{k}.out")), quiet = TRUE)$savedata
        write.csv(c_prob, file = file.path(year_dir, glue("c_prob_{country}_{year}_C{k}.csv")))
        
        return(lpa_enum_fit)
      }, error = function(e) {
        message(glue("Error in model {k}: ", e$message))
        return(NULL)
      })
    })
    
    # Read the Mplus output directory
    output_enum <- readModels(year_dir, quiet = TRUE)
    
    # Generate the summary table for the LPA models
    enum_summary <- LatexSummaryTable(
      output_enum,
      keepCols = c("Title", "LL", "BIC", "aBIC", "AIC", "Entropy", "BLRT_PValue", "T11_VLMR_PValue", "T11_LMR_PValue", "Observations"),
      sortBy = "Title"
    )
    
    # Extract classification probabilities (range of lowest and highest) for each model
    classification_probs_list <- sapply(output_enum, extract_classification_probabilities)
    
    # Extract class sizes and proportions for each model
    class_sizes_proportions_list <- sapply(output_enum, extract_class_sizes_proportions)
    
    # Add classification probabilities (range of lowest and highest) and class sizes/proportions to the summary table
    enum_summary <- cbind(enum_summary, ClassificationProbRange = classification_probs_list, ClassSizes_n_Percent = class_sizes_proportions_list)
    
    # Display the summary as a table using the `gt` package
    enum_summary %>%
      gt() %>%
      tab_header(title = "LPA summary")
    
    # Convert the enum_summary to a data frame
    enum_summary_df <- as.data.frame(enum_summary)
    
    # Write the data frame to an Excel file
    write_xlsx(enum_summary_df, file_path)
    
    ### Old section with age category loop
    
    # # Loop through age categories
    # for (age_cat in unique(lpa_data$agecat)) {
    #   # Create age category directory
    #   age_dir <- file.path(year_dir, paste0("AgeCat_", age_cat))
    #   dir.create(age_dir, showWarnings = FALSE)
    #   
    #   # Define specific .xlsx file name
    #   file_name <- glue("{country}_{year}_{age_cat}_enum_summary.xlsx")
    #   file_path <- file.path(age_dir, file_name)
    #   
    #   if (file.exists(file_path)) {
    #     cat("File already exists, skipping:", file_path, "\n")
    #     next  # Skip this iteration if the file already exists
    #   }
    #   
    #   # Run the analysis for each class from 1 to 6 in Mplus
    #   lpa <- lapply(1:6, function(k) {
    #     #model_syntax <- generate_model_syntax(k)
    #     
    #     lpa_enum <- mplusObject(
    #       TITLE = glue("{country}_{year}_{age_cat}_C{k};"),
    #       VARIABLE = glue("USEVAR = phy sle und sm alc;
    #                  CLASSES = C({k});"
    #                  IDVARIABLE IS ID;"),
    #       ANALYSIS = "ESTIMATOR = MLR;
    #             TYPE = MIXTURE;
    #             STARTS = 2000 500;
    #             STITERATIONS = 100;",
    #       MODEL = model_syntax,
    #       OUTPUT = "TECH11 TECH14;",
    #       SAVEDATA = glue("FILE = CPROB_{country}_{year}_{age_cat}_C{k}.dat;
    #                        SAVE = CPROB;"),
    #       PLOT = "TYPE = PLOT3;
    #         SERIES = phy(1) sle(2) und(3) sm(4) alc(5);",
    #       usevariables = c("phy", "sle", "und", "sm", "alc"),
    #       rdata = lpa_data %>% filter(countryname == country & surveyyear == year & agecat == age_cat)
    #     )
    #     
    #     # Specify output paths directly in the temporary output directory
    #     dataout <- file.path(age_dir, glue("{country}_{year}_{age_cat}_C{k}.dat"))
    #     modelout <- file.path(age_dir, glue("{country}_{year}_{age_cat}_C{k}.inp"))
    #     
    #     tryCatch({
    #       lpa_enum_fit <- mplusModeler(lpa_enum,
    #                                    dataout = dataout,
    #                                    modelout = modelout,
    #                                    check = TRUE, 
    #                                    run = TRUE, 
    #                                    hashfilename = FALSE,
    #                                    writeData = 'always')
    #       return(lpa_enum_fit)
    #     }, error = function(e) {
    #       message(glue("Error in model {k}: ", e$message))
    #       return(NULL)
    #     })
    #   })
    #   
    #   # Read the Mplus output directory
    #   output_enum <- readModels(age_dir, quiet = TRUE)
    #   
    #   # Generate the summary table for the LPA models
    #   enum_summary <- LatexSummaryTable(
    #     output_enum,
    #     keepCols = c("Title", "LL", "BIC", "aBIC", "AIC", "Entropy", "BLRT_PValue", "T11_VLMR_PValue", "T11_LMR_PValue", "Observations"),
    #     sortBy = "Title"
    #   )
    #   
    #   # Extract classification probabilities (range of lowest and highest) for each model
    #   classification_probs_list <- sapply(output_enum, extract_classification_probabilities)
    #   
    #   # Extract class sizes and proportions for each model
    #   class_sizes_proportions_list <- sapply(output_enum, extract_class_sizes_proportions)
    #   
    #   # Add classification probabilities (range of lowest and highest) and class sizes/proportions to the summary table
    #   enum_summary <- cbind(enum_summary, ClassificationProbRange = classification_probs_list, ClassSizes_n_Percent = class_sizes_proportions_list)
    #   
    #   # Display the summary as a table using the `gt` package
    #   enum_summary %>%
    #     gt() %>%
    #     tab_header(title = "LPA summary")
    #   
    #   # Convert the enum_summary to a data frame
    #   enum_summary_df <- as.data.frame(enum_summary)
    #   
    #   # Write the data frame to an Excel file
    #   write_xlsx(enum_summary_df, file_path)
    # }
  }
}
