### Cronbach alpha calculation

# Load required packages
library(dplyr)
library(tidyr)
library(psych)

data_dir  = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse")
data_path = file.path(data_dir, "hbsc_raw.csv")
output_dir = file.path(data_dir, "Cronbach Alpha")
dir.create(output_dir, showWarnings = FALSE)

hbsc_alpha <- read.csv(data_path, header=TRUE)
hbsc_alpha_CH <- hbsc_alpha %>% filter(countryname == 'Switzerland')


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
write.csv(results_wide, file.path(output_dir, "hbsc_alpha_by_year_wide.csv"), row.names = FALSE)

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
      item_data <- year_data[, items]
      valid_items <- items[!apply(item_data, 2, function(x) all(is.na(x)))]

      cat("\n", var_name, "\n")
      cat(rep("-", 50), "\n", sep = "")

      if (length(valid_items) < 2) {
        cat("Insufficient valid items for this year.\n")
      } else {
        alpha_result <- tryCatch({
          suppressWarnings(alpha(year_data[, valid_items], check.keys = TRUE))
        }, error = function(e) {
          cat("Error:", conditionMessage(e), "\n")
          NULL
        })
        if (!is.null(alpha_result)) print(alpha_result)
      }
      cat("\n")
    }
  }
}

# Optional: Get detailed output for each multi-item scale
cat("\n\n=== DETAILED ALPHA ANALYSIS ===\n\n")

for (var_name in names(variables)) {
  items <- variables[[var_name]]

  if (length(items) > 1) {
    item_data <- hbsc_alpha_CH[, items]
    valid_items <- items[!apply(item_data, 2, function(x) all(is.na(x)))]

    cat("\n", rep("=", 60), "\n", sep = "")
    cat(var_name, "\n")
    cat(rep("=", 60), "\n", sep = "")

    if (length(valid_items) < 2) {
      cat("Insufficient valid items.\n")
    } else {
      alpha_result <- tryCatch({
        suppressWarnings(alpha(hbsc_alpha_CH[, valid_items], check.keys = TRUE))
      }, error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
        NULL
      })
      if (!is.null(alpha_result)) print(alpha_result)
    }
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
