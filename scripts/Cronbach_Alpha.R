### Cronbach alpha calculation

# Load required packages
library(dplyr)
library(tidyr)
library(psych)
library(openxlsx)

data_dir  = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse")
data_path = file.path(data_dir, "hbsc_raw.csv")
output_dir = file.path(data_dir, "Cronbach Alpha")
dir.create(output_dir, showWarnings = FALSE)

hbsc_alpha <- read.csv(data_path, header=TRUE)

# Exclude countries
hbsc_alpha <- hbsc_alpha %>%
  filter(!countryname %in% c("Turkey", "Russia", "Kazakhstan"))

# Define your variables and their items
variables <- list(
  # "Alcohol" = c("beer_rev", "wine_rev", "spirits_rev", "alc30d_2_rs"),
  # "Smoking" = c("smoking_rev", "smok30d_2_rs"),
  # "Physical Inactivity" = "physinact",
  # "Sleep Problems" = "sleepprob",
  # "Unhealthy Diet" = c("fruits3r", "vegetables3r", "sweets3", "softdrinks3"),
  "Internalizing Symptoms" = c("dizzy_rev", "irritable_rev", "nervous_rev", "feellow_rev"),
  "Somatic Symptoms" = c("backache_rev", "headache_rev", "stomachache_rev"),
  "Life Satisfaction" = c("lifesat")
)

# Define styles once (reused across all country workbooks)
header_style    <- createStyle(fontName = "Arial", fontSize = 11, halign = "center",
                               textDecoration = "bold", border = "Bottom")
subheader_style <- createStyle(fontName = "Arial", fontSize = 11, halign = "center",
                               textDecoration = "bold")
data_style      <- createStyle(fontName = "Arial", fontSize = 11, halign = "center")
var_style       <- createStyle(fontName = "Arial", fontSize = 11, halign = "left")

# Iterate over each country
countries <- sort(unique(hbsc_alpha$countryname))

for (country in countries) {
  cat("\nProcessing:", country, "\n")

  country_data <- hbsc_alpha %>% filter(countryname == country)
  survey_years <- sort(unique(country_data$surveyyear))

  # --- Calculate alpha per variable per year ---
  results_by_year <- data.frame()

  for (year in survey_years) {
    year_data <- country_data[country_data$surveyyear == year, ]

    for (var_name in names(variables)) {
      items  <- variables[[var_name]]
      n_items <- length(items)

      if (n_items == 1) {
        n_non_na <- sum(!is.na(year_data[, items]))
        results_by_year <- rbind(results_by_year, data.frame(
          SurveyYear = year,
          Variable   = var_name,
          Items      = as.integer(n_non_na > 0),
          N          = n_non_na,
          Alpha      = "N/A"
        ))
      } else {
        item_data       <- year_data[, items]
        all_na          <- apply(item_data, 2, function(x) all(is.na(x)))
        valid_items     <- items[!all_na]
        n_valid_items   <- length(valid_items)
        complete_cases  <- sum(complete.cases(item_data))

        if (n_valid_items < 2) {
          results_by_year <- rbind(results_by_year, data.frame(
            SurveyYear = year,
            Variable   = var_name,
            Items      = n_valid_items,
            N          = complete_cases,
            Alpha      = "No data"
          ))
        } else {
          valid_item_data <- year_data[, valid_items]
          alpha_result <- tryCatch({
            suppressWarnings(alpha(valid_item_data, check.keys = TRUE))
          }, error = function(e) NULL)

          if (is.null(alpha_result)) {
            results_by_year <- rbind(results_by_year, data.frame(
              SurveyYear = year,
              Variable   = var_name,
              Items      = n_valid_items,
              N          = complete_cases,
              Alpha      = "Error"
            ))
          } else {
            results_by_year <- rbind(results_by_year, data.frame(
              SurveyYear = year,
              Variable   = var_name,
              Items      = n_valid_items,
              N          = sum(complete.cases(valid_item_data)),
              Alpha      = sprintf("%.3f", alpha_result$total$raw_alpha)
            ))
          }
        }
      }
    }
  }

  # --- Build wide format ---
  results_wide <- results_by_year %>%
    select(SurveyYear, Variable, Items, Alpha) %>%
    pivot_wider(names_from  = SurveyYear,
                values_from = c(Items, Alpha),
                names_glue  = "{SurveyYear}_{.value}")

  year_cols_ordered <- as.character(sort(unique(results_by_year$SurveyYear)))
  col_order <- c("Variable", unlist(lapply(year_cols_ordered, function(y) {
    c(paste0(y, "_Items"), paste0(y, "_Alpha"))
  })))
  results_wide <- results_wide[, col_order]

  # --- Export Excel ---
  wb <- createWorkbook()
  addWorksheet(wb, "Cronbach Alpha")

  writeData(wb, "Cronbach Alpha", country, startRow = 1, startCol = 1)
  addStyle(wb, "Cronbach Alpha", subheader_style, rows = 1, cols = 1)

  writeData(wb, "Cronbach Alpha", "Variable", startRow = 2, startCol = 1)
  addStyle(wb, "Cronbach Alpha", subheader_style, rows = 2, cols = 1)

  for (i in seq_along(year_cols_ordered)) {
    year_label <- year_cols_ordered[i]
    items_col  <- (i - 1) * 2 + 2
    alpha_col  <- items_col + 1

    writeData(wb, "Cronbach Alpha", year_label, startRow = 1, startCol = items_col)
    mergeCells(wb, "Cronbach Alpha", cols = items_col:alpha_col, rows = 1)
    addStyle(wb, "Cronbach Alpha", header_style, rows = 1, cols = items_col)

    writeData(wb, "Cronbach Alpha", "Items", startRow = 2, startCol = items_col)
    writeData(wb, "Cronbach Alpha", "Alpha", startRow = 2, startCol = alpha_col)
    addStyle(wb, "Cronbach Alpha", subheader_style, rows = 2, cols = items_col)
    addStyle(wb, "Cronbach Alpha", subheader_style, rows = 2, cols = alpha_col)

    items_vals <- results_wide[[paste0(year_label, "_Items")]]
    alpha_vals <- results_wide[[paste0(year_label, "_Alpha")]]
    writeData(wb, "Cronbach Alpha", data.frame(items_vals), startRow = 3,
              startCol = items_col, colNames = FALSE)
    writeData(wb, "Cronbach Alpha", data.frame(alpha_vals), startRow = 3,
              startCol = alpha_col, colNames = FALSE)
    addStyle(wb, "Cronbach Alpha", data_style, rows = 3:(2 + nrow(results_wide)),
             cols = items_col, gridExpand = TRUE)
    addStyle(wb, "Cronbach Alpha", data_style, rows = 3:(2 + nrow(results_wide)),
             cols = alpha_col, gridExpand = TRUE)
  }

  writeData(wb, "Cronbach Alpha", data.frame(results_wide$Variable),
            startRow = 3, startCol = 1, colNames = FALSE)
  addStyle(wb, "Cronbach Alpha", var_style, rows = 3:(2 + nrow(results_wide)),
           cols = 1, gridExpand = TRUE)

  setColWidths(wb, "Cronbach Alpha", cols = 1, widths = 25)
  setColWidths(wb, "Cronbach Alpha", cols = 2:(1 + length(year_cols_ordered) * 2), widths = 9)

  country_filename <- paste0("hbsc_alpha_", gsub("[^a-zA-Z0-9]", "_", tolower(country)), ".xlsx")
  saveWorkbook(wb, file.path(output_dir, country_filename), overwrite = TRUE)
  cat("  Saved:", country_filename, "\n")
}
