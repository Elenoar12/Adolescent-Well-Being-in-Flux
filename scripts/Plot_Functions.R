### Plot Functions
# Run from the project root (the folder containing app.R and data/).
#
# Required files:
#   data/hbsc_variables.csv   – main dataset
#   data/hbsc_raw.csv         – item-level data for histograms
#   data/hbsc_labels.xlsx     – country/profile labels
#   data/Regression/          – regression CSV outputs
#   data/LPA/                 – LPA CSV outputs
#
# Note: generate_map() produces an interactive Leaflet map.
#       Use mapshot() from the mapview package to save it as a static image.

# Set working directory to project root (folder containing app.R and data/)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(giscoR)
library(fmsb)
library(MplusAutomation)
library(reshape2)
library(readxl)

# Load data
data_path = "data/hbsc_variables.csv"
hbsc <- read.csv(data_path, header=TRUE)

# Exclude select countries from data set
hbsc <- hbsc %>% 
  filter(!countryname %in% c("Russia",      # See exclusion from HBSC project
                             "Turkey",      # Substance use was not surveyed
                             "Kazakhstan")) # LPA analysis resulted in only C1 solution

# Define Variables
pred_vars <- c("physinact", "sleepprob", "undiet", "smoking", "alcohol")

# Z-Standardized data per country (more than one survey year) before summary stats
survey_counts <- hbsc %>%
  group_by(countryname) %>%
  summarise(n_surveys = n_distinct(surveyyear))

z_hbsc <- hbsc %>%
  left_join(survey_counts, by = "countryname") %>%
  filter(n_surveys > 1) %>%
  group_by(countryname) %>%
  mutate(across(all_of(pred_vars),
                ~ scale(.) %>% as.vector())) %>%
  ungroup()

# Compute mean for each country and survey year
hbsc_mean <- z_hbsc %>%
  group_by(countryno, countryname, surveyyear) %>%
  summarise(across(all_of(pred_vars), mean, na.rm = TRUE), .groups = "drop")

# Load necessary world data
world <- ne_countries(scale= "medium", returnclass = "sf") %>%
  select(name, geometry)

# Merge `hbsc` with `world` spatial data
hbsc_map <- hbsc_mean %>%
  left_join(world, by = c("countryname" = "name"))

# Remove Belgium records from hbsc_map
hbsc_map <- hbsc_map %>%
  filter(!(countryname %in% c("Belgium Flemish", "Belgium French")))

# Find the records in hbsc_mean that need geometries
bel_records <- hbsc_mean %>%
  filter(countryname %in% c("Belgium Flemish", "Belgium French"))

# Get Belgium regions (Level 1: Flemish, Walloon, Brussels)
bel_regions <- gisco_get_nuts(year = "2021", nuts_level = 1, country = "BE") %>% 
  select(NAME_LATN, geometry)

# Extract the Flemish Region separately
bel_flem <- bel_regions %>% 
  filter(NAME_LATN == "Vlaams Gewest") %>% 
  mutate(NAME_LATN = "Belgium Flemish") 

# Merge Walloon & Brussels into one geometry "Belgium (French)"
bel_fren <- bel_regions %>%
  filter(NAME_LATN %in% c("Région wallonne", "Région de Bruxelles-Capitale/Brussels Hoofdstedelijk Gewest")) %>%
  summarise(NAME_LATN = "Belgium French")

# Combine the two adjusted geometries
bel_custom <- bind_rows(bel_flem, bel_fren) %>%
  # Rename column to match joining key in hbsc_map
  rename(countryname = NAME_LATN)

# Create complete Belgium records with geometries
bel_records <- bel_records %>%
  left_join(bel_custom, by = "countryname")

hbsc_map <- bind_rows(hbsc_map, bel_records)

# Get UK regions (Level 1: England, Scotland, Wales, Northern Ireland)
uk_regions <- gisco_get_nuts(year = "2021", nuts_level = 1, country = "UK") %>%
  select(NAME_LATN, geometry) %>% 
  rename(countryname = NAME_LATN)

# Create England by combining everything that's not Scotland, Wales or Northern Ireland
uk_england <- uk_regions %>%
  filter(!countryname %in% c("Scotland", "Wales", "Northern Ireland")) %>%
  summarise(countryname = "England")

uk_scotland <- uk_regions %>%
  filter(countryname == "Scotland")

uk_wales <- uk_regions %>%
  filter(countryname == "Wales")

# Combine the UK regions
uk_custom <- bind_rows(uk_england, uk_scotland, uk_wales)

# Remove UK regions from hbsc_map if they exist
hbsc_map <- hbsc_map %>%
  filter(!(countryname %in% c("England", "Scotland", "Wales")))

# Find the records in hbsc_mean that need geometries
uk_records <- hbsc_mean %>%
  filter(countryname %in% c("England", "Scotland", "Wales"))

# Add geometries to the UK records
uk_records <- uk_records %>%
  left_join(uk_custom, by = "countryname")

# Add the UK regions with geometries to hbsc_map
hbsc_map <- st_as_sf(bind_rows(hbsc_map, uk_records))

### Remove overseas territories from France and Netherlands

# Get metropolitan France (mainland + Corsica)
france_metro <- gisco_get_countries(year = "2024", resolution = "10", country = "FR") %>%
  # Break multipolygon into separate polygons
  st_cast("POLYGON") %>%
  # Calculate area of each polygon
  mutate(area = st_area(.)) %>%
  # Keep only the two largest polygons (mainland + Corsica)
  arrange(desc(area)) %>%
  slice(1,3) %>%
  # Recombine into multipolygon
  summarise(geometry = st_union(geometry)) %>%
  mutate(countryname = "France")

# For Netherlands: Remove overseas territories (keep only European part)
netherlands_metro <- gisco_get_countries(year = "2024", resolution = "10", country = "NL") %>%
  # Break multipolygon into separate polygons
  st_cast("POLYGON") %>%
  # Calculate area of each polygon
  mutate(area = st_area(.)) %>%
  # Keep only the largest polygon (European Netherlands)
  arrange(desc(area)) %>%
  slice(1) %>%
  # Recombine into multipolygon
  summarise(geometry = st_union(geometry)) %>%
  mutate(countryname = "Netherlands")

# Remove France and Netherlands from hbsc_map
hbsc_map <- hbsc_map %>%
  filter(!(countryname %in% c("France", "Netherlands")))

# Find the records in hbsc_mean that need geometries
fr_nl_records <- hbsc_mean %>%
  filter(countryname %in% c("France", "Netherlands"))

# Add geometries to the France and Netherlands records
fr_records <- fr_nl_records %>%
  filter(countryname == "France") %>%
  left_join(france_metro, by = "countryname")

nl_records <- fr_nl_records %>%
  filter(countryname == "Netherlands") %>%
  left_join(netherlands_metro, by = "countryname")

# Combine the records
fr_nl_records_with_geo <- bind_rows(fr_records, nl_records)

# Add the modified countries back to hbsc_map
hbsc_map <- st_as_sf(bind_rows(hbsc_map, fr_nl_records_with_geo))

### Add Crimea to Ukraine

# Get Ukraine geometry
ukraine <- gisco_get_countries(year = "2013", resolution = "10", country = "UA") %>%
  select(geometry) %>%
  mutate(countryname = "Ukraine")

# Remove Ukraine from hbsc_map if it exists
hbsc_map <- hbsc_map %>%
  filter(countryname != "Ukraine")

# Find Ukraine record in hbsc_mean
ukraine_record <- hbsc_mean %>%
  filter(countryname == "Ukraine")

# Join the Ukraine record with the 2013 geometry
ukraine_with_crimea <- ukraine_record %>%
  left_join(ukraine, by = "countryname")

# Add to hbsc_map
hbsc_map <- st_as_sf(bind_rows(hbsc_map, ukraine_with_crimea))

# Function to generate the Leaflet map for any variable
generate_map <- function(variable, year) {
  # Filter main hbsc data for selected year
  data_year <- z_hbsc %>% filter(surveyyear == year)
  
  # Compute per-country summary stats (lightweight table)
  country_stats <- data_year %>%
    group_by(countryname) %>%
    summarise(
      n_participants = n(),
      n_valid = sum(!is.na(.data[[variable]])),
      n_na = sum(is.na(.data[[variable]])),
      response_rate = round(n_valid / n_participants * 100, 1),
      sd = round(sd(.data[[variable]], na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  # Filter spatial data for year + non-missing values
  filtered_data <- hbsc_map %>%
    filter(surveyyear == year) %>%
    filter(!is.na(.data[[variable]])) %>%
    left_join(country_stats, by = "countryname")
  
  # Define manual legend values centered at 0.00 with ±0.10 steps
  legend_values <- c(-0.40, -0.30, -0.20, -0.10, 0.00, 0.10, 0.20, 0.30, 0.40)
  
  # Define color palette using manual legend values
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = legend_values,
    reverse = TRUE,
    na.color = "transparent"
  )
  
  # Clamp values to legend range for coloring
  filtered_data$clamped_values <- pmax(-0.40, pmin(0.40, filtered_data[[variable]]))
  
  # Force full range by adding extreme values to the data
  legend_range <- c(-0.40, 0.40, filtered_data$clamped_values)
  
  # Create Leaflet map
  leaflet(filtered_data,
          options = leafletOptions(
            minZoom = 3,
            maxZoom = 8,
            maxBounds = list(list(-90, -180), list(90, 180)),
            zoomControl = TRUE,
            zoomControlPosition = "topleft"
          )) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    setView(lng = -30, lat = 45, zoom = 3) %>%  # Updated view over Atlantic Ocean
    addPolygons(
      fillColor = ~pal(clamped_values),
      weight = 1,
      opacity = 0.6,
      color = "black",
      fillOpacity = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "blue",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~countryname,
      labelOptions = labelOptions(
        style = list("font-weight" = "bold", "color" = "black"),
        textsize = "14px",
        direction = "auto"
      ),
      popup = ~paste0(
        "<b>", countryname, "</b><br>",
        "Participants: ", n_participants, " (", response_rate, "%)<br>",
        "Missing Values: ", n_na, "<br>",
        variable, ": ", round(get(variable), 2), "<br>",
        "SD: ", sd
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = legend_range,
      title = paste("Average", variable, "z-Score"),
      
      opacity = 1
    )
}

### Integrated labelling process to save disk space
hbsc_raw <- read.csv("data/hbsc_raw.csv")

hbsc_mappings <- list(
  # Health complaints (backache_rev, headache_rev, stomachache_rev, dizzy_rev, irritable_rev, nervous_rev, feellow_rev)
  # These use formula 6 - x on a 1-5 scale, giving values 1-5
  health_complaints_rev = c("1" = "Rarely or never", "2" = "About every month",
                            "3" = "About every week", "4" = "More than once a week",
                            "5" = "About every day"),

  # Sleep problems: sleepprob = 5 - sleepdifficulty (1-5 scale), giving values 0-4
  sleep_problems = c("0" = "Rarely or never", "1" = "About every month",
                     "2" = "About every week", "3" = "More than once a week",
                     "4" = "About every day"),
  
  # Life satisfaction (0-10 scale)
  likert_lifesat = c("0" = "0 (Worst)", "1" = "1", "2" = "2", "3" = "3", "4" = "4", 
                     "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10 (Best)"),
  
  # Health status (not reversed)
  likert_health = c("1" = "Poor", "2" = "Fair", "3" = "Good", "4" = "Excellent"),
  
  # Family affluence score (fas)
  likert_famcar = c("1" = "No", "2" = "Yes, one", "3" = "Yes, two or more"),
  
  likert_bedroom = c("1" = "No", "2" = "Yes"),
  
  likert_computers = c("1" = "None", "2" = "1", "3" = "2", "4" = "More than two"),
  
  likert_holidays = c("1" = "Not at all", "2" = "Once", "3" = "Twice", "4" = "More than twice"),
  
  # Physical activity: physinact = 7 - physact60 (0-7 scale), giving values 0-7
  likert_physinact = c("0" = "7 days", "1" = "6 days", "2" = "5 days",
                       "3" = "4 days", "4" = "3 days", "5" = "2 days",
                       "6" = "1 day", "7" = "0 days"),
  # Unhealthy Diet (reverse-coded): fruits3r/vegetables3r = 7 - x (1-7 scale), giving values 0-6
  likert_undiet_r = c("0" = "Every day, more than once", "1" = "Once a day, every day",
                      "2" = "5-6 days a week", "3" = "2-4 days a week",
                      "4" = "Once a week", "5" = "Less than once a week", "6" = "Never"),
  likert_undiet = c("1" = "Never", "2" = "Less than once a week",
                    "3" = "Once a week", "4" = "2-4 days a week", 
                    "5" = "5-6 days a week", "6" = "Once a day, every day", 
                    "7" = "Every day, more than once"),
  
  # Alcohol
  likert_alcohol = c("1" = "Never", "2" = "Rarely", "3" = "Every month", 
                     "4" = "Every week", "5" = "Every day"),
  likert_alc30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                      "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)"),
  
  # Smoking
  lit_q_smoking = "How often do you smoke tobacco at present?",
  likert_smoking = c("1" = "Don't", "2" = "Less than once a week", 
                     "3" = "Once a week", "4" = "Every day"),
  
  likert_smok30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                       "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
)

# Apply health complaints mappings (backache_rev etc. use 6-x formula → values 1-5)
health_complaint_vars <- c("backache_rev", "headache_rev", "stomachache_rev",
                           "dizzy_rev", "irritable_rev", "nervous_rev", "feellow_rev")

for(var in health_complaint_vars) {
  new_var_name <- paste0(var, "_labeled")
  hbsc_raw[[new_var_name]] <- factor(hbsc_raw[[var]],
                                     levels = names(hbsc_mappings$health_complaints_rev),
                                     labels = hbsc_mappings$health_complaints_rev)
}

# Sleep problems uses a different formula (5 - sleepdifficulty → values 0-4)
hbsc_raw$sleepprob_labeled <- factor(hbsc_raw$sleepprob,
                                     levels = names(hbsc_mappings$sleep_problems),
                                     labels = hbsc_mappings$sleep_problems)

# Life satisfaction mapping
hbsc_raw$lifesat_labeled <- factor(hbsc_raw$lifesat,
                                   levels = names(hbsc_mappings$likert_lifesat),
                                   labels = hbsc_mappings$likert_lifesat)

# Health status mapping
hbsc_raw$health_labeled <- factor(hbsc_raw$health,
                                  levels = names(hbsc_mappings$likert_health),
                                  labels = hbsc_mappings$likert_health)

# Family Affluence Score mappings
hbsc_raw$famcar3_labeled <- factor(hbsc_raw$famcar3,
                                   levels = names(hbsc_mappings$likert_famcar),
                                   labels = hbsc_mappings$likert_famcar)

hbsc_raw$bedroom3_labeled <- factor(hbsc_raw$bedroom3,
                                    levels = names(hbsc_mappings$likert_bedroom),
                                    labels = hbsc_mappings$likert_bedroom)

hbsc_raw$computers3_labeled <- factor(hbsc_raw$computers3,
                                      levels = names(hbsc_mappings$likert_computers),
                                      labels = hbsc_mappings$likert_computers)

hbsc_raw$holidays3_labeled <- factor(hbsc_raw$holidays3,
                                     levels = names(hbsc_mappings$likert_holidays),
                                     labels = hbsc_mappings$likert_holidays)

# Physical inactivity mapping
hbsc_raw$physinact_labeled <- factor(hbsc_raw$physinact,
                                     levels = names(hbsc_mappings$likert_physinact),
                                     labels = hbsc_mappings$likert_physinact)

# Dietary behaviour mappings
# fruits3r and vegetables3r use reverse coding
hbsc_raw$fruits3r_labeled <- factor(hbsc_raw$fruits3r,
                                    levels = names(hbsc_mappings$likert_undiet_r),
                                    labels = hbsc_mappings$likert_undiet_r)

hbsc_raw$vegetables3r_labeled <- factor(hbsc_raw$vegetables3r,
                                        levels = names(hbsc_mappings$likert_undiet_r),
                                        labels = hbsc_mappings$likert_undiet_r)

# sweets3 and softdrinks3 use regular coding
hbsc_raw$sweets3_labeled <- factor(hbsc_raw$sweets3,
                                   levels = names(hbsc_mappings$likert_undiet),
                                   labels = hbsc_mappings$likert_undiet)

hbsc_raw$softdrinks3_labeled <- factor(hbsc_raw$softdrinks3,
                                       levels = names(hbsc_mappings$likert_undiet),
                                       labels = hbsc_mappings$likert_undiet)

# Alcohol mappings
alcohol_vars <- c("beer_rev", "wine_rev", "spirits_rev")
for(var in alcohol_vars) {
  new_var_name <- paste0(var, "_labeled")
  hbsc_raw[[new_var_name]] <- factor(hbsc_raw[[var]], 
                                     levels = names(hbsc_mappings$likert_alcohol),
                                     labels = hbsc_mappings$likert_alcohol)
}

# Alcohol 30-day mapping
hbsc_raw$alc30d_2_labeled <- factor(hbsc_raw$alc30d_2,
                                    levels = names(hbsc_mappings$likert_alc30d_2),
                                    labels = hbsc_mappings$likert_alc30d_2)

# Smoking mappings
hbsc_raw$smoking_rev_labeled <- factor(hbsc_raw$smoking_rev,
                                       levels = names(hbsc_mappings$likert_smoking),
                                       labels = hbsc_mappings$likert_smoking)

hbsc_raw$smok30d_2_labeled <- factor(hbsc_raw$smok30d_2,
                                     levels = names(hbsc_mappings$likert_smok30d_2),
                                     labels = hbsc_mappings$likert_smok30d_2)

hbsc_label <- hbsc_raw %>% 
  select(surveyyear,countryname,
         backache_rev_labeled, headache_rev_labeled, stomachache_rev_labeled,
         dizzy_rev_labeled, irritable_rev_labeled, nervous_rev_labeled, feellow_rev_labeled,
         health_labeled, lifesat_labeled,
         famcar3_labeled, bedroom3_labeled, computers3_labeled, holidays3_labeled,
         sleepprob_labeled,
         physinact_labeled,
         fruits3r_labeled, vegetables3r_labeled, sweets3_labeled, softdrinks3_labeled,
         beer_rev_labeled, wine_rev_labeled, spirits_rev_labeled, alc30d_2_labeled,
         smoking_rev_labeled, smok30d_2_labeled 
  )

# Clean up column names by removing suffixes
names(hbsc_label) <- names(hbsc_label) %>%
  str_remove("_.*labeled$") %>%        # Remove _*labeled (any characters followed by labeled)
  str_remove("_rev$") %>%              # Remove _rev suffix
  str_remove("3$")                     # Remove trailing 3

# Clean up the dataframe and rename specific columns
hbsc_label <- hbsc_label %>%
  rename(vegetables = vegetables3r,
         fruits = fruits3r)

### Plot histograms for variable selection
create_histogram <- function(filtered_data, variable_name, title, order) {
  
  # Check what values are actually in the data
  actual_values <- unique(filtered_data[[variable_name]])
  actual_values <- actual_values[!is.na(actual_values)]
  
  # Match actual values to the order vector instead of sorting alphabetically
  ordered_values <- intersect(order, actual_values)
  
  # Create histogram for a single variable with NA handling
  p <- filtered_data %>%
    mutate(!!variable_name := factor(.data[[variable_name]], 
                                     levels = ordered_values,
                                     exclude = NULL)) %>%
    ggplot(aes(x = .data[[variable_name]], fill = is.na(.data[[variable_name]]))) +
    geom_bar(color = "black", alpha = 0.7) +
    scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray"),
                      guide = "none") +  # Hide legend
    labs(
      title = title,
      subtitle = paste("Variable:", variable_name),
      x = variable_name,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),      
      plot.subtitle = element_text(size = 12),                  
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
      axis.text.y = element_text(size = 12),                    
      axis.title = element_text(size = 14, face = "bold"),      
      axis.title.x = element_text(margin = margin(t = 10)),     
      axis.title.y = element_text(margin = margin(r = 10)),     
      plot.margin = margin(20, 20, 20, 20)                     
    )
  
  return(p)
}

desc_histograms <- function(data, countryname = NULL, surveyyear = NULL, variable = NULL) {
  
  # Filter data
  filtered_data <- data
  
  if (!is.null(countryname)) {
    filtered_data <- filtered_data %>% filter(countryname %in% !!countryname)
  }
  
  if (!is.null(surveyyear)) {
    filtered_data <- filtered_data %>% filter(surveyyear %in% !!surveyyear)
  }
  
  # Initialize empty list to store plots
  plot_list <- list()
  
  # Create histograms based on variable selection
  ### Demographic variable
  if (variable == "fas") {
    
    # Define titles
    lit_q_famcar = "Does your family own a car, van or truck?"
    lit_q_bedroom = "Do you have your own bedroom for yourself?"
    lit_q_computers = "How many computers do your family own (from 2014: including laptops and tablets, not including game consoles and smartphones)?"
    lit_q_holidays_0210 = "During the past 12 months, how many times did you travel away on holiday [vacation] with your family?"
    lit_q_holidays_1418 = "How many times did you and your family travel out of [insert country here] for a holiday/vacation last year?"
    
    # Define orders
    likert_famcar = c("1" = "No", "2" = "Yes, one", "3" = "Yes, two or more")
    likert_bedroom = c("1" = "No", "2" = "Yes")
    likert_computers = c("1" = "None", "2" = "1", "3" = "2", "4" = "More than two")
    likert_holidays = c("1" = "Not at all", "2" = "Once", "3" = "Twice", "4" = "More than twice")
    
    # Determine holiday title based on survey year
    if (!is.null(surveyyear) && any(surveyyear %in% c(2014, 2018))) {
      holidays_title <- lit_q_holidays_1418
    } else {
      holidays_title <- lit_q_holidays_0210
    }
    
    # Call create_histogram for each FAS variable
    plot_list[["famcar"]] <- create_histogram(filtered_data, "famcar", lit_q_famcar, likert_famcar)
    plot_list[["bedroom"]] <- create_histogram(filtered_data, "bedroom", lit_q_bedroom, likert_bedroom)
    plot_list[["computers"]] <- create_histogram(filtered_data, "computers", lit_q_computers, likert_computers)
    plot_list[["holidays"]] <- create_histogram(filtered_data, "holidays", holidays_title, likert_holidays)
  }
  ### Outcome variables
  
  if (variable == "health"){
    # Define title
    lit_q_health = "Would you say your health is……?"
    # Define order
    likert_health = c("1" = "Poor", "2" = "Fair", "3" = "Good", "4" = "Excellent")
    
    plot_list[["health"]] <- create_histogram(filtered_data, "health", lit_q_health, likert_health)
  }
  
  if (variable == "lifesat"){
    # Define title
    lit_q_lifesat = "Here is a picture of a ladder. The top of the ladder '10' is the best possible life for you and the bottom '0' is the worst possible life for you.\nIn general, where on the ladder do you feel you stand at the moment?"
    # Define order
    likert_lifesat = c("0" = "0 (Worst)", "1" = "1", "2" = "2", "3" = "3", "4" = "4", 
                       "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10 (Best)")
    
    plot_list[["lifesat"]] <- create_histogram(filtered_data, "lifesat", lit_q_lifesat, likert_lifesat)
  }
  
  ### Health complaints (sleepprob, backache_rev, headache_rev, stomachache_rev, dizzy_rev, irritable_rev, nervous_rev, feellow_rev)
  # Define titles for Health complaints 
  lit_q_health_complaints = "In the last 6 months: how often have you had the following….?"
  # Define orders for Health complaints
  likert_health_complaints = c("1" = "Rarely or never", "2" = "About every month", 
                               "3" = "About every week", "4" = "More than once a week", 
                               "5" = "About every day")
  
  if (variable == "ache"){
    plot_list[["backache"]] <- create_histogram(filtered_data, "backache", lit_q_health_complaints, likert_health_complaints)
    plot_list[["headache"]] <- create_histogram(filtered_data, "headache", lit_q_health_complaints, likert_health_complaints)
    plot_list[["stomachache"]] <- create_histogram(filtered_data, "stomachache", lit_q_health_complaints, likert_health_complaints)
  }
  
  if (variable == "feeling"){
    plot_list[["dizzy"]] <- create_histogram(filtered_data, "dizzy", lit_q_health_complaints, likert_health_complaints)
    plot_list[["irritable"]] <- create_histogram(filtered_data, "irritable", lit_q_health_complaints, likert_health_complaints)
    plot_list[["nervous"]] <- create_histogram(filtered_data, "nervous", lit_q_health_complaints, likert_health_complaints)
    plot_list[["feellow"]] <- create_histogram(filtered_data, "feellow", lit_q_health_complaints, likert_health_complaints)
  }
  
  ### Predictor variables
  if (variable == "sleepprob"){
    plot_list[["sleepprob"]] <- create_histogram(filtered_data, "sleepprob", lit_q_health_complaints, likert_health_complaints)
  }
  
  if (variable == "undiet"){
    
    # Define titles
    lit_q_undiet = "How many times a week do you usually eat or drink .... ?"
    
    # Define orders
    likert_undiet_r = c("1" = "Every day, more than once", "2" = "Once a day, every day",
                        "3" = "5-6 days a week", "4" = "2-4 days a week", 
                        "5" = "Once a week", "6" = "Less than once a week", "7" = "Never")
    likert_undiet = c("1" = "Never", "2" = "Less than once a week",
                      "3" = "Once a week", "4" = "2-4 days a week", 
                      "5" = "5-6 days a week", "6" = "Once a day, every day", 
                      "7" = "Every day, more than once")
    
    # Call create_histogram for each undiet variable
    plot_list[["sweets"]] <- create_histogram(filtered_data, "sweets", lit_q_undiet, likert_undiet)
    plot_list[["softdrinks"]] <- create_histogram(filtered_data, "softdrinks", lit_q_undiet, likert_undiet)
    plot_list[["vegetables"]] <- create_histogram(filtered_data, "vegetables", lit_q_undiet, likert_undiet_r)
    plot_list[["fruits"]] <- create_histogram(filtered_data, "fruits", lit_q_undiet, likert_undiet_r)
  }
  
  if (variable == "physinact"){
    
    # Define titles
    lit_q_physinact = "Over the past 7 days, on how many days were you physically active for a total of at least 60 minutes per day?"
    
    # Define orders
    likert_physinact = c("1" = "7 days", "2" = "6 days", "3" = "5 days", 
                         "4" = "4 days", "5" = "3 days", "6" = "2 days", 
                         "7" = "1 day", "8" = "0 days")
    
    plot_list[["physinact"]] <- create_histogram(filtered_data, "physinact", lit_q_physinact, likert_physinact)
  }
  
  if (variable == "alcohol"){
    
    # Define title
    lit_q_alcohol = "At present, how often do you drink anything alcoholic, such as beer, wine or spirits like…."
    
    # Define order
    likert_alcohol = c("1" = "Never", "2" = "Rarely", "3" = "Every month", 
                       "4" = "Every week", "5" = "Every day")
    
    if (surveyyear == 2014){
      # Plot all for 2014
      plot_list[["beer"]] <- create_histogram(filtered_data, "beer", lit_q_alcohol, likert_alcohol)
      plot_list[["wine"]] <- create_histogram(filtered_data, "wine", lit_q_alcohol, likert_alcohol)
      plot_list[["spirits"]] <- create_histogram(filtered_data, "spirits", lit_q_alcohol, likert_alcohol)
      
      lit_q_alc30d_2 = "On how many days (if any) have you drunk alcohol?"
      likert_alc30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                          "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["alc30d"]] <- create_histogram(filtered_data, "alc30d", lit_q_alc30d_2, likert_alc30d_2)
    } else if (surveyyear == 2018){
      # Only 30d for 2018
      lit_q_alc30d_2 = "On how many days (if any) have you drunk alcohol?"
      likert_alc30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                          "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["alc30d"]] <- create_histogram(filtered_data, "alc30d", lit_q_alc30d_2, likert_alc30d_2)
    } else {
      # Only beer, wine, spirits for before 2014
      plot_list[["beer"]] <- create_histogram(filtered_data, "beer", lit_q_alcohol, likert_alcohol)
      plot_list[["wine"]] <- create_histogram(filtered_data, "wine", lit_q_alcohol, likert_alcohol)
      plot_list[["spirits"]] <- create_histogram(filtered_data, "spirits", lit_q_alcohol, likert_alcohol)
    }
  }
  
  if (variable == "smoking"){
    
    # Define title
    lit_q_smoking = "How often do you smoke tobacco at present?"
    
    # Define order
    likert_smoking = c("1" = "Don't", "2" = "Less than once a week", 
                       "3" = "Once a week", "4" = "Every day")
    
    if (surveyyear == 2014){
      # Plot all for 2014
      plot_list[["smoking"]] <- create_histogram(filtered_data, "smoking", lit_q_smoking, likert_smoking)
      
      lit_q_smok30d_2 = "On how many days (if any) have you smoked cigarettes?"
      likert_smok30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                           "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["smok30d"]] <- create_histogram(filtered_data, "smok30d", lit_q_smok30d_2, likert_smok30d_2)
    } else if (surveyyear == 2018){
      # Only 30d for 2018
      lit_q_smok30d_2 = "On how many days (if any) have you smoked cigarettes?"
      likert_smok30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                           "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["smok30d"]] <- create_histogram(filtered_data, "smok30d", lit_q_smok30d_2, likert_smok30d_2)
    } else {
      # Only smoking for before 2014
      plot_list[["smoking"]] <- create_histogram(filtered_data, "smoking", lit_q_smoking, likert_smoking)
    }
  }
  
  return(plot_list)
}

### LPA panel data processing

hbsc_labels <- read_excel("data/hbsc_labels.xlsx")

# Function to convert hbsc_labels to nested list structure (removes NAs properly)
convert_to_profile_mapping <- function(df) {
  # Initialize the result list
  result <- list()

  # Iterate through each row (each country)
  for (i in 1:nrow(df)) {
    country <- df$Country[i]

    # Create country entry
    result[[country]] <- list()

    # Add "ALL" (Overall profiles)
    overall_cols <- paste0("Overall_Profile_", 1:5)
    overall_values <- as.character(df[i, overall_cols])
    # Remove NAs, empty strings, and the string "NA"
    overall_values <- overall_values[!is.na(overall_values) & overall_values != "" & overall_values != "NA"]
    if (length(overall_values) > 0) {
      result[[country]][["ALL"]] <- overall_values
    }

    # Add year-specific profiles
    years <- c("2002", "2006", "2010", "2014", "2018")
    for (year in years) {
      year_cols <- paste0(year, "_Profile_", 1:5)
      year_values <- as.character(df[i, year_cols])
      # Remove NAs, empty strings, and the string "NA"
      year_values <- year_values[!is.na(year_values) & year_values != "" & year_values != "NA"]
      if (length(year_values) > 0) {
        result[[country]][[year]] <- year_values
      }
    }
  }

  return(result)
}

# Convert the data
profile_mapping <- convert_to_profile_mapping(hbsc_labels)

# Function to create LPA line plot
create_lpa_plot <- function(input_country, input_year = NULL) {
  
  # Define the master lookup table for all possible profile labels
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use", 
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:6,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#2e6f8e", "#8856a7", "#482173"),
    linetype = c("solid", "dashed", "longdash", "dotted", "dashed", "solid"),
    shape = c(16, 17, 18, 4, 8, 16),
    stringsAsFactors = FALSE
  )
  
  # Helper function to match profile labels to base categories
  match_profile_category <- function(profile_label) {
    for (i in 1:nrow(profile_styles)) {
      if (grepl(paste0("^", profile_styles$label[i]), profile_label)) {
        return(i)
      }
    }
    return(NA)
  }
  
  # Construct file paths
  if (!is.null(input_year) && input_year != "ALL") {
    class_solution <- hbsc_labels %>% filter(Country == input_country) %>% pull(ClassSolution)
    class_num <- gsub("C", "", class_solution)
    mplus_folder_path <- paste0("data/LPA/", input_country, "/",  input_year)
    csv_filename <- paste0(tolower(input_country), "_", input_year, "_c", class_num, ".csv")
  } else {
    class_solution <- hbsc_labels %>% filter(Country == input_country) %>% pull(ClassSolution)
    class_num <- gsub("C", "", class_solution)
    mplus_folder_path <- paste0("data/LPA/", input_country)
    csv_filename <- paste0(tolower(input_country), "_c", class_num, ".csv")
  }
  
  csv_filepath <- file.path(mplus_folder_path, csv_filename)
  
  tryCatch({
    # Read the CSV file directly
    means_df_filtered <- read.csv(csv_filepath)
    
    # Rename "undietary behaviour" to "Unhealthy Diet" (corrected)
    means_df_filtered$Variable <- gsub("undietary behavio(u)?r", "Unhealthy Diet", means_df_filtered$Variable, ignore.case = TRUE)
    
    # Rename "alcohol" to "Alcohol Consumption"
    means_df_filtered$Variable <- gsub("alcohol", "Alcohol Consumption", means_df_filtered$Variable, ignore.case = TRUE)
    
    # Set the desired order for x-axis variables
    desired_order <- c("Alcohol Consumption", "Smoking", "Physical Inactivity", "Sleep Problems", "Unhealthy Diet")
    means_df_filtered$Variable <- factor(means_df_filtered$Variable, levels = desired_order)
    
    # Convert LatentClass to factor - dynamically based on actual number of classes
    class_num_numeric <- as.numeric(class_num)
    means_df_filtered$LatentClass <- factor(means_df_filtered$LatentClass,
                                            levels = 1:class_num_numeric)
    
    # Check if country and year exist in profile mapping
    year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
    use_mapping <- input_country %in% names(profile_mapping) && 
      year_key %in% names(profile_mapping[[input_country]])
    
    if (use_mapping) {
      # Use profile mapping to create meaningful labels
      country_mapping <- profile_mapping[[input_country]][[year_key]]
      
      # Create profile labels using the mapping
      means_df_filtered$ProfileLabel <- country_mapping[means_df_filtered$LatentClass]
      
      # Match each profile label to its base category
      means_df_filtered$ProfileCategory <- sapply(means_df_filtered$ProfileLabel, function(label) {
        idx <- match_profile_category(label)
        if (!is.na(idx)) {
          return(profile_styles$label[idx])
        } else {
          return(label)  # Fallback to original label if no match
        }
      })
      
      # Create profile labels with proportions for display
      means_df_filtered$ProfileLabelWithProp <- paste0(
        means_df_filtered$ProfileLabel, 
        " (", 
        means_df_filtered$Proportion, 
        "%)"
      )
      
      # Get unique categories that actually exist in the data
      existing_categories <- unique(means_df_filtered$ProfileCategory)
      
      # Filter profile_styles to only include existing categories and sort by order
      relevant_styles <- profile_styles[profile_styles$label %in% existing_categories, ]
      relevant_styles <- relevant_styles[order(relevant_styles$order), ]
      
      # Create ordered factor levels based on the category order
      # First create a mapping from ProfileLabel to order
      means_df_filtered$ProfileOrder <- sapply(means_df_filtered$ProfileCategory, function(cat) {
        order_val <- relevant_styles$order[relevant_styles$label == cat]
        if (length(order_val) > 0) {
          return(order_val[1])  # Take first element if multiple matches
        } else {
          return(NA)
        }
      }, USE.NAMES = FALSE, simplify = TRUE)
      
      # Ensure ProfileOrder is numeric
      means_df_filtered$ProfileOrder <- as.numeric(means_df_filtered$ProfileOrder)
      
      # Order the ProfileLabelWithProp by the category order
      ordered_data <- means_df_filtered[order(means_df_filtered$ProfileOrder), ]
      ordered_levels <- unique(ordered_data$ProfileLabelWithProp)
      
      means_df_filtered$ProfileLabelWithProp <- factor(
        means_df_filtered$ProfileLabelWithProp, 
        levels = ordered_levels
      )
      
      means_df_filtered$ProfileLabel <- factor(
        means_df_filtered$ProfileLabel,
        levels = unique(ordered_data$ProfileLabel)
      )
      
    } else {
      # Fall back to existing labeling logic
      means_df_filtered$ProfileLabel <- factor(
        paste("Profile", means_df_filtered$LatentClass),
        levels = paste("Profile", 1:class_num)
      )
      
      means_df_filtered$ProfileLabelWithProp <- paste0(
        means_df_filtered$ProfileLabel, 
        " (", 
        means_df_filtered$Proportion, 
        "%)"
      )
      
      # Set factor levels for fallback case
      means_df_filtered$ProfileLabelWithProp <- factor(
        means_df_filtered$ProfileLabelWithProp, 
        levels = unique(means_df_filtered$ProfileLabelWithProp)
      )
      
      # For consistency, set ProfileCategory for fallback
      means_df_filtered$ProfileCategory <- as.character(means_df_filtered$ProfileLabel)
    }
    
    # Reshape the data to long format for plotting
    means_long <- melt(means_df_filtered, id.vars = c("Variable", "LatentClass", "ProfileLabel", "ProfileLabelWithProp", "ProfileCategory"),
                       measure.vars = "Estimate")
    
    # Create scale mappings
    if (use_mapping) {
      # Get unique ProfileLabelWithProp values
      unique_labels_with_prop <- levels(means_df_filtered$ProfileLabelWithProp)
      
      # Get the corresponding categories for each label
      categories <- sapply(unique_labels_with_prop, function(label) {
        unique(means_df_filtered$ProfileCategory[means_df_filtered$ProfileLabelWithProp == label])
      })
      
      # Match to profile_styles lookup table
      style_indices <- match(categories, profile_styles$label)
      
      # Create named vectors for the scales
      color_values <- profile_styles$color[style_indices]
      names(color_values) <- unique_labels_with_prop
      
      linetype_values <- profile_styles$linetype[style_indices]
      names(linetype_values) <- unique_labels_with_prop
      
      shape_values <- profile_styles$shape[style_indices]
      names(shape_values) <- unique_labels_with_prop
      
    } else {
      # Fallback scale mappings for generic profile labels
      unique_labels <- unique(means_df_filtered$ProfileLabelWithProp)
      n_profiles <- length(unique_labels)
      
      # Use first n colors/shapes/linetypes from the lookup table
      color_values <- profile_styles$color[1:n_profiles]
      names(color_values) <- unique_labels
      
      linetype_values <- profile_styles$linetype[1:n_profiles]
      names(linetype_values) <- unique_labels
      
      shape_values <- profile_styles$shape[1:n_profiles]
      names(shape_values) <- unique_labels
    }
    
    # Create the plot with ggplot2
    plot <- ggplot(means_long, aes(x = Variable, y = value, 
                                   group = ProfileLabelWithProp,
                                   color = ProfileLabelWithProp, 
                                   linetype = ProfileLabelWithProp, 
                                   shape = ProfileLabelWithProp)) +
      geom_point(size = 4) +
      geom_line() +
      labs(
        title = paste0("Latent Profile Analysis ", input_country, 
                       ifelse(!is.null(input_year) && input_year != "ALL", paste0(" ", input_year), ""), 
                       ", ", class_num, " Profiles"),
        x = "Health Behaviours",
        y = "Means",
        color = "Risk Profile",
        linetype = "Risk Profile",
        shape = "Risk Profile"
      ) +
      scale_y_continuous(limits = c(-1, 7)) +
      scale_color_manual(values = color_values) +
      scale_linetype_manual(values = linetype_values) +
      scale_shape_manual(values = shape_values) +
      theme_minimal() +
      theme(
        title = element_text(size = 14),
        text = element_text(size = 12),
        axis.title.x = element_text(size = 14, margin = margin(t = 10)),
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        legend.key.width = unit(.5, "line"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.position = "top"
      )
    
    return(plot)
    
  }, error = function(e) {
    # Check for missing predictor variables in HBSC data
    pred_labels <- c("Physical Inactivity", "Sleep Problems", "Unhealthy Diet", "Smoking", "Alcohol Consumption")
    
    tryCatch({
      # Filter HBSC data for the specific country and year
      if (!is.null(input_year) && input_year != "ALL") {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country, surveyyear == input_year)
      } else {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country)
      }
      
      # Check which variable is missing (all NA)
      if (nrow(hbsc_subset) > 0) {
        missing_var <- sapply(pred_vars, function(var) {
          all(is.na(hbsc_subset[[var]]))
        })
        
        if (any(missing_var)) {
          missing_label <- pred_labels[missing_var][1]
          error_msg <- paste("The risk behaviour variable", missing_label, 
                             "was not collected in", input_country, input_year, ": Unable to calculate LPA profiles")
        }
      }
    })
    
    # Return an error plot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = error_msg) +
      theme_void()
  })
}

# Function to create multinomial regression plot
create_multinomial_plot <- function(input_country, input_year = NULL) {
  
  # Define the master lookup table for all possible profile labels (same as in LPA function)
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use",
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:6,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#2e6f8e", "#8856a7", "#482173"),
    shape = c(16, 17, 18, 4, 8, 16),
    stringsAsFactors = FALSE
  )
  
  # Helper function to match profile labels to base categories
  match_profile_category <- function(profile_label) {
    for (i in 1:nrow(profile_styles)) {
      if (grepl(paste0("^", profile_styles$label[i]), profile_label)) {
        return(i)
      }
    }
    return(NA)
  }
  
  # Construct file paths
  if (!is.null(input_year) && input_year != "ALL") {
    file_path <- file.path("data", "Regression", input_country, input_year, 
                           paste0(input_country, "_", input_year, "_multinom_profile~age+sex+ses.csv"))
  } else {
    file_path <- file.path("data", "Regression", input_country, 
                           paste0(input_country, "_all_multinom_profile~age+sex+ses.csv"))
  }
  
  tryCatch({
    # Read the multinomial regression data
    df_multinom <- read.csv(file_path)
    
    # Check if country and year exist in profile mapping
    year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
    use_mapping <- input_country %in% names(profile_mapping) && 
      year_key %in% names(profile_mapping[[input_country]])
    
    # Prepare data for plotting
    if (use_mapping) {
      df_plot <- df_multinom %>%
        filter(term != "(Intercept)") %>%  # exclude intercepts
        mutate(
          # Get the full profile name from mapping
          ProfileName = profile_mapping[[input_country]][[year_key]][as.numeric(y.level)],
          term = factor(term, levels = unique(term))
        )
      
      # Match each profile to its base category
      df_plot$ProfileCategory <- sapply(df_plot$ProfileName, function(label) {
        idx <- match_profile_category(label)
        if (!is.na(idx)) {
          return(profile_styles$label[idx])
        } else {
          return(label)
        }
      })
      
      # Create the display label (ProfileName vs Low risk)
      df_plot$Profile <- paste(df_plot$ProfileName, "vs Low risk")
      
      # Get unique categories and order them
      existing_categories <- unique(df_plot$ProfileCategory)
      relevant_styles <- profile_styles[profile_styles$label %in% existing_categories, ]
      relevant_styles <- relevant_styles[order(relevant_styles$order), ]
      
      # Add order column for sorting
      df_plot$ProfileOrder <- sapply(df_plot$ProfileCategory, function(cat) {
        order_val <- relevant_styles$order[relevant_styles$label == cat]
        if (length(order_val) > 0) {
          return(order_val[1])
        } else {
          return(NA)
        }
      }, USE.NAMES = FALSE, simplify = TRUE)
      df_plot$ProfileOrder <- as.numeric(df_plot$ProfileOrder)
      
      # Order the profiles
      df_plot <- df_plot[order(df_plot$ProfileOrder), ]
      
      # Set factor levels based on order
      df_plot$ProfileName <- factor(df_plot$ProfileName, 
                                     levels = unique(df_plot$ProfileName))
      df_plot$Profile <- factor(df_plot$Profile,
                                levels = unique(df_plot$Profile))
      
      # Create color and shape mappings based on categories
      unique_profile_names <- levels(df_plot$ProfileName)
      categories <- sapply(unique_profile_names, function(name) {
        unique(df_plot$ProfileCategory[df_plot$ProfileName == name])
      })
      
      style_indices <- match(categories, profile_styles$label)
      
      color_values <- profile_styles$color[style_indices]
      names(color_values) <- unique_profile_names
      
      shape_values <- profile_styles$shape[style_indices]
      names(shape_values) <- unique_profile_names
      
    } else {
      # Fallback to generic labels
      df_plot <- df_multinom %>%
        filter(term != "(Intercept)") %>%
        mutate(
          ProfileName = paste("Profile", y.level),
          Profile = paste(ProfileName, "vs Profile 1"),
          term = factor(term, levels = unique(term))
        )
      
      # Set factor levels
      df_plot$ProfileName <- factor(df_plot$ProfileName,
                                     levels = unique(df_plot$ProfileName))
      df_plot$Profile <- factor(df_plot$Profile,
                                levels = unique(df_plot$Profile))
      
      # Fallback colors and shapes
      n_profiles <- length(unique(df_plot$ProfileName))
      color_values <- profile_styles$color[1:n_profiles]
      names(color_values) <- levels(df_plot$ProfileName)
      
      shape_values <- profile_styles$shape[1:n_profiles]
      names(shape_values) <- levels(df_plot$ProfileName)
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = odds_ratio, y = term)) +
      geom_point(aes(color = ProfileName, shape = ProfileName), size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = ProfileName), 
                     height = 0.2) +
      geom_text(aes(label = sprintf("OR = %.2f", odds_ratio), x = odds_ratio),
                size = 3.5, hjust = 0.46, vjust = 4, color = "black") +  
      theme_minimal() +
      labs(title = paste("Multinomial Regression: Odds Ratios by Profile -", input_country,
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0(" ", input_year), "")),
           x = "Estimate (Odds Ratio, 95% CI)", y = "",
           color = "Risk Profile", shape = "Risk Profile") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
      facet_wrap(~ Profile) +
      scale_color_manual(values = color_values) +
      scale_shape_manual(values = shape_values) +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 12),                  
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.text.y = element_text(size = 12, margin = margin(r = 10)),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    return(plot)
    
  }, error = function(e) {
    # PRETEST 1: Check for missing predictor variables in HBSC data
    pred_labels <- c("Physical Inactivity", "Sleep Problems", "Unhealthy Diet", "Smoking", "Alcohol Consumption")
    
    tryCatch({
      # Filter HBSC data for the specific country and year
      if (!is.null(input_year) && input_year != "ALL") {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country, surveyyear == input_year)
      } else {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country)
      }
      
      # Check which predictor variable is missing (all NA)
      if (nrow(hbsc_subset) > 0) {
        missing_var <- sapply(pred_vars, function(var) {
          all(is.na(hbsc_subset[[var]]))
        })
        
        if (any(missing_var)) {
          missing_label <- pred_labels[which(missing_var)[1]]
          error_msg <- paste("The risk behaviour variable", missing_label, 
                             "was not collected in", input_country, input_year, 
                             ": Unable to calculate profiles and regressions")
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
                   theme_void())
        }
      }
    }, error = function(e2) {
      # If the pretest itself fails, continue to next check
    })
    
    # PRETEST 2: Check for missing outcome variable
    error_msg <- paste("The outcome variable", outcome_variable, 
                       "was not collected in", input_country, input_year)
    
    # Return error plot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
      theme_void()
  })
}

# Function to create linear regression plot with interaction
create_linear_plot_interaction <- function(input_country, input_year = NULL, outcome_variable) {
  
  # Define the master lookup table for all possible profile labels (same as in LPA function)
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use",
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:6,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#2e6f8e", "#8856a7", "#482173"),
    shape = c(16, 17, 18, 4, 8, 16),
    stringsAsFactors = FALSE
  )
  
  # Helper function to match profile labels to base categories
  match_profile_category <- function(profile_label) {
    for (i in 1:nrow(profile_styles)) {
      if (grepl(paste0("^", profile_styles$label[i]), profile_label)) {
        return(profile_styles$label[i])
      }
    }
    return(NA)
  }
  
  # Construct file paths
  if (!is.null(input_year) && input_year != "ALL") {
    file_path <- paste0("data/Regression/", input_country, "/", input_year, "/", input_country, "_", input_year, "_reg_", outcome_variable,  "_profile+age+sex+ses+profilexsex.csv")
  } else {
    file_path <- paste0("data/Regression/", input_country, "/", input_country, "_all_reg_", outcome_variable, "_profile+age+sex+ses+profilexsex.csv")
  }
  
  tryCatch({
    # Read the linear regression data
    df_linear <- read.csv(file_path)
    
    # Check if country and year exist in profile mapping
    year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
    use_mapping <- input_country %in% names(profile_mapping) && 
      year_key %in% names(profile_mapping[[input_country]])
    
    # Prepare data for plotting
    df_plot <- df_linear %>%
      filter(term != "(Intercept)") %>%  # exclude intercepts
      mutate(
        conf.low = std_estimate - 1.96 * std.error,
        conf.high = std_estimate + 1.96 * std.error,
        significant = ifelse((conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0), 
                             "Significant", "Non-significant")
      )
    
    # Find reference profile by checking which profile number is missing from terms (if using mapping)
    if (use_mapping) {
      # Get the total number of profiles from the mapping
      num_profiles <- length(profile_mapping[[input_country]][[year_key]])
      
      profile_terms <- df_plot$term[grepl("^profile[0-9]+", df_plot$term)]
      present_profiles <- as.numeric(gsub("profile", "", gsub(":.*", "", profile_terms)))
      all_profiles <- 1:num_profiles
      reference_profile_num <- setdiff(all_profiles, present_profiles)[1]
    }
    
    df_plot <- df_plot %>%
      mutate(
        # Update term names to use profile mapping if available
        term_updated = if (use_mapping) {
          sapply(term, function(t) {
            # Check if term contains profile reference with any number
            if (grepl("^profile[0-9]+", t)) {
              profile_num <- as.numeric(gsub("profile", "", gsub(":.*", "", t)))
              profile_name <- profile_mapping[[input_country]][[year_key]][profile_num]
              # Replace the profile part with the mapped name
              gsub(paste0("^profile", profile_num), profile_name, t)
            } else {
              as.character(t)
            }
          })
        } else {
          as.character(term)
        }
      )
    
    # Set the desired order for profile terms and add ProfileName/ProfileCategory for coloring
    if (use_mapping) {
      # Match to base categories for all terms
      df_plot$ProfileCategory <- sapply(df_plot$term_updated, function(t) {
        # Extract the profile name part (before any colon if it's an interaction)
        profile_part <- gsub(":.*", "", t)
        # Use the helper function to match to base category
        category <- match_profile_category(profile_part)
        return(category)
      })
      
      # Get unique categories that actually exist in the data
      existing_categories <- unique(df_plot$ProfileCategory)
      existing_categories <- existing_categories[!is.na(existing_categories)]
      
      # Filter and order based on profile_styles
      relevant_styles <- profile_styles[profile_styles$label %in% existing_categories, ]
      relevant_styles <- relevant_styles[order(relevant_styles$order), ]
      desired_profile_order <- relevant_styles$label
      
      # Separate terms into categories
      profile_pattern <- paste0("^(", paste(sapply(desired_profile_order, function(x) {
        # Escape special regex characters and allow for additional text after the base label
        gsub("([.+?^${}()|\\[\\]\\\\])", "\\\\\\1", x)
      }), collapse = "|"), ")")
      
      profile_main_terms <- df_plot$term_updated[grepl(profile_pattern, df_plot$term_updated) & 
                                                   !grepl(":", df_plot$term_updated)]
      interaction_terms <- df_plot$term_updated[grepl(profile_pattern, df_plot$term_updated) & 
                                                  grepl(":", df_plot$term_updated)]
      other_terms <- df_plot$term_updated[!grepl(profile_pattern, df_plot$term_updated)]
      
      # Order each category by matching to base categories
      ordered_profile_main <- character(0)
      for (profile in desired_profile_order) {
        matching_terms <- profile_main_terms[sapply(profile_main_terms, function(t) {
          cat <- match_profile_category(t)
          !is.na(cat) && cat == profile
        })]
        ordered_profile_main <- c(ordered_profile_main, matching_terms)
      }
      
      ordered_interactions <- character(0)
      for (profile in desired_profile_order) {
        matching_terms <- interaction_terms[sapply(interaction_terms, function(t) {
          profile_part <- gsub(":.*", "", t)
          cat <- match_profile_category(profile_part)
          !is.na(cat) && cat == profile
        })]
        ordered_interactions <- c(ordered_interactions, matching_terms)
      }
      
      # Final order: profile main effects, other terms, then interactions
      term_order <- c(ordered_profile_main, other_terms, ordered_interactions)
      df_plot$term_updated <- factor(df_plot$term_updated, levels = term_order)
      
      # Set ProfileCategory as factor for consistent coloring
      df_plot$ProfileCategory <- factor(df_plot$ProfileCategory, 
                                        levels = relevant_styles$label)
      
      # Create color and shape mappings
      color_values <- relevant_styles$color
      names(color_values) <- relevant_styles$label
      
      shape_values <- relevant_styles$shape
      names(shape_values) <- relevant_styles$label
      
    } else {
      df_plot$term_updated <- factor(df_plot$term_updated, levels = unique(df_plot$term_updated))
      df_plot$ProfileCategory <- NA
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = std_estimate, y = term_updated)) +
      {if (use_mapping && any(!is.na(df_plot$ProfileCategory))) {
        list(
          geom_point(aes(color = ProfileCategory, shape = ProfileCategory), size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = ProfileCategory), height = 0.2)
        )
      } else {
        list(
          geom_point(aes(color = significant), shape = 18, size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = significant), height = 0.2)
        )
      }} +
      geom_text(aes(label = sprintf("Est = %.2f", std_estimate), x = std_estimate),
                size = 3.5, hjust = 0.46, vjust = 2, color = "black") +
      theme_minimal() +
      labs(title = paste("With Interaction - Outcome Variable:", outcome_variable, "-", input_country,
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0(" ", input_year), "")),
           x = "Estimate (Standardized Estimate, 95% CI)", y = "",
           color = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "Risk Profile" else "Significance",
           shape = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "Risk Profile" else NULL) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      {if (use_mapping && any(!is.na(df_plot$ProfileCategory))) {
        list(
          scale_color_manual(values = color_values, na.value = "black"),
          scale_shape_manual(values = shape_values, na.value = 18)
        )
      } else {
        scale_color_manual(values = c("Significant" = "steelblue", "Non-significant" = "black"))
      }} +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
            plot.title = element_text(size = 12, face = "bold"),
            legend.position = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "none" else "bottom",
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.text.y = element_text(size = 12, margin = margin(r = 10))) +
      {if (!use_mapping || !any(!is.na(df_plot$ProfileCategory))) {
        guides(color = guide_legend(title = ""))
      }}
    
    return(plot)
    
  }, error = function(e) {
    # PRETEST 1: Check for missing predictor variables in HBSC data
    pred_labels <- c("Physical Inactivity", "Sleep Problems", "Unhealthy Diet", "Smoking", "Alcohol Consumption")
    
    tryCatch({
      # Filter HBSC data for the specific country and year
      if (!is.null(input_year) && input_year != "ALL") {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country, surveyyear == input_year)
      } else {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country)
      }
      
      # Check which predictor variable is missing (all NA)
      if (nrow(hbsc_subset) > 0) {
        missing_var <- sapply(pred_vars, function(var) {
          all(is.na(hbsc_subset[[var]]))
        })
        
        if (any(missing_var)) {
          missing_label <- pred_labels[which(missing_var)[1]]
          error_msg <- paste("The risk behaviour variable", missing_label, 
                             "was not collected in", input_country, input_year, 
                             ": Unable to calculate profiles and regressions")
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
                   theme_void())
        }
      }
    }, error = function(e2) {
      # If the pretest itself fails, continue to next check
    })
    
    # PRETEST 2: Check for missing outcome variable
    error_msg <- paste("The outcome variable", outcome_variable, 
                       "was not collected in", input_country, input_year)
    
    # Return error plot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
      theme_void()
  })
}

# Function to create linear regression plot without interaction
create_linear_plot_main <- function(input_country, input_year = NULL, outcome_variable) {
  
  # Define the master lookup table for all possible profile labels (same as in LPA function)
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use",
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:6,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#2e6f8e", "#8856a7", "#482173"),
    shape = c(16, 17, 18, 4, 8, 16),
    stringsAsFactors = FALSE
  )
  
  # Helper function to match profile labels to base categories
  match_profile_category <- function(profile_label) {
    for (i in 1:nrow(profile_styles)) {
      if (grepl(paste0("^", profile_styles$label[i]), profile_label)) {
        return(profile_styles$label[i])
      }
    }
    return(NA)
  }
  
  # Construct file paths
  if (!is.null(input_year) && input_year != "ALL") {
    file_path <- paste0("data/Regression/", input_country, "/", input_year, "/", input_country, "_", input_year, "_reg_", outcome_variable,  "_profile.csv")
  } else {
    file_path <- paste0("data/Regression/", input_country, "/", input_country, "_all_reg_", outcome_variable, "_profile.csv")
  }
  
  tryCatch({
    # Read the linear regression data
    df_linear <- read.csv(file_path)
    
    # Check if country and year exist in profile mapping
    year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
    use_mapping <- input_country %in% names(profile_mapping) && 
      year_key %in% names(profile_mapping[[input_country]])
    
    # Prepare data for plotting
    df_plot <- df_linear %>%
      filter(term != "(Intercept)") %>%  # exclude intercepts
      mutate(
        conf.low = std_estimate - 1.96 * std.error,
        conf.high = std_estimate + 1.96 * std.error,
        significant = ifelse((conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0), 
                             "Significant", "Non-significant")
      )
    
    # Find reference profile by checking which profile number is missing from terms
    if (use_mapping) {
      # Get the total number of profiles from the mapping
      num_profiles <- length(profile_mapping[[input_country]][[year_key]])
      
      profile_terms <- df_plot$term[grepl("^profile[0-9]+", df_plot$term)]
      present_profiles <- as.numeric(gsub("profile", "", profile_terms))
      all_profiles <- 1:num_profiles
      reference_profile_num <- setdiff(all_profiles, present_profiles)[1]
    }
    
    df_plot <- df_plot %>%
      mutate(
        # Update term names to use profile mapping if available
        term_updated = if (use_mapping) {
          sapply(term, function(t) {
            # Check if term contains profile reference with any number
            if (grepl("^profile[0-9]+", t)) {
              profile_num <- as.numeric(gsub("profile", "", t))
              profile_name <- profile_mapping[[input_country]][[year_key]][profile_num]
              profile_name
            } else {
              as.character(t)
            }
          })
        } else {
          as.character(term)
        }
      )
    
    # Set the desired order for profile terms and add ProfileCategory for coloring
    if (use_mapping) {
      # Match to base categories for all terms
      df_plot$ProfileCategory <- sapply(df_plot$term_updated, function(t) {
        # Use the helper function to match to base category
        category <- match_profile_category(t)
        return(category)
      })
      
      # Get unique categories that actually exist in the data
      existing_categories <- unique(df_plot$ProfileCategory)
      existing_categories <- existing_categories[!is.na(existing_categories)]
      
      # Filter and order based on profile_styles
      relevant_styles <- profile_styles[profile_styles$label %in% existing_categories, ]
      relevant_styles <- relevant_styles[order(relevant_styles$order), ]
      desired_profile_order <- relevant_styles$label
      
      # Order terms: profile terms first in desired order, then other terms
      profile_terms <- df_plot$term_updated[!is.na(df_plot$ProfileCategory)]
      other_terms <- df_plot$term_updated[is.na(df_plot$ProfileCategory)]
      
      ordered_profile_terms <- character(0)
      for (profile in desired_profile_order) {
        matching_terms <- profile_terms[sapply(profile_terms, function(t) {
          cat <- match_profile_category(t)
          !is.na(cat) && cat == profile
        })]
        ordered_profile_terms <- c(ordered_profile_terms, matching_terms)
      }
      
      term_order <- c(ordered_profile_terms, other_terms)
      df_plot$term_updated <- factor(df_plot$term_updated, levels = term_order)
      
      # Set ProfileCategory as factor for consistent coloring
      df_plot$ProfileCategory <- factor(df_plot$ProfileCategory, 
                                        levels = relevant_styles$label)
      
      # Create color and shape mappings
      color_values <- relevant_styles$color
      names(color_values) <- relevant_styles$label
      
      shape_values <- relevant_styles$shape
      names(shape_values) <- relevant_styles$label
      
    } else {
      df_plot$term_updated <- factor(df_plot$term_updated, levels = unique(df_plot$term_updated))
      df_plot$ProfileCategory <- NA
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = std_estimate, y = term_updated)) +
      {if (use_mapping && any(!is.na(df_plot$ProfileCategory))) {
        list(
          geom_point(aes(color = ProfileCategory, shape = ProfileCategory), size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = ProfileCategory), height = 0.2)
        )
      } else {
        list(
          geom_point(aes(color = significant), shape = 18, size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = significant), height = 0.2)
        )
      }} +
      geom_text(aes(label = sprintf("Est = %.2f", std_estimate), x = std_estimate),
                size = 3.5, hjust = 0.46, vjust = 2, color = "black") +
      theme_minimal() +
      labs(title = paste("Main Effects - Outcome Variable:", outcome_variable, "-", input_country,
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0(" ", input_year), "")),
           x = "Estimate (Standardized Estimate, 95% CI)", y = "",
           color = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "Risk Profile" else "Significance",
           shape = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "Risk Profile" else NULL) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      {if (use_mapping && any(!is.na(df_plot$ProfileCategory))) {
        list(
          scale_color_manual(values = color_values, na.value = "black"),
          scale_shape_manual(values = shape_values, na.value = 18)
        )
      } else {
        scale_color_manual(values = c("Significant" = "steelblue", "Non-significant" = "black"))
      }} +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
            plot.title = element_text(size = 12, face = "bold"),
            legend.position = if (use_mapping && any(!is.na(df_plot$ProfileCategory))) "none" else "bottom",
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.text.y = element_text(size = 12, margin = margin(r = 10))) +
      {if (!use_mapping || !any(!is.na(df_plot$ProfileCategory))) {
        guides(color = guide_legend(title = ""))
      }}
    
    return(plot)
    
  }, error = function(e) {
    # PRETEST 1: Check for missing predictor variables in HBSC data
    pred_labels <- c("Physical Inactivity", "Sleep Problems", "Unhealthy Diet", "Smoking", "Alcohol Consumption")
    
    tryCatch({
      # Filter HBSC data for the specific country and year
      if (!is.null(input_year) && input_year != "ALL") {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country, surveyyear == input_year)
      } else {
        hbsc_subset <- hbsc %>% 
          filter(countryname == input_country)
      }
      
      # Check which predictor variable is missing (all NA)
      if (nrow(hbsc_subset) > 0) {
        missing_var <- sapply(pred_vars, function(var) {
          all(is.na(hbsc_subset[[var]]))
        })
        
        if (any(missing_var)) {
          missing_label <- pred_labels[which(missing_var)[1]]
          error_msg <- paste("The risk behaviour variable", missing_label, 
                             "was not collected in", input_country, input_year, 
                             ": Unable to calculate profiles and regressions")
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
                   theme_void())
        }
      }
    }, error = function(e2) {
      # If the pretest itself fails, continue to next check
    })
    
    # PRETEST 2: Check for missing outcome variable
    error_msg <- paste("The outcome variable", outcome_variable, 
                       "was not collected in", input_country, input_year)
    
    # Return error plot
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = error_msg, size = 5) +
      theme_void()
  })
}

# Function to create all regression plots for a given country and year
create_reg_plots <- function(input_country, input_year) {
  
  # Create multinomial regression plot
  multinomial_plot <- create_multinomial_plot(input_country, input_year)
  
  # Create linear regression plots for all outcomes (both main effects and interaction)
  outcome_variables <- c("ache", "feeling", "health", "lifesat")
  linear_plots_main <- list()
  linear_plots_interaction <- list()
  
  for (outcome in outcome_variables) {
    linear_plots_main[[outcome]] <- create_linear_plot_main(input_country, input_year, outcome)
    linear_plots_interaction[[outcome]] <- create_linear_plot_interaction(input_country, input_year, outcome)
  }
  
  # Return all plots as a list
  return(list(
    multinomial_plot = multinomial_plot,
    linear_plots_main = linear_plots_main,
    linear_plots_interaction = linear_plots_interaction
  ))
}

# Helper function to get regression data for tables
get_reg_data <- function(input_country, input_year = NULL) {
  
  # Construct file paths
  year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
  
  # Multinomial data
  if (!is.null(input_year) && input_year != "ALL") {
    multinom_path <- file.path("data", "Regression", input_country, input_year, 
                               paste0(input_country, "_", input_year, "_multinom_profile~age+sex+ses.csv"))
  } else {
    multinom_path <- file.path("data", "Regression", input_country, 
                               paste0(input_country, "_all_multinom_profile~age+sex+ses.csv"))
  }
  
  # Linear regression data paths
  outcome_vars <- c("ache", "feeling", "health", "lifesat")
  
  linear_main_data <- list()
  linear_interaction_data <- list()
  
  for (var in outcome_vars) {
    if (!is.null(input_year) && input_year != "ALL") {
      main_path <- paste0("data/Regression/", input_country, "/", input_year, "/", 
                          input_country, "_", input_year, "_reg_", var, "_profile.csv")
      interaction_path <- paste0("data/Regression/", input_country, "/", input_year, "/", 
                                 input_country, "_", input_year, "_reg_", var, "_profile+age+sex+ses+profilexsex.csv")
    } else {
      main_path <- paste0("data/Regression/", input_country, "/", 
                          input_country, "_all_reg_", var, "_profile.csv")
      interaction_path <- paste0("data/Regression/", input_country, "/", 
                                 input_country, "_all_reg_", var, "_profile+age+sex+ses+profilexsex.csv")
    }
    
    linear_main_data[[var]] <- tryCatch(read.csv(main_path), error = function(e) NULL)
    linear_interaction_data[[var]] <- tryCatch(read.csv(interaction_path), error = function(e) NULL)
  }
  
  multinom_data <- tryCatch(read.csv(multinom_path), error = function(e) NULL)

  # Apply profile name mapping to term column if available
  use_mapping <- input_country %in% names(profile_mapping) &&
    year_key %in% names(profile_mapping[[input_country]])

  rename_profile_terms <- function(df) {
    if (is.null(df) || !use_mapping) return(df)
    if ("term" %in% names(df)) {
      df <- df %>%
        mutate(term = sapply(term, function(t) {
          if (grepl("^profile[0-9]+", t)) {
            profile_num <- as.numeric(gsub("profile", "", gsub(":.*", "", t)))
            profile_name <- profile_mapping[[input_country]][[year_key]][profile_num]
            gsub(paste0("^profile", profile_num), profile_name, t)
          } else {
            as.character(t)
          }
        }))
    }
    if ("y.level" %in% names(df)) {
      df <- df %>%
        mutate(y.level = sapply(y.level, function(y) {
          label <- profile_mapping[[input_country]][[year_key]][as.numeric(y)]
          if (!is.null(label) && !is.na(label)) label else as.character(y)
        }))
    }
    df
  }

  # Profile label order (same as in plot functions)
  ps_labels <- c("Low risk", "Slightly elevated substance use", "Moderate substance use",
                 "High sleep problems", "High alcohol use", "Highest risk")

  get_profile_order <- function(label) {
    label_part <- gsub(":.*", "", as.character(label))
    for (i in seq_along(ps_labels)) {
      if (grepl(paste0("^", ps_labels[i]), label_part)) return(i)
    }
    return(NA_integer_)
  }

  order_table_rows <- function(df) {
    if (is.null(df) || !use_mapping) return(df)
    if ("y.level" %in% names(df)) {
      # Multinomial: sort by y.level profile order
      df$y_order <- sapply(df$y.level, get_profile_order)
      df <- df[order(is.na(df$y_order), df$y_order), ]
      df$y_order <- NULL
    } else if ("term" %in% names(df)) {
      # Linear: profile non-interaction terms first (in profile order), then other terms, then interactions
      df$term_order    <- sapply(df$term, get_profile_order)
      df$is_interaction <- grepl(":", df$term)
      df$sort_group    <- ifelse(!is.na(df$term_order) & !df$is_interaction, 1,
                          ifelse( is.na(df$term_order) & !df$is_interaction, 2, 3))
      df <- df[order(df$sort_group, df$term_order), ]
      df$term_order <- NULL
      df$is_interaction <- NULL
      df$sort_group <- NULL
    }
    rownames(df) <- NULL
    df
  }

  multinom_data <- order_table_rows(rename_profile_terms(multinom_data))
  linear_main_data <- lapply(linear_main_data, function(d) order_table_rows(rename_profile_terms(d)))
  linear_interaction_data <- lapply(linear_interaction_data, function(d) order_table_rows(rename_profile_terms(d)))

  return(list(
    multinomial = multinom_data,
    linear_main = linear_main_data,
    linear_interaction = linear_interaction_data
  ))
}

agecat_map <- c("11 y/o" = 1, "13 y/o" = 2, "15 y/o" = 3)

country <- sort(unique(hbsc$countryname))

survey_years <- unique(hbsc$surveyyear)

