library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(giscoR)
library(fmsb)
library(MplusAutomation)
library(reshape2)

# Load data
# data_path = r"(C:\Users\hanst\OneDrive - Universität Zürich UZH\Datenanalyse\hbsc_allrel.csv)"
data_path = "data/hbsc_variables.csv"
hbsc <- read.csv(data_path, header=TRUE)

# Define Variables
pred_vars <- c("physinact", "sleepprob", "undiet", "smoking", "alcohol")

# Z-Standardized data per country before summary stats
z_hbsc <- hbsc %>%
  group_by(countryname) %>%
  mutate(across(all_of(pred_vars),
                ~ scale(.) %>%
                  as.vector())) %>%
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

# Exclude Russia from data set
hbsc_map <- hbsc_map %>%
  filter(!(countryname == "Russia"))

hbsc <- hbsc %>% 
  filter(!(countryname == "Russia"))

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
  # Health complaints (sleepprob, backache_rev, headache_rev, stomachache_rev, dizzy_rev, irritable_rev, nervous_rev, feellow_rev)
  health_complaints_rev = c("1" = "Rarely or never", "2" = "About every month", 
                            "3" = "About every week", "4" = "More than once a week", 
                            "5" = "About every day"),
  
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
  
  # Physical activity
  likert_physinact = c("1" = "7 days", "2" = "6 days", "3" = "5 days", 
                       "4" = "4 days", "5" = "3 days", "6" = "2 days", 
                       "7" = "1 day", "8" = "0 days"),
  # Unhealthy Diet
  likert_undiet_r = c("1" = "Every day, more than once", "2" = "Once a day, every day",
                      "3" = "5-6 days a week", "4" = "2-4 days a week", 
                      "5" = "Once a week", "6" = "Less than once a week", "7" = "Never"),
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

# Apply health complaints mappings (all use health_complaints_rev)
health_complaint_vars <- c("backache_rev", "headache_rev", "stomachache_rev", 
                           "dizzy_rev", "irritable_rev", "nervous_rev", "feellow_rev", "sleepprob")

for(var in health_complaint_vars) {
  new_var_name <- paste0(var, "_labeled")
  hbsc_raw[[new_var_name]] <- factor(hbsc_raw[[var]], 
                                     levels = names(hbsc_mappings$health_complaints_rev),
                                     labels = hbsc_mappings$health_complaints_rev)
}

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

# Dietary behavior mappings
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
    
    plot_list[["beer"]] <- create_histogram(filtered_data, "beer", lit_q_alcohol, likert_alcohol)
    plot_list[["wine"]] <- create_histogram(filtered_data, "wine", lit_q_alcohol, likert_alcohol)
    plot_list[["spirits"]] <- create_histogram(filtered_data, "spirits", lit_q_alcohol, likert_alcohol)
    
    if (surveyyear %in% c(2014, 2018)){
      lit_q_alc30d_2 = "On how many days (if any) have you drunk alcohol?"
      likert_alc30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                          "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["alc30d"]] <- create_histogram(filtered_data, "alc30d", lit_q_alc30d_2, likert_alc30d_2)
    }
  }
  
  if (variable == "smoking"){
    
    # Define title
    lit_q_smoking = "How often do you smoke tobacco at present?"
    
    # Define order
    likert_smoking = c("1" = "Don't", "2" = "Less than once a week", 
                       "3" = "Once a week", "4" = "Every day")
    
    plot_list[["smoking"]] <- create_histogram(filtered_data, "smoking", lit_q_smoking, likert_smoking)
    
    if (surveyyear %in% c(2014, 2018)){
      lit_q_smok30d_2 = "On how many days (if any) have you smoked cigarettes?"
      likert_smok30d_2 = c("1" = "Never", "2" = "1-2 days", "3" = "3-5 days", "4" = "6-9 days", 
                           "5" = "10-19 days", "6" = "20-29 days", "7" = "30 days (or more)")
      plot_list[["smok30d"]] <- create_histogram(filtered_data, "smok30d", lit_q_smok30d_2, likert_smok30d_2)
    }
  }
  
  return(plot_list)
  
}

### LPA panel data processing

# Nested directory for profile mapping
profile_mapping <- list(
  "Switzerland" = list(
    "ALL" = c("Moderate risk varying substance use", "High substance use", "Low risk", "High risk"),
    "2002" = c("High substance use", "Low risk", "Moderate risk varying substance use", "High risk"),
    "2006" = c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"),
    "2010" = c("High substance use", "High risk", "Moderate risk varying substance use", "Low risk"),
    "2014" = c("Low risk", "High substance use", "High risk", "Moderate risk varying substance use"),
    "2018" = c("High substance use", "Low risk", "Moderate risk varying substance use", "High risk")
  )
)

# Function to create LPA line plot
create_lpa_plot <- function(input_country, input_year = NULL) {
  
  # Construct file paths
  if (!is.null(input_year) && input_year != "ALL") {
    mplus_folder_path <- paste0("data/LPA/", input_country, "/",  input_year)
    csv_filename <- paste0(tolower(input_country), "_", input_year, "_c4.csv")
  } else {
    mplus_folder_path <- paste0("data/LPA/", input_country)
    csv_filename <- paste0(tolower(input_country), "_c4.csv")
  }
  
  csv_filepath <- file.path(mplus_folder_path, csv_filename)
  
  tryCatch({
    # Read the CSV file directly
    means_df_filtered <- read.csv(csv_filepath)
    
    # Rename "undietary behavior" to "Unhealthy Diet" (corrected)
    means_df_filtered$Variable <- gsub("undietary behavior", "Unhealthy Diet", means_df_filtered$Variable, ignore.case = TRUE)
    
    # Rename "alcohol" to "Alcohol Consumption"
    means_df_filtered$Variable <- gsub("alcohol", "Alcohol Consumption", means_df_filtered$Variable, ignore.case = TRUE)
    
    # Set the desired order for x-axis variables
    desired_order <- c("Alcohol Consumption", "Smoking", "Physical Inactivity", "Sleep Problems", "Unhealthy Diet")
    means_df_filtered$Variable <- factor(means_df_filtered$Variable, levels = desired_order)
    
    # Convert LatentClass to factor
    means_df_filtered$LatentClass <- factor(means_df_filtered$LatentClass,
                                            levels = c(1, 2, 3, 4))
    
    # Check if country and year exist in profile mapping
    year_key <- ifelse(is.null(input_year) || input_year == "ALL", "ALL", input_year)
    use_mapping <- input_country %in% names(profile_mapping) && 
      year_key %in% names(profile_mapping[[input_country]])
    
    if (use_mapping) {
      # Use profile mapping to create meaningful labels
      country_mapping <- profile_mapping[[input_country]][[year_key]]
      
      # Create profile labels using the mapping (without proportions for consistent styling)
      means_df_filtered$ProfileLabel <- factor(
        country_mapping[means_df_filtered$LatentClass],
        levels = c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk")
      )
      
      # Create profile labels with proportions for display
      means_df_filtered$ProfileLabelWithProp <- paste0(
        means_df_filtered$ProfileLabel, 
        " (", 
        means_df_filtered$Proportion, 
        "%)"
      )
      
      # Create ordered factor levels based on the actual data
      desired_order <- c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk")
      actual_labels_with_prop <- unique(means_df_filtered$ProfileLabelWithProp)
      ordered_levels <- character(0)
      
      for(profile in desired_order) {
        matching_label <- actual_labels_with_prop[grepl(paste0("^", profile, " \\("), actual_labels_with_prop)]
        if(length(matching_label) > 0) {
          ordered_levels <- c(ordered_levels, matching_label[1])
        }
      }
      
      means_df_filtered$ProfileLabelWithProp <- factor(means_df_filtered$ProfileLabelWithProp, 
                                                       levels = ordered_levels)
      
    } else {
      # Fall back to existing labeling logic
      means_df_filtered$ProfileLabel <- factor(
        paste("Profile", means_df_filtered$LatentClass),
        levels = paste("Profile", 1:4)
      )
      
      means_df_filtered$ProfileLabelWithProp <- paste0(
        means_df_filtered$ProfileLabel, 
        " (", 
        means_df_filtered$Proportion, 
        "%)"
      )
      
      # Set factor levels for fallback case
      means_df_filtered$ProfileLabelWithProp <- factor(means_df_filtered$ProfileLabelWithProp, 
                                                       levels = unique(means_df_filtered$ProfileLabelWithProp))
    }
    
    # Reshape the data to long format for plotting
    means_long <- melt(means_df_filtered, id.vars = c("Variable", "LatentClass", "ProfileLabel", "ProfileLabelWithProp"),
                       measure.vars = "Estimate")
    
    # Create scale mappings
    if (use_mapping) {
      # Get unique ProfileLabelWithProp values to create proper scale mappings
      unique_labels <- unique(means_df_filtered$ProfileLabelWithProp)
      
      # Extract the base profile names for mapping colors/shapes/linetypes
      base_profiles <- gsub(" \\(.*?\\)", "", unique_labels)
      
      # Create named vectors for the scales
      color_values <- c("#29af7f", "#bddf26", "#2e6f8e", "#482173")
      names(color_values) <- unique_labels[match(c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"), base_profiles)]
      
      linetype_values <- c("solid", "dashed", "dotted", "dotdash")
      names(linetype_values) <- unique_labels[match(c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"), base_profiles)]
      
      shape_values <- c(16, 17, 15, 18)
      names(shape_values) <- unique_labels[match(c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"), base_profiles)]
      
    } else {
      # Fallback scale mappings for generic profile labels
      unique_labels <- unique(means_df_filtered$ProfileLabelWithProp)
      color_values <- c("#482173", "#2e6f8e", "#29af7f", "#bddf26")
      names(color_values) <- unique_labels
      
      linetype_values <- c("solid", "dashed", "dotted", "dotdash")
      names(linetype_values) <- unique_labels
      
      shape_values <- c(16, 17, 15, 18)
      names(shape_values) <- unique_labels
    }
    
    # Create the plot with ggplot2 using ProfileLabelWithProp for both styling and legend
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
                       ", 4 Profiles"),
        x = "Health Behaviors",
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
    # Return an error plot if something goes wrong
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error loading data for", input_country, ":", e$message)) +
      theme_void()
  })
}

# Function to create multinomial regression plot
create_multinomial_plot <- function(input_country, input_year = NULL) {
  
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
    df_plot <- df_multinom %>%
      filter(term != "(Intercept)") %>%  # exclude intercepts
      mutate(
        # Use mapping if available, otherwise fall back to generic labels
        ProfileName = if (use_mapping) {
          profile_mapping[[input_country]][[year_key]][as.numeric(y.level)]
        } else {
          paste("Profile", y.level)
        },
        # Reference profile is always "Low risk"
        Profile = paste(ProfileName, "vs Low risk"),
        term = factor(term, levels = unique(term))
      )
    
    # Add ProfileName as factor for consistent coloring
    if (use_mapping) {
      df_plot$ProfileName <- factor(df_plot$ProfileName, 
                                    levels = c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"))
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = odds_ratio, y = term)) +
      geom_point(aes(color = if (use_mapping) ProfileName else Profile, 
                     shape = if (use_mapping) ProfileName else Profile), size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, 
                         color = if (use_mapping) ProfileName else Profile), height = 0.2) +
      geom_text(aes(label = sprintf("OR = %.2f", odds_ratio), x = odds_ratio),
                size = 3.5, hjust = 0.46, vjust = 4, color = "black") +  
      theme_minimal() +
      labs(title = paste("Multinomial Regression: Odds Ratios by Profile -", input_country,
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0("", input_year), "")),
           x = "Estimate (Odds Ratio, 95% CI)", y = "",
           color = "Risk Profile", shape = "Risk Profile") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
      facet_wrap(~ Profile) +
      {if (use_mapping) {
        list(
          scale_color_manual(values = c("Low risk" = "#29af7f", 
                                        "Moderate risk varying substance use" = "#bddf26",
                                        "High substance use" = "#2e6f8e", 
                                        "High risk" = "#482173")),
          scale_shape_manual(values = c("Low risk" = 16, 
                                        "Moderate risk varying substance use" = 17,
                                        "High substance use" = 15, 
                                        "High risk" = 18))
        )
      }} +
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
    # Return an error plot if something goes wrong
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error loading multinomial data for", input_country, ":", e$message)) +
      theme_void()
  })
}

# Function to create linear regression plot with interaction
create_linear_plot_interaction <- function(input_country, input_year = NULL, outcome_variable) {
  
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
    
    # Find reference profile by checking which profile number is missing from terms
    if (use_mapping) {
      profile_terms <- df_plot$term[grepl("^profile[1234]", df_plot$term)]
      present_profiles <- as.numeric(gsub("profile", "", gsub(":.*", "", profile_terms)))
      all_profiles <- 1:4
      reference_profile_num <- setdiff(all_profiles, present_profiles)[1]
    }
    
    df_plot <- df_plot %>%
      mutate(
        # Update term names to use profile mapping if available
        term_updated = if (use_mapping) {
          sapply(term, function(t) {
            # Check if term contains profile reference (e.g., "profile1", "profile2", "profile3", "profile4")
            if (grepl("^profile[1234]", t)) {
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
    
    # Set the desired order for profile terms and add ProfileName for coloring
    if (use_mapping) {
      desired_profile_order <- c("Moderate risk varying substance use", "High substance use", "High risk")
      
      # Extract profile names for coloring/shaping
      df_plot$ProfileName <- sapply(df_plot$term_updated, function(t) {
        for (profile in desired_profile_order) {
          if (grepl(paste0("^", profile), t)) {
            return(profile)
          }
        }
        return(NA)
      })
      
      # Separate terms into categories
      profile_main_terms <- df_plot$term_updated[grepl("^(Moderate risk|High substance|High risk)", df_plot$term_updated) & 
                                                   !grepl(":", df_plot$term_updated)]
      interaction_terms <- df_plot$term_updated[grepl("^(Moderate risk|High substance|High risk)", df_plot$term_updated) & 
                                                  grepl(":", df_plot$term_updated)]
      other_terms <- df_plot$term_updated[!grepl("^(Moderate risk|High substance|High risk)", df_plot$term_updated)]
      
      # Order each category
      ordered_profile_main <- character(0)
      for (profile in desired_profile_order) {
        matching_terms <- profile_main_terms[grepl(paste0("^", profile, "$"), profile_main_terms)]
        ordered_profile_main <- c(ordered_profile_main, matching_terms)
      }
      
      ordered_interactions <- character(0)
      for (profile in desired_profile_order) {
        matching_terms <- interaction_terms[grepl(paste0("^", profile, ":"), interaction_terms)]
        ordered_interactions <- c(ordered_interactions, matching_terms)
      }
      
      # Final order: profile main effects, other terms, then interactions
      term_order <- c(ordered_profile_main, other_terms, ordered_interactions)
      df_plot$term_updated <- factor(df_plot$term_updated, levels = term_order)
      
      # Set ProfileName as factor for consistent coloring
      df_plot$ProfileName <- factor(df_plot$ProfileName, 
                                    levels = c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"))
    } else {
      df_plot$term_updated <- factor(df_plot$term_updated, levels = unique(df_plot$term_updated))
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = std_estimate, y = term_updated)) +
      {if (use_mapping && any(!is.na(df_plot$ProfileName))) {
        list(
          geom_point(aes(color = ProfileName, shape = ProfileName), size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = ProfileName), height = 0.2)
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
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0("", input_year), "")),
           x = "Estimate (Standardized Estimate, 95% CI)", y = "",
           color = if (use_mapping && any(!is.na(df_plot$ProfileName))) "Risk Profile" else "Significance",
           shape = if (use_mapping && any(!is.na(df_plot$ProfileName))) "Risk Profile" else NULL) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      {if (use_mapping && any(!is.na(df_plot$ProfileName))) {
        list(
          scale_color_manual(values = c("Low risk" = "#29af7f", 
                                        "Moderate risk varying substance use" = "#bddf26",
                                        "High substance use" = "#2e6f8e", 
                                        "High risk" = "#482173"),
                             na.value = "black"),
          scale_shape_manual(values = c("Low risk" = 16, 
                                        "Moderate risk varying substance use" = 17,
                                        "High substance use" = 15, 
                                        "High risk" = 18),
                             na.value = 18)
        )
      } else {
        scale_color_manual(values = c("Significant" = "steelblue", "Non-significant" = "black"))
      }} +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
            plot.title = element_text(size = 12, face = "bold"),
            legend.position = if (use_mapping && any(!is.na(df_plot$ProfileName))) "none" else "bottom",
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.text.y = element_text(size = 12, margin = margin(r = 10))) +
      {if (!use_mapping || !any(!is.na(df_plot$ProfileName))) {
        guides(color = guide_legend(title = ""))
      }}
    
    return(plot)
    
  }, error = function(e) {
    # Return an error plot if something goes wrong
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error loading linear interaction data for", input_country, outcome_variable, ":", e$message)) +
      theme_void()
  })
}

# Function to create linear regression plot without interaction
create_linear_plot_main <- function(input_country, input_year = NULL, outcome_variable) {
  
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
      profile_terms <- df_plot$term[grepl("^profile[1234]", df_plot$term)]
      present_profiles <- as.numeric(gsub("profile", "", profile_terms))
      all_profiles <- 1:4
      reference_profile_num <- setdiff(all_profiles, present_profiles)[1]
    }
    
    df_plot <- df_plot %>%
      mutate(
        # Update term names to use profile mapping if available
        term_updated = if (use_mapping) {
          sapply(term, function(t) {
            # Check if term contains profile reference (e.g., "profile1", "profile2", "profile3", "profile4")
            if (grepl("^profile[1234]", t)) {
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
    
    # Set the desired order for profile terms and add ProfileName for coloring
    if (use_mapping) {
      desired_profile_order <- c("Moderate risk varying substance use", "High substance use", "High risk")
      
      # Extract profile names for coloring/shaping (main effects only, no interactions)
      df_plot$ProfileName <- sapply(df_plot$term_updated, function(t) {
        for (profile in desired_profile_order) {
          if (t == profile) {
            return(profile)
          }
        }
        return(NA)
      })
      
      # Order terms: profile terms first in desired order, then other terms
      profile_terms <- df_plot$term_updated[!is.na(df_plot$ProfileName)]
      other_terms <- df_plot$term_updated[is.na(df_plot$ProfileName)]
      
      ordered_profile_terms <- character(0)
      for (profile in desired_profile_order) {
        if (profile %in% profile_terms) {
          ordered_profile_terms <- c(ordered_profile_terms, profile)
        }
      }
      
      term_order <- c(ordered_profile_terms, other_terms)
      df_plot$term_updated <- factor(df_plot$term_updated, levels = term_order)
      
      # Set ProfileName as factor for consistent coloring
      df_plot$ProfileName <- factor(df_plot$ProfileName, 
                                    levels = c("Low risk", "Moderate risk varying substance use", "High substance use", "High risk"))
    } else {
      df_plot$term_updated <- factor(df_plot$term_updated, levels = unique(df_plot$term_updated))
    }
    
    # Create the plot
    plot <- ggplot(df_plot, aes(x = std_estimate, y = term_updated)) +
      {if (use_mapping && any(!is.na(df_plot$ProfileName))) {
        list(
          geom_point(aes(color = ProfileName, shape = ProfileName), size = 3),
          geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = ProfileName), height = 0.2)
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
                         ifelse(!is.null(input_year) && input_year != "ALL", paste0("", input_year), "")),
           x = "Estimate (Standardized Estimate, 95% CI)", y = "",
           color = if (use_mapping && any(!is.na(df_plot$ProfileName))) "Risk Profile" else "Significance",
           shape = if (use_mapping && any(!is.na(df_plot$ProfileName))) "Risk Profile" else NULL) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      {if (use_mapping && any(!is.na(df_plot$ProfileName))) {
        list(
          scale_color_manual(values = c("Low risk" = "#29af7f", 
                                        "Moderate risk varying substance use" = "#bddf26",
                                        "High substance use" = "#2e6f8e", 
                                        "High risk" = "#482173"),
                             na.value = "black"),
          scale_shape_manual(values = c("Low risk" = 16, 
                                        "Moderate risk varying substance use" = 17,
                                        "High substance use" = 15, 
                                        "High risk" = 18),
                             na.value = 18)
        )
      } else {
        scale_color_manual(values = c("Significant" = "steelblue", "Non-significant" = "black"))
      }} +
      theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
            plot.title = element_text(size = 12, face = "bold"),
            legend.position = if (use_mapping && any(!is.na(df_plot$ProfileName))) "none" else "bottom",
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12, margin = margin(t = 10)),
            axis.text.y = element_text(size = 12, margin = margin(r = 10))) +
      {if (!use_mapping || !any(!is.na(df_plot$ProfileName))) {
        guides(color = guide_legend(title = ""))
      }}
    
    return(plot)
    
  }, error = function(e) {
    # Return an error plot if something goes wrong
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error loading linear main effects data for", input_country, outcome_variable, ":", e$message)) +
      theme_void()
  })
}

# Function to create all plots for a given country and year
create_all_plots <- function(input_country, input_year) {
  
  # Create LPA plot
  lpa_plot <- create_lpa_plot(input_country, input_year)
  
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
    lpa_plot = lpa_plot,
    multinomial_plot = multinomial_plot,
    linear_plots_main = linear_plots_main,
    linear_plots_interaction = linear_plots_interaction
  ))
}

agecat_map <- c("11 y/o" = 1, "13 y/o" = 2, "15 y/o" = 3)

country <- unique(hbsc$countryname)

survey_years <- unique(hbsc$surveyyear)

# Define UI
ui <- fluidPage(
  
  includeCSS("www/uzh_styles.css"),
  
  titlePanel("Bechtiger & Janousch (2025): Adolescent Well-Being in Flux"),
  
  # Navigation bar for different pages
  navbarPage("Menu", selected = "Map",
             
             # Home Page
             tabPanel("Home",
                      fluidPage(
                        h2("Welcome to the Adolescent Well-Being in Flux App"),
                        
                        fluidRow(
                          column(width = 12,
                                 tags$img(src = "www/logo_UZH.png", height = "80px", style = "margin: 10px;"),
                                 tags$img(src = "www/logo_PUK.png", height = "80px", style = "margin: 10px;"),
                                 tags$img(src = "www/logo_JC.png", height = "80px", style = "margin: 10px;"),
                                 tags$img(src = "www/logo_PRC.png", height = "80px", style = "margin: 10px;")
                          )
                        ),
                        
                        p("We gratefully acknowledge the UZH Population Research Center for supporting this research through a Seed Grant."),
                        p("This app provides insights into adolescent well-being through descriptive statistics and latent profile analysis (LPA)."),
                        p("Use the navigation menu to explore the data.")
                      )
             ),
             
             # Map Page
             tabPanel("Map",
                      tabPanel("Map",
                               # Set max height for the page to fill browser window + info icon styling
                               tags$style(type = "text/css", "
                    html, body {height: 100%} 
                    #mapContainer {height: calc(90vh - 200px);}
                    
                    /* Info icon styling */
                    .info-icon {
                      color: #007bff;
                      margin-left: -2px;
                      cursor: pointer;
                      font-size: 14px;
                      font-weight: bold;
                      vertical-align: super;
                      position: relative;
                      top: -4px;
                      text-shadow: 0 0 1px #007bff;
                    }
                    
                    /* Remove any title attributes that show on hover */
                    .info-icon[title] {
                      title: none !important;
                    }
                    
                    /* Prevent tab titles from interfering with tooltips */
                    .nav-tabs > li > a {
                      position: relative;
                      z-index: 1;
                    }
                    
                    /* Remove browser default tooltips */
                    * {
                      -webkit-user-select: none;
                      -moz-user-select: none;
                      -ms-user-select: none;
                      user-select: none;
                    }
                    
                    .tooltip-text {
                      -webkit-user-select: text;
                      -moz-user-select: text;
                      -ms-user-select: text;
                      user-select: text;
                    }
                    
                    /* Tooltip styling */
                    .info-tooltip {
                      position: relative;
                      display: inline-block;
                    }
                    
                    .info-tooltip .tooltip-text {
                      visibility: hidden;
                      width: 320px;
                      background-color: #333;
                      color: white;
                      text-align: left;
                      border-radius: 6px;
                      padding: 10px 15px;
                      position: absolute;
                      z-index: 1000;
                      top: 50%;
                      left: 100%;
                      margin-left: 5px;
                      margin-top: -50px;
                      opacity: 0;
                      transition: opacity 0.3s;
                      font-size: 13px;
                      line-height: 1.4;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
                    }
                    
                    .info-tooltip .tooltip-text::before {
                      content: '';
                      position: absolute;
                      top: 50%;
                      left: -5px;
                      margin-top: -5px;
                      border-width: 5px;
                      border-style: solid;
                      border-color: transparent #333 transparent transparent;
                    }
                    
                    .info-tooltip:hover .tooltip-text,
                    .info-tooltip .tooltip-text:hover {
                      visibility: visible !important;
                      opacity: 1 !important;
                      animation: none !important;
                    }
                    
                    /* Auto-show tooltip for first visit */
                    .info-tooltip .tooltip-text.auto-show {
                      visibility: visible !important;
                      opacity: 1 !important;
                      animation: fadeOut 6s ease-in-out forwards;
                    }
                    
                    @keyframes fadeOut {
                      0% { opacity: 1; visibility: visible; }
                      83% { opacity: 1; visibility: visible; }
                      100% { opacity: 0; visibility: hidden; }
                    }
                  "),
                               fluidPage(
                                 # Title with info icon
                                 h3(HTML("Health Indicators Over Time 
                              <span class='info-tooltip'>
                                <span class='info-icon' title=''>ⓘ</span>
                                <span class='tooltip-text' id='map-tooltip'>
                                  This interactive map displays health behavior data from the HBSC study. 
                                  Select different indicators to explore patterns across European countries 
                                  and survey years. Click on countries for detailed information. 
                                  Countries are not supposed to be compared directly (z-Scores were calculated within country). 
                                  Use the survey year slider to see how health indicators have developed for each country.
                                  <br><br>
                                  <a href='#' onclick='$(\"a[data-value=\\\"About\\\"]\").tab(\"show\"); return false;' 
                                     style='color: #87CEEB; text-decoration: underline;'>Learn more in the About section</a>
                                </span>
                              </span>")),
                                 
                                 # Health indicator buttons - more compact
                                 fluidRow(
                                   column(12, align = "center",
                                          div(class = "radio-buttons",
                                              radioButtons("map_variable", "Select Health Indicator:", 
                                                           choices = c("Alcohol Consumption" = "alcohol",
                                                                       "Smoking" = "smoking",
                                                                       "Physical Inactivity" = "physinact",
                                                                       "Sleep Problems" = "sleepprob", 
                                                                       "Unhealthy Diet" = "undiet"),
                                                           inline = TRUE)
                                          )
                                   )
                                 ),
                                 
                                 # Map takes remaining height
                                 fluidRow(
                                   column(12, 
                                          div(id = "mapContainer",
                                              leafletOutput("mapPlot", height = "100%", width = "100%")
                                          )
                                   )
                                 ),
                                 
                                 # Year slider - more compact
                                 fluidRow(
                                   column(12, align = "center",
                                          div(class = "year-slider",
                                              sliderInput("year_slider", "Select Survey Year:",
                                                          min = min(hbsc_map$surveyyear, na.rm = TRUE), 
                                                          max = max(hbsc_map$surveyyear, na.rm = TRUE),
                                                          value = min(hbsc_map$surveyyear, na.rm = TRUE), 
                                                          step = 4, sep = "",
                                                          width = "80%")
                                          )
                                   )
                                 ),
                                 
                                 # JavaScript for auto-show tooltip on first visit
                                 tags$script(HTML("
                                   $(document).ready(function() {
                                     // Reset the flag on page refresh
                                     sessionStorage.removeItem('hasSeenMapTooltip');
                                     
                                     // Check if user has seen tooltip in this session
                                     var hasSeenMapTooltip = sessionStorage.getItem('hasSeenMapTooltip');
                                     
                                     if (!hasSeenMapTooltip) {
                                       // Show tooltip automatically for 4 seconds
                                       setTimeout(function() {
                                         $('#map-tooltip').addClass('auto-show');
                                         sessionStorage.setItem('hasSeenMapTooltip', 'true');
                                         
                                         // Remove the auto-show class after animation completes
                                         setTimeout(function() {
                                           $('#map-tooltip').removeClass('auto-show');
                                         }, 4000);
                                       }, 1000); // Increased delay to ensure everything is loaded
                                     }
                                   });
                                   
                                   // Also trigger when Map tab is clicked
                                   $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
                                     if ($(e.target).text().trim() === 'Map') {
                                       var hasSeenMapTooltip = sessionStorage.getItem('hasSeenMapTooltip');
                                       if (!hasSeenMapTooltip) {
                                         setTimeout(function() {
                                           $('#map-tooltip').addClass('auto-show');
                                           sessionStorage.setItem('hasSeenMapTooltip', 'true');
                                           
                                           setTimeout(function() {
                                             $('#map-tooltip').removeClass('auto-show');
                                           }, 6000);
                                         }, 500);
                                       }
                                     }
                                   });
                                 "))
                               )
                      )
             ),
             
             # Descriptive Statistics Page
             tabPanel("Descriptive Statistics",
                      fluidPage(
                        h3("Descriptive Statistics"),
                        br(),
                        p("This page presents descriptive statistics for a selected country, with the option to filter results by survey year."),
                        p("Users may begin by selecting a country and, if desired, refining the data by specifying a survey year. Upon choosing a variable of interest, a summary of that variable will be displayed. This summary can be further expanded by selecting a specific survey year, at which point detailed response plots for the subvariables comprising the selected variable will be generated and displayed."),
                        
                        # All three selectInputs next to each other
                        tags$div(style = "display: flex; gap: 20px;",
                                 tags$div(style = "width: 250px;",
                                          selectInput("country", "Select Country:",
                                                      choices = c("", country),
                                                      selected = "Switzerland")
                                 ),
                                 tags$div(style = "width: 250px;",
                                          selectInput("surveyyear", "Select Survey Year:",
                                                      choices = c("All Survey Years" = "", "Select a country first" = "none"),
                                                      selected = ""
                                          )
                                 ),
                                 
                                 tags$div(style = "width: 300px;",
                                          selectInput("variable", "Select Variable:",
                                                      choices = list(
                                                        "No Variable Selected" = "None",
                                                        "Demographics" = list(
                                                          "Family Affluence Score" = "fas"
                                                        ),
                                                        "Outcome Variables" = list(
                                                          "Feelings" = "feeling",
                                                          "Life Satisfaction" = "lifesat",
                                                          "Physical Aches" = "ache",
                                                          "Self-rated Health" = "health"
                                                        ),
                                                        "Health Behavior" = list(
                                                          "Alcohol Consumption" = "alcohol",
                                                          "Smoking" = "smoking",
                                                          "Physical Inactivity" = "physinact",
                                                          "Sleep Problems" = "sleepprob",
                                                          "Unhealthy Diet" = "undiet"
                                                        )
                                                      ),
                                                      selected = ""
                                          )
                                 )
                        ),
                        
                        # Default content shown when no variable is selected
                        conditionalPanel(
                          condition = "input.variable == 'None' || input.variable == ''",
                          tags$div(style = "display: flex; flex-wrap: wrap;",
                                   tags$div(style = "flex: 0 0 auto; min-width: 300px; margin-right: 30px;",
                                            h4("LPA Indicator Variables"),
                                            tableOutput("predictorStatsTable")
                                   ),
                                   tags$div(style = "flex: 0 0 auto; min-width: 250px;",
                                            h4("Outcome Variables"),
                                            tableOutput("outcomeStatsTable")
                                   )
                          ),
                          
                          h4("Demographics"),
                          # Age and Sex histograms side by side with responsive design
                          tags$div(style = "display: flex; flex-wrap: wrap; gap: 20px;",
                                   tags$div(style = "flex: 1; min-width: 300px;",
                                            plotOutput("ageHistogramDefault", height = "400px")
                                   ),
                                   tags$div(style = "flex: 1; min-width: 300px;",
                                            plotOutput("sexHistogramDefault", height = "400px")
                                   )
                          ),
                          br()
                        ),
                        
                        # Variable-specific content shown when variable is selected
                        conditionalPanel(
                          condition = "input.variable != 'None' && input.variable != ''",
                          
                          div(
                            style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin-bottom: 20px;",
                            h5("Variable Definition", style = "margin-top: 0; color: #007bff;"),
                            htmlOutput("variableDefinition")
                          ),
                          
                          # Container for variable summary and demographics side by side
                          tags$div(style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 20px;",
                                   # Variable Summary Section
                                   tags$div(style = "flex: 0 0 auto; width: auto;",
                                            # Show summary when All Survey Years is selected
                                            conditionalPanel(
                                              condition = "input.surveyyear == 'ALL' || input.surveyyear == ''",
                                              h4("Variable Summary"),
                                              tableOutput("variableSummaryTable")
                                            ),
                                            
                                            # Show year-specific when a specific year is selected  
                                            conditionalPanel(
                                              condition = "input.surveyyear != 'ALL' && input.surveyyear != ''",
                                              h4("Variable Summary by Year"),
                                              tableOutput("variableYearSummaryTable")
                                            )
                                   ),
                                   
                                   # Demographics Section
                                   tags$div(style = "flex: 1; min-width: 400px;",
                                            h4("Demographics"),
                                            # Age and Sex histograms side by side
                                            tags$div(style = "display: flex; flex-wrap: wrap; gap: 20px;",
                                                     tags$div(style = "flex: 1; min-width: 280px;",
                                                              plotOutput("ageHistogramVariable", height = "300px")
                                                     ),
                                                     tags$div(style = "flex: 1; min-width: 280px;",
                                                              plotOutput("sexHistogramVariable", height = "300px")
                                                     )
                                            )
                                   )
                          ),
                          
                          # Variable Composition Section (moved outside the flex container)
                          h3("Variable Composition"),
                          br(),
                          p("Select a survey year to display response plots. The response plots below show which subvariables the selected variable is composed of. It also presents the literal questions, scales and distribution of responses for a selected survey year."),
                          br(),
                          
                          # Response Plots (shown only when specific year is selected)
                          conditionalPanel(
                            condition = "input.surveyyear != 'ALL' && input.surveyyear != ''",
                            br(),
                            h4("Response Plots"),
                            uiOutput("histogramPlots")
                          )
                        )
                      )
             ),
             
             # LPA Page
             tabPanel("LPA",
                      fluidPage(
                        h3("Latent Profile Analysis"),
                        tags$div(style = "display: flex; flex-wrap: wrap;",
                                 tags$div(style = "flex: 0 0 auto; min-width: 300px; margin-right: 30px;",
                                          selectInput("lpa_country", "Select Country:",
                                                      choices = c("", country),
                                                      selected = "Switzerland")
                                 ),
                                 tags$div(style = "flex: 0 0 auto; min-width: 250px; margin-right: 30px;",
                                          selectInput("lpa_year", "Select Survey Year:",
                                                      choices = c("All Survey Years" = "ALL"),
                                                      selected = "ALL")
                                 ),
                                 tags$div(style = "flex: 0 0 auto; min-width: 200px;",
                                          checkboxInput("compare_countries", "Compare Countries", 
                                                        value = FALSE)
                                 )
                        ),
                        
                        # Conditional country selection for comparison
                        conditionalPanel(
                          condition = "input.compare_countries",
                          tags$div(style = "margin-top: 10px; margin-bottom: 10px;",
                                   selectInput("lpa_country_compare", "Select Second Country for Comparison:",
                                               choices = c("", country),
                                               selected = "")
                          )
                        ),
                        br(),
                        
                        # LPA plots - conditional layout based on comparison toggle
                        conditionalPanel(
                          condition = "!input.compare_countries",
                          fluidRow(
                            column(12,
                                   plotOutput("lpa_plot_single", width = "100%", height = "600px")
                            )
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.compare_countries",
                          fluidRow(
                            column(6,
                                   h4(textOutput("country1_title")),
                                   plotOutput("lpa_plot", width = "100%", height = "600px")
                            ),
                            column(6,
                                   h4(textOutput("country2_title")),
                                   plotOutput("lpa_plot_compare", width = "100%", height = "600px")
                            )
                          )
                        ),
                        br(),
                        
                        tags$div(style = "display: flex; flex-wrap: wrap;",
                                 tags$div(style = "flex: 0 0 auto; min-width: 200px;",
                                          checkboxInput("show_regression", "Show Regression Results", 
                                                        value = FALSE)
                                 )
                        ),
                        br(),
                        
                        # Conditional Linear regression plots
                        conditionalPanel(
                          condition = "input.show_regression",
                          
                          h4("Multinomial Regression Results"),
                          fluidRow(
                            column(12,
                                   plotOutput("multinomial_plot", width = "100%", height = "600px")
                            )
                          ),
                          br(),
                          
                          h4("Linear Regression Results"),
                          
                          # Ache plots
                          h5("Outcome Variable: Ache"),
                          fluidRow(
                            column(6,
                                   plotOutput("ache_main_plot", width = "100%", height = "500px")
                            ),
                            column(6,
                                   plotOutput("ache_interaction_plot", width = "100%", height = "500px")
                            )
                          ),
                          br(),
                          
                          # Feeling plots
                          h5("Outcome Variable: Feeling"),
                          fluidRow(
                            column(6,
                                   plotOutput("feeling_main_plot", width = "100%", height = "500px")
                            ),
                            column(6,
                                   plotOutput("feeling_interaction_plot", width = "100%", height = "500px")
                            )
                          ),
                          br(),
                          
                          # Health plots
                          h5("Outcome Variable: Health"),
                          fluidRow(
                            column(6,
                                   plotOutput("health_main_plot", width = "100%", height = "500px")
                            ),
                            column(6,
                                   plotOutput("health_interaction_plot", width = "100%", height = "500px")
                            )
                          ),
                          br(),
                          
                          # Life Satisfaction plots
                          h5("Outcome Variable: Life Satisfaction"),
                          fluidRow(
                            column(6,
                                   plotOutput("lifesat_main_plot", width = "100%", height = "500px")
                            ),
                            column(6,
                                   plotOutput("lifesat_interaction_plot", width = "100%", height = "500px")
                            )
                          )
                        )
                      )
             ),
             
             # About Page
             tabPanel("About",
                      fluidPage(
                        h3("About the Project"),
                        br(),
                        p("This section will present the detailed information about the project, methods and personnel")
                      )
             ),
             
             # Add footer at the end
             div(class = "footer", 
                 "© 2025 Universität Zürich. All rights reserved.")
  )
)  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Descriptive Statistics
  # Helper function to filter data
  filterData <- function(data, country, surveyyear) {
    filtered_data <- data %>% filter(countryname == country)
    
    if (!is.null(surveyyear) && surveyyear != "" && surveyyear != "ALL") {
      # Try to match data types
      if (is.numeric(data$surveyyear)) {
        surveyyear_filter <- as.numeric(surveyyear)
      } else {
        surveyyear_filter <- as.character(surveyyear)
      }
      
      filtered_data <- filtered_data %>% filter(surveyyear == surveyyear_filter)
      
    }
    
    return(filtered_data)
  }
  
  # Descriptive statistics tables
  
  output$predictorStatsTable <- renderTable({
    req(input$country)
    
    # Define predictor variables in alphabetical order with full names
    predictor_variables <- c("Alcohol Consumption" = "alcohol",
                             "Smoking" = "smoking",
                             "Physical Inactivity" = "physinact",
                             "Sleep Problems" = "sleepprob",
                             "Unhealthy Diet" = "undiet")
    
    # Filter data for selected country and survey year
    country_data <- filterData(hbsc, input$country, input$surveyyear)
    
    # Calculate statistics for predictor variables
    stats_df <- data.frame()
    
    for (i in 1:length(predictor_variables)) {
      var_name <- names(predictor_variables)[i]  # Full name
      var_code <- predictor_variables[i]         # Variable code
      
      if (var_code %in% names(country_data)) {
        var_data <- country_data[[var_code]]
        n_participants <- nrow(country_data)
        n_na <- sum(is.na(var_data))
        response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
        mean_val <- round(mean(var_data, na.rm = TRUE), 2)
        sd_val <- round(sd(var_data, na.rm = TRUE), 2)
        
        stats_df <- rbind(stats_df, data.frame(
          Variable = var_name,  # Use full name instead of code
          Mean = mean_val,
          SD = sd_val,
          Participants = n_participants,
          `Missing Values` = n_na,
          `Response Rate (%)` = response_rate,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(stats_df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$outcomeStatsTable <- renderTable({
    req(input$country)
    
    # Define outcome variables in alphabetical order with full names
    outcome_variables <- c("Feelings" = "feeling",
                           "Life Satisfaction" = "lifesat",
                           "Physical Aches" = "ache",
                           "Self-rated Health" = "health"
    )
    
    # Filter data for selected country and survey year
    country_data <- filterData(hbsc, input$country, input$surveyyear)
    
    # Calculate statistics for outcome variables
    stats_df <- data.frame()
    
    for (i in 1:length(outcome_variables)) {
      var_name <- names(outcome_variables)[i]  # Full name
      var_code <- outcome_variables[i]         # Variable code
      
      if (var_code %in% names(country_data)) {
        var_data <- country_data[[var_code]]
        n_participants <- nrow(country_data)
        n_na <- sum(is.na(var_data))
        response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
        mean_val <- round(mean(var_data, na.rm = TRUE), 2)
        sd_val <- round(sd(var_data, na.rm = TRUE), 2)
        
        stats_df <- rbind(stats_df, data.frame(
          Variable = var_name,  # Use full name instead of code
          Mean = mean_val,
          SD = sd_val,
          Participants = n_participants,
          `Missing Values` = n_na,
          `Response Rate (%)` = response_rate,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(stats_df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Demographics plots
  
  # Default demographics plots (duplicates of your existing logic)
  output$ageHistogramDefault <- renderPlot({
    req(input$country)
    
    # Filter data for selected country and survey year
    country_data <- filterData(z_hbsc, input$country, input$surveyyear)
    
    # Remap age using agecat_map with correct column name
    country_data$age_category <- names(agecat_map)[match(country_data$agecat, agecat_map)]
    
    # Create title based on survey year selection
    plot_title <- if (!is.null(input$surveyyear) && input$surveyyear != "" && input$surveyyear != "ALL") {
      paste("Age Distribution -", input$surveyyear)
    } else {
      "Age Distribution"
    }
    
    # Create age bar plot with steelblue color and gray for NAs
    ggplot(country_data, aes(x = age_category, fill = is.na(age_category))) +
      geom_bar(color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      labs(title = plot_title,
           x = "Age Category",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5),
            legend.position = "none")
  })
  
  output$sexHistogramDefault <- renderPlot({
    req(input$country)
    
    # Filter data for selected country and survey year
    country_data <- filterData(z_hbsc, input$country, input$surveyyear)
    
    # Create title based on survey year selection
    plot_title <- if (!is.null(input$surveyyear) && input$surveyyear != "" && input$surveyyear != "ALL") {
      paste("Sex Distribution -", input$surveyyear)
    } else {
      "Sex Distribution"
    }
    
    # Create sex bar plot with steelblue color and gray for NAs
    country_data$sex_label <- factor(country_data$sex, levels = c(1, 2), labels = c("Male", "Female"))
    
    ggplot(country_data, aes(x = sex_label, fill = is.na(sex_label))) +
      geom_bar(color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      labs(title = plot_title,
           x = "Sex",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5),
            legend.position = "none")
  })
  
  # Variable section demographics plots (duplicates of your existing logic)
  output$ageHistogramVariable <- renderPlot({
    req(input$country)
    
    # Filter data for selected country and survey year
    country_data <- filterData(z_hbsc, input$country, input$surveyyear)
    
    # Remap age using agecat_map with correct column name
    country_data$age_category <- names(agecat_map)[match(country_data$agecat, agecat_map)]
    
    # Create title based on survey year selection
    plot_title <- if (!is.null(input$surveyyear) && input$surveyyear != "" && input$surveyyear != "ALL") {
      paste("Age Distribution -", input$surveyyear)
    } else {
      "Age Distribution"
    }
    
    # Create age bar plot with steelblue color and gray for NAs
    ggplot(country_data, aes(x = age_category, fill = is.na(age_category))) +
      geom_bar(color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      labs(title = plot_title,
           x = "Age Category",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5),
            legend.position = "none")
  })
  
  output$sexHistogramVariable <- renderPlot({
    req(input$country)
    
    # Filter data for selected country and survey year
    country_data <- filterData(z_hbsc, input$country, input$surveyyear)
    
    # Create title based on survey year selection
    plot_title <- if (!is.null(input$surveyyear) && input$surveyyear != "" && input$surveyyear != "ALL") {
      paste("Sex Distribution -", input$surveyyear)
    } else {
      "Sex Distribution"
    }
    
    # Create sex bar plot with steelblue color and gray for NAs
    country_data$sex_label <- factor(country_data$sex, levels = c(1, 2), labels = c("Male", "Female"))
    
    ggplot(country_data, aes(x = sex_label, fill = is.na(sex_label))) +
      geom_bar(color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "gray")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      labs(title = plot_title,
           x = "Sex",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 16, hjust = 0.5),
            legend.position = "none")
  })
  
  # Reactive expression for available years
  available_years <- reactive({
    req(input$country)
    
    years <- hbsc_label %>%
      filter(countryname == input$country) %>%
      distinct(surveyyear) %>%
      arrange(surveyyear) %>%
      pull(surveyyear)
    
    year_choices <- as.list(years)
    names(year_choices) <- years
    return(year_choices)
  })
  
  observeEvent(input$country, {
    if (input$country != "") {
      years <- available_years()
      
      if (!is.null(years) && length(years) > 0) {
        all_choices <- c("All Survey Years" = "ALL", years)
        
        updateSelectInput(session, "surveyyear",
                          choices = all_choices,
                          selected = "ALL"
        )
      }
    }
  })
  
  observeEvent(input$variable, {
    if (input$variable != "") {
      variable_choices <- list(
        "No Variable Selected" = "None",
        "Demographics" = list(
          "Family Affluence Score" = "fas"
        ),
        "Outcome Variables" = list(
          "Feelings" = "feeling",
          "Life Satisfaction" = "lifesat",
          "Physical Aches" = "ache",
          "Self-rated Health" = "health"
        ),
        "Health Behavior" = list(
          "Alcohol Consumption" = "alcohol",
          "Smoking" = "smoking",
          "Physical Inactivity" = "physinact",
          "Sleep Problems" = "sleepprob",
          "Unhealthy Diet" = "undiet"
        )
      )
      
      updateSelectInput(session, "variable",
                        choices = variable_choices,
                        selected = input$variable
      )
    }
  })
  
  # Reset surveyyear when a new variable for summary is chosen
  observeEvent(input$variable, {
    updateSelectInput(session, "surveyyear", selected = "ALL")
  })
  
  # Variable definitions output
  output$variableDefinition <- renderText({
    req(input$variable)
    
    definitions <- list(
      "fas" = "The <strong>Family Affluence Scale (FAS)</strong> is a composite measure of socioeconomic status based on material assets. It includes questions about family car ownership, having own bedroom, number of computers, and family holidays. Higher scores indicate higher family affluence.",
      
      "health" = "<strong>Self-rated Health</strong> measures how adolescents perceive their overall health status on a scale from poor to excellent. This subjective health measure is a strong predictor of future health outcomes and healthcare utilization.",
      
      "lifesat" = "<strong>Life Satisfaction</strong> is measured using the Cantril Ladder, where respondents place themselves on a scale from 0 (worst possible life) to 10 (best possible life). This measure captures overall subjective well-being and life satisfaction.",
      
      "ache" = "<strong>Physical Aches</strong> includes three somatic health complaints: backache, headache, and stomachache. These are measured by frequency of occurrence in the past 6 months and are indicators of psychosomatic health problems.",
      
      "feeling" = "<strong>Psychological Feelings</strong> encompasses emotional and psychological symptoms including feeling dizzy, irritable, nervous, and feeling low. These measures capture psychological distress and emotional well-being.",
      
      "sleepprob" = "<strong>Sleep Problems</strong> measures the frequency of sleep difficulties experienced by adolescents in the past 6 months. Sleep quality is crucial for physical and mental health, academic performance, and overall well-being.",
      
      "undiet" = "<strong>Unhealthy Diet</strong> includes consumption patterns of sweets, soft drinks, vegetables, and fruits. These measures assess adherence to healthy eating patterns and consumption of foods that may impact health outcomes.",
      
      "physinact" = "<strong>Physical Inactivity</strong> measures the number of days per week adolescents are physically active for at least 60 minutes. Physical activity is essential for healthy development and prevention of chronic diseases.",
      
      "alcohol" = "<strong>Alcohol Consumption</strong> assesses current drinking frequency and patterns. This includes general alcohol use frequency and, for recent surveys, number of drinking days in the past month.",
      
      "smoking" = "<strong>Smoking Behavior</strong> measures current tobacco use frequency and patterns. This includes general smoking frequency and, for recent surveys, number of smoking days in the past month."
    )
    
    # Return the definition for the selected variable
    if (input$variable %in% names(definitions)) {
      return(definitions[[input$variable]])
    } else {
      return("No definition available for this variable.")
    }
  })
  
  # Variable summary table (when only variable is selected)
  output$variableSummaryTable <- renderTable({
    req(input$country, input$variable)
    
    # Filter data for selected country
    country_data <- hbsc %>% 
      filter(countryname == input$country)
    
    # Calculate statistics for selected variable
    if (input$variable %in% names(country_data)) {
      var_data <- country_data[[input$variable]]
      n_participants <- nrow(country_data)
      n_na <- sum(is.na(var_data))
      response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
      mean_val <- round(mean(var_data, na.rm = TRUE), 2)
      sd_val <- round(sd(var_data, na.rm = TRUE), 2)
      min_val <- round(min(var_data, na.rm = TRUE), 2)
      max_val <- round(max(var_data, na.rm = TRUE), 2)
      median_val <- round(median(var_data, na.rm = TRUE), 2)
      
      stats_df <- data.frame(
        Statistic = c("Mean", "Standard Deviation", "Median", "Minimum", "Maximum", 
                      "Total Participants", "Missing Values", "Response Rate (%)"),
        Value = c(mean_val, sd_val, median_val, min_val, max_val, 
                  n_participants, n_na, response_rate),
        stringsAsFactors = FALSE
      )
      
      return(stats_df)
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Variable and year summary table (when both variable and year are selected)
  output$variableYearSummaryTable <- renderTable({
    req(input$country, input$variable, input$surveyyear)
    
    # Filter data for selected country and survey year
    country_year_data <- hbsc %>% 
      filter(countryname == input$country, surveyyear == as.numeric(input$surveyyear))
    
    # Calculate statistics for selected variable and year
    if (input$variable %in% names(country_year_data)) {
      var_data <- country_year_data[[input$variable]]
      n_participants <- nrow(country_year_data)
      n_na <- sum(is.na(var_data))
      response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
      mean_val <- round(mean(var_data, na.rm = TRUE), 2)
      sd_val <- round(sd(var_data, na.rm = TRUE), 2)
      min_val <- round(min(var_data, na.rm = TRUE), 2)
      max_val <- round(max(var_data, na.rm = TRUE), 2)
      median_val <- round(median(var_data, na.rm = TRUE), 2)
      
      stats_df <- data.frame(
        Statistic = c("Mean", "Standard Deviation", "Median", "Minimum", "Maximum", 
                      "Total Participants", "Missing Values", "Response Rate (%)"),
        Value = c(mean_val, sd_val, median_val, min_val, max_val, 
                  n_participants, n_na, response_rate),
        stringsAsFactors = FALSE
      )
      
      return(stats_df)
    }
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Generate histograms for variable and year selection
  output$histogramPlots <- renderUI({
    req(input$country, input$variable, input$surveyyear)
    
    # Generate plots using your function
    plots <- desc_histograms(
      data = hbsc_label, 
      countryname = input$country, 
      surveyyear = as.numeric(input$surveyyear), 
      variable = input$variable
    )
    
    # Create plot outputs using full width
    plot_outputs <- lapply(seq_along(plots), function(i) {
      plot_name <- paste0("plot_", i)
      
      # Create renderPlot for each histogram
      output[[plot_name]] <- renderPlot({
        plots[[i]]
      })
      
      # Return each plot in full width with some spacing
      div(
        plotOutput(plot_name, height = "400px"),
        style = "margin-bottom: 30px;"
      )
    })
    
    # Return plots stacked vertically, each taking full width
    if (length(plot_outputs) > 0) {
      do.call(tagList, plot_outputs)
    } else {
      div(
        style = "text-align: center; padding: 50px; border: 2px dashed #ccc; margin: 20px 0;",
        h5("No plots available for this selection"),
        p("Please try a different variable or survey year combination.")
      )
    }
  })
  
  ### World map
  output$mapPlot <- renderLeaflet({
    req(input$map_variable, input$year_slider)
    
    # Generate the map with your existing function
    map <- generate_map(input$map_variable, input$year_slider)
    
  })
  
  ### LPA Analysis and Regression
  
  # Generate all plots when country or year changes
  all_plots <- reactive({
    req(input$lpa_country != "")  
    
    create_all_plots(input$lpa_country, input$lpa_year)
  })
  
  # Dynamic titles for comparison
  output$country1_title <- renderText({
    if (input$compare_countries && !is.null(input$lpa_country) && input$lpa_country != "") {
      paste(input$lpa_country, ifelse(!is.null(input$lpa_year) && input$lpa_year != "", paste0(" (", input$lpa_year, ")"), ""))
    } else {
      ""
    }
  })
  
  output$country2_title <- renderText({
    if (input$compare_countries && !is.null(input$lpa_country_compare) && input$lpa_country_compare != "") {
      paste(input$lpa_country_compare, ifelse(!is.null(input$lpa_year) && input$lpa_year != "", paste0(" (", input$lpa_year, ")"), ""))
    } else {
      ""
    }
  })
  
  # Reactive expression for available years for LPA
  available_lpa_years <- reactive({
    req(input$lpa_country)
    
    years <- hbsc_label %>%
      filter(countryname == input$lpa_country) %>%
      distinct(surveyyear) %>%
      arrange(surveyyear) %>%
      pull(surveyyear)
    
    year_choices <- as.list(years)
    names(year_choices) <- years
    return(year_choices)
  })
  
  observeEvent(input$lpa_country, {
    if (input$lpa_country != "") {
      years <- available_lpa_years()
      
      if (!is.null(years) && length(years) > 0) {
        all_choices <- c("All Survey Years" = "ALL", years)
        
        updateSelectInput(session, "lpa_year",
                          choices = all_choices,
                          selected = "ALL"
        )
      }
    } else {
      # Reset to default when no country selected
      updateSelectInput(session, "lpa_year",
                        choices = c("All Survey Years" = "ALL", "Select a country first" = "none"),
                        selected = "ALL"
      )
    }
  })
  
  # LPA Plot
  output$lpa_plot <- renderPlot({
    plots <- all_plots()
    plots$lpa_plot
  })
  
  # LPA Plot for single view (when not comparing)
  output$lpa_plot_single <- renderPlot({
    plots <- all_plots()
    plots$lpa_plot
  })
  
  # LPA Plot for comparison country (just this one plot)
  output$lpa_plot_compare <- renderPlot({
    req(input$compare_countries, input$lpa_country_compare != "")
    
    create_lpa_plot(input$lpa_country_compare, input$lpa_year)
  })
  
  # Multinomial Regression Plot
  output$multinomial_plot <- renderPlot({
    plots <- all_plots()
    plots$multinomial_plot
  })
  
  # Linear Regression Plots - Main Effects
  output$ache_main_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_main$ache
  })
  
  output$feeling_main_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_main$feeling
  })
  
  output$health_main_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_main$health
  })
  
  output$lifesat_main_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_main$lifesat
  })
  
  # Linear Regression Plots - With Interaction
  output$ache_interaction_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_interaction$ache
  })
  
  output$feeling_interaction_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_interaction$feeling
  })
  
  output$health_interaction_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_interaction$health
  })
  
  output$lifesat_interaction_plot <- renderPlot({
    plots <- all_plots()
    plots$linear_plots_interaction$lifesat
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)