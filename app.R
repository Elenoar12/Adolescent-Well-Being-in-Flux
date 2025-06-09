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
  filter(!(countryname %in% c("Belgium (Flemish)", "Belgium (French)")))

# Find the records in hbsc_mean that need geometries
bel_records <- hbsc_mean %>%
  filter(countryname %in% c("Belgium (Flemish)", "Belgium (French)"))

# Get Belgium regions (Level 1: Flemish, Walloon, Brussels)
bel_regions <- gisco_get_nuts(year = "2021", nuts_level = 1, country = "BE") %>% 
  select(NAME_LATN, geometry)

# Extract the Flemish Region separately
bel_flem <- bel_regions %>% 
  filter(NAME_LATN == "Vlaams Gewest") %>% 
  mutate(NAME_LATN = "Belgium (Flemish)") 

# Merge Walloon & Brussels into one geometry "Belgium (French)"
bel_fren <- bel_regions %>%
  filter(NAME_LATN %in% c("Région wallonne", "Région de Bruxelles-Capitale/Brussels Hoofdstedelijk Gewest")) %>%
  summarise(NAME_LATN = "Belgium (French)")

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
    setView(lng = -30, lat = 45, zoom = 4) %>%  # Updated view over Atlantic Ocean
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

create_radarchart <- function(cprob, color = "red", 
                              vlabels = colnames(data), vlcex = 1,
                              caxislabels = NULL, title = NULL, ...){
  
  cprob <- read.csv(cprob)
  
  # Make sure class variable is a factor
  cprob$C <- as.factor(cprob$C)
  
  # Identify the most frequent profile
  most_frequent_profile <- names(sort(table(cprob$C), decreasing = TRUE))[1]
  
  # Relevel so the most frequent is first (reference level)
  cprob$C <- relevel(cprob$C, ref = most_frequent_profile)
  
  # Group by class and calculate means
  spider_df <- cprob %>%
    group_by(C) %>%
    summarise(across(c(PHY, SLE, UND, SM, ALC), mean, na.rm = TRUE), .groups = "drop")
  
  # Ensure rows are ordered according to factor levels (Class 1 = most frequent)
  spider_df <- spider_df %>%
    mutate(C = factor(C, levels = levels(cprob$C))) %>%
    arrange(C)
  
  # Global min and max across all numeric columns
  global_max <- max(as.matrix(spider_df[, -1]))
  global_min <- min(as.matrix(spider_df[, -1]))
  
  if (is.null(caxislabels)) {
    caxislabels <- round(seq(global_min, global_max, length.out = 5), digits = 3)
  }
  
  # Create max and min rows
  max_row <- spider_df[1, ]
  max_row[1, ] <- NA
  max_row[1, -1] <- global_max
  
  min_row <- spider_df[1, ]
  min_row[1, ] <- NA
  min_row[1, -1] <- global_min
  
  # Add max and min rows, then set row names
  radar_data <- as.data.frame(rbind(max_row, min_row, spider_df))
  rownames(radar_data) <- c("Max", "Min", paste0("Class ", spider_df$C))
  radar_data$C <- NULL
  
  radarchart(
    radar_data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.2), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
  
  legend(
    x = "topright",
    legend = paste0("Class ", spider_df$C), 
    col = color, 
    lty = 1, 
    lwd = 2,
    bty = "n"
  )
}

### Integrate labelling process to save disk space
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
  # Undietary behavior
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
  
  # Create histogram for a single variable
  p <- filtered_data %>%
    mutate(!!variable_name := factor(.data[[variable_name]], 
                                     levels = ordered_values,
                                     exclude = NULL)) %>%
    ggplot(aes_string(x = variable_name)) +
    geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
    labs(
      title = title,
      subtitle = paste("Variable:", variable_name),
      x = variable_name,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),      # Increased from 10
      plot.subtitle = element_text(size = 12),                  # Increased from 8
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Increased from 8
      axis.text.y = element_text(size = 12),                    # Increased from 8
      axis.title = element_text(size = 14, face = "bold"),      # Increased from 9
      axis.title.x = element_text(margin = margin(t = 10)),     # Add spacing
      axis.title.y = element_text(margin = margin(r = 10)),     # Add spacing
      plot.margin = margin(20, 20, 20, 20)                     # Add plot margins
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

agecat_map <- c("11 y/o" = 1, "13 y/o" = 2, "15 y/o" = 3)

country <- unique(hbsc$countryname)

survey_years <- unique(hbsc$surveyyear)

# Define UI
ui <- fluidPage(
  titlePanel("Bechtiger & Janousch (2025): Adolescent Well-Being in Flux"),
  
  # Navigation bar for different pages
  navbarPage("Menu", selected = "Map",
             
             # Home Page
             tabPanel("Home",
                      fluidPage(
                        h2("Welcome to the Adolescent Well-Being in Flux App"),
                        p("This app provides insights into adolescent well-being through descriptive statistics and latent profile analysis (LPA)."),
                        p("Use the navigation menu to explore the data.")
                      )
             ),
             
             # Map Page
             tabPanel("Map",
                      # Set max height for the page to fill browser window
                      tags$style(type = "text/css", "html, body {height: 100%} 
                                 #mapContainer {height: calc(90vh - 200px);}"),
                      fluidPage(
                        h3("Health Indicators Over Time"),
                        # Health indicator buttons above the map
                        fluidRow(
                          column(12, align = "center",
                                 radioButtons("map_variable", "Select Health Indicator:", 
                                              choices = pred_vars,
                                              inline = TRUE)
                          )
                        ),
                        # Map takes full width and most of the height
                        fluidRow(
                          column(12, 
                                 div(id = "mapContainer",
                                     leafletOutput("mapPlot", height = "100%", width = "100%")
                                 )
                          )
                        ),
                        # Year slider below the map
                        fluidRow(
                          column(12, align = "center",
                                 sliderInput("year_slider", "Select Survey Year:",
                                             min = min(hbsc_map$surveyyear, na.rm = TRUE), 
                                             max = max(hbsc_map$surveyyear, na.rm = TRUE),
                                             value = min(hbsc_map$surveyyear, na.rm = TRUE), 
                                             step = 4, sep = "",
                                             width = "80%")
                                )
                                )
                                )
             ),
             
             # Descriptive Statistics Page
             tabPanel("Descriptive Statistics",
                      fluidPage(
                        selectInput("country", "Select Country:",
                                    choices = c("", country),
                                    selected = "Switzerland"),
                        h3("Descriptive Statistics"),
                        tags$div(style = "display: flex; flex-wrap: wrap;",
                                 tags$div(style = "flex: 0 0 auto; min-width: 300px; margin-right: 30px;",
                                          h4("Predictor Variables"),
                                          tableOutput("predictorStatsTable")
                                 ),
                                 tags$div(style = "flex: 0 0 auto; min-width: 250px;",
                                          h4("Outcome Variables"),
                                          tableOutput("outcomeStatsTable")
                                 )
                        ),
                        br(),
                        h4("Demographics"),
                        plotOutput("ageHistogram", height = "450px"),
                        br(),
                        plotOutput("sexHistogram", height = "450px"),
                        br(),
                        h3("Variable Composition"),
                        # Variable selection with proper spacing
                        tags$div(style = "display: flex; flex-wrap: wrap;",
                                 tags$div(style = "flex: 0 0 auto; min-width: 300px; margin-right: 30px;",
                                          selectInput("variable", "Select Variable:",
                                                      choices = list(
                                                        "Demographics" = list(
                                                          "Family Affluence Score" = "fas"
                                                        ),
                                                        "Outcome Variables" = list(
                                                          "Self-rated Health" = "health",
                                                          "Life Satisfaction" = "lifesat",
                                                          "Physical Aches" = "ache",
                                                          "Feelings" = "feeling"
                                                        ),
                                                        "Health Behavior" = list(
                                                          "Sleep Problems" = "sleepprob",
                                                          "Undietary Behavior" = "undiet",
                                                          "Physical Inactivity" = "physinact",
                                                          "Alcohol Consumption" = "alcohol",
                                                          "Smoking" = "smoking"
                                                        )
                                                      ),
                                                      selected = ""
                                          )
                                 ),
                                 tags$div(style = "flex: 0 0 auto; min-width: 250px;",
                                          selectInput("surveyyear", "Select Survey Year:",
                                                      choices = c("Select a country first" = ""),
                                                      selected = ""
                                                      )
                                          )
                        ),
                        br(),
                        # Conditional panel for variable summary (when only variable is selected)
                        conditionalPanel(
                          condition = "input.variable != '' && input.surveyyear == ''",
                          # Variable definition/description
                          div(
                            style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin-bottom: 20px;",
                            h5("Variable Definition", style = "margin-top: 0; color: #007bff;"),
                            htmlOutput("variableDefinition")
                              ),
                          h4("Variable Summary"),
                          tableOutput("variableSummaryTable")
                        ),
                        # Conditional panel for variable and year summary (when both are selected)
                        conditionalPanel(
                          condition = "input.variable != '' && input.surveyyear != ''",
                          h4("Variable Summary by Year"),
                          tableOutput("variableYearSummaryTable"),
                          br(),
                          h4("Response Plots"),
                          uiOutput("histogramPlots")
                          )
                        )
             ),
             
             # LPA Page
             tabPanel("LPA",
                      fluidPage(
                        h3("Latent Profile Analysis"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("lpa_country", "Select Country:",
                                        choices = c("", country),
                                        selected = "Switzerland"),
                            selectInput("lpa_year", "Select Survey Year:",
                                        choices = c("All Years" = "", survey_years),
                                        selected = "")
                          ),
                          mainPanel(
                            plotOutput("lpaSpiderPlot", width = "100%", height = "600px")
                          )
                        )
                      )
             ),
             
             # About Page
             tabPanel("About",
                      fluidPage(
                        h3("About the Project"),
                        p("This section will present the detailed information about the project, methods and personnel")
                                )
                      )
             )
  )  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Descriptive statistics
  
  output$predictorStatsTable <- renderTable({
    req(input$country)
    
    # Define predictor variables
    predictor_variables <- c("physinact", "sleepprob", "undiet", "smoking", "alcohol")
    
    # Filter data for selected country
    country_data <- hbsc %>% 
      filter(countryname == input$country)
    
    # Calculate statistics for predictor variables
    stats_df <- data.frame()
    
    for (var in predictor_variables) {
      if (var %in% names(country_data)) {
        var_data <- country_data[[var]]
        n_participants <- nrow(country_data)
        n_na <- sum(is.na(var_data))
        response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
        mean_val <- round(mean(var_data, na.rm = TRUE), 2)
        sd_val <- round(sd(var_data, na.rm = TRUE), 2)
        
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
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
    
    # Define outcome variables
    outcome_variables <- c("health", "lifesat", "feeling", "ache")
    
    # Filter data for selected country
    country_data <- hbsc %>% 
      filter(countryname == input$country)
    
    # Calculate statistics for outcome variables
    stats_df <- data.frame()
    
    for (var in outcome_variables) {
      if (var %in% names(country_data)) {
        var_data <- country_data[[var]]
        n_participants <- nrow(country_data)
        n_na <- sum(is.na(var_data))
        response_rate <- round(((n_participants - n_na) / n_participants) * 100, 1)
        mean_val <- round(mean(var_data, na.rm = TRUE), 2)
        sd_val <- round(sd(var_data, na.rm = TRUE), 2)
        
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
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
  
  output$ageHistogram <- renderPlot({
    req(input$country)
    
    # Filter data for selected country
    country_data <- z_hbsc %>% 
      filter(countryname == input$country)
    
    # Remap age using agecat_map with correct column name
    country_data$age_category <- names(agecat_map)[match(country_data$agecat, agecat_map)]
    
    # Create age bar plot with different colors
    ggplot(country_data, aes(x = age_category, fill = age_category)) +
      geom_bar(alpha = 0.8) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("11 y/o" = "#1f77b4", "13 y/o" = "#ff7f0e", "15 y/o" = "#9467bd")) +
      labs(title = "Age Distribution",
           x = "Age Category",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none")
  })
  
  output$sexHistogram <- renderPlot({
    req(input$country)
    
    # Filter data for selected country
    country_data <- z_hbsc %>% 
      filter(countryname == input$country)
    
    # Create sex bar plot with gender neutral colors
    ggplot(country_data, aes(x = factor(sex), fill = factor(sex))) +
      geom_bar(alpha = 0.8) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("1" = "#F7EA52", "2" = "#4CAF50"), 
                        labels = c("1" = "Male", "2" = "Female")) +
      scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
      labs(title = "Sex Distribution",
           x = "Sex",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.5),
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
  
  # Observer to update survey year choices
  observe({
    updateSelectInput(session, "surveyyear",
                      choices = c("All Years" = "", available_years()),
                      selected = ""
    )
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
      
      "undiet" = "<strong>Dietary Behaviors</strong> includes consumption patterns of sweets, soft drinks, vegetables, and fruits. These measures assess adherence to healthy eating patterns and consumption of foods that may impact health outcomes.",
      
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
  # LPA Spider Plot
  output$lpaSpiderPlot <- renderPlot({
    req(input$lpa_country != "")  # Wait until a country is selected
    
    # Dynamically build path
    if (input$lpa_year == "") {
      cprob_path <- file.path(
        "C:/Users/hanst/OneDrive - Universität Zürich UZH/Datenanalyse",
        input$lpa_country,
        paste0("c_prob_", input$lpa_country, "_C4.csv")
      )
    } else {
      cprob_path <- file.path(
        "C:/Users/hanst/OneDrive - Universität Zürich UZH/Datenanalyse",
        input$lpa_country,
        paste0("SurveyYear_", input$lpa_year),
        paste0("c_prob_", input$lpa_country, "_", input$lpa_year, "_C4.csv")
      )
    }
    
    # Optional: check file existence
    if (!file.exists(cprob_path)) {
      showNotification("Data file not found.", type = "error")
      return(NULL)
    }
    
    # Read and plot radar chart
    create_radarchart(cprob_path,
                      color = c("red", "blue", "green", "purple"),
                      title = paste("Latent Classes -", input$lpa_country, input$lpa_year))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)