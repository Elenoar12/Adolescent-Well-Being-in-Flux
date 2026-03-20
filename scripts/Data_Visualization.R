library(readxl)
library(tidyverse)
library(leaflet)
library(sf)
library(rnaturalearth)
library(giscoR)
library(dplyr)
library(ggplot2)
library(ggdist)   # for stat_slab if needed
library(viridis)
library(ggtext)   # for element_markdown if needed
library(MplusAutomation)
library(reshape2)
library(glue)
library(tibble)

data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_allrel.csv")
hbsc <- read.csv(data_path, header=TRUE)

# Select variables beforehand
pred_vars <- c("physinact", "sleepprob", "undiet", "smoking", "alcohol")

# Z-Standardized data per country before summary stats
z_hbsc <- hbsc %>%
  group_by(countryname) %>%
  mutate(across(all_of(pred_vars),
                ~ scale(.) %>%
                  as.vector())) %>%
  ungroup()


# filter and reshape the data
raindrop <- z_hbsc %>%
  filter(countryname == 'Switzerland') %>% 
  select(all_of(pred_vars), surveyyear) %>%
  pivot_longer(cols = all_of(pred_vars), names_to = "variable") %>% 
  mutate(surveyyear = factor(surveyyear),
         variable = factor(variable, levels = pred_vars))

raindrop_counts <- raindrop %>%
  group_by(surveyyear, variable, value) %>%
  summarise(n = n(), .groups = "drop")
  
##-------------------------- base plot

# Create the plot with proper legend handling
p1 <- ggplot(raindrop, aes(x = variable, y = value)) +
  
  # POINTS with custom legend - map color but suppress its legend
  geom_point(
    data = raindrop_counts,
    aes(size = n, color = variable), # Keep color mapping
    shape = 21, 
    fill = NA, 
    stroke = 0.7, 
    alpha = 0.6,
    position = position_dodge(width = 0.8),
    show.legend = c(color = FALSE, size = TRUE) # Only show size legend, not color
  ) +
  
  # BOXES with fill aesthetic
  geom_boxplot(
    aes(fill = variable),
    width = 0.28, 
    alpha = 0.7, 
    outlier.alpha = 0,
    lwd = 0.3, 
    position = position_dodge(width = 0.8)
  ) +
  
  facet_wrap(~ surveyyear, ncol = 5) +
  
  labs(
    title = "Switzerland | Variable Distributions by Survey Year",
    y = "z-score"
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 12),
    axis.title.y = element_text(size = 9, color = "grey60"),
    axis.text.y = element_text(size = 9, color = "grey60"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.text = ggtext::element_markdown(size = 11),
    legend.box = "vertical",
    strip.text = element_text(size = 11, color = "white"),
    strip.background = element_rect(fill = "grey10")
  ) +
  
  # Keep both scales to ensure matching colors
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "magma") +
  scale_fill_viridis_d(begin = 0.2, end = 0.8, option = "magma", name = "Variable") +
  
  # Improved size scale to show the full range of values
  scale_size_continuous(
    range = c(1.5, 5), 
    name = "Count (n)",
    breaks = seq(500, 7500, length.out = 8),
    labels = function(x) round(x)
  ) +
  
  # Position the legends correctly
  guides(
    size = guide_legend(position = "left", 
                        override.aes = list(shape = 21, fill = NA, stroke = 0.7)),
    fill = guide_legend(position = "bottom",
                        override.aes = list(alpha = 0.7))
  )
p1


### Spider plot

library("fmsb")

cprob <- file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "Switzerland", "c_prob_Switzerland_C4.csv")

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

create_radarchart(cprob, 
                  color = c("red", "blue", "green", "purple"), 
                  title = "Klassenzugehörigkeit nach Variabelausprägung")

### LPA line plots combined using facet
create_switzerland_lpa_facets <- function() {
  
  # Load necessary data
  hbsc_labels <- read_excel("data/hbsc_labels.xlsx")
  
  # Define the master lookup table for all possible profile labels
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use", 
              "Moderate risk (mixed)", 
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:7,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#fc8d59", "#2e6f8e", "#8856a7", "#482173"),
    linetype = c("solid", "dashed", "longdash", "dotdash", "dotted", "dashed", "solid"),
    shape = c(16, 17, 18, 15, 4, 8, 16),
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
  
  # Convert hbsc_labels to profile mapping
  convert_to_profile_mapping <- function(df) {
    result <- list()
    for (i in 1:nrow(df)) {
      country <- df$Country[i]
      result[[country]] <- list()
      
      # Add "ALL" (Overall profiles)
      overall_cols <- paste0("Overall_Profile_", 1:5)
      overall_values <- as.character(df[i, overall_cols])
      overall_values <- overall_values[!is.na(overall_values) & overall_values != "" & overall_values != "NA"]
      if (length(overall_values) > 0) {
        result[[country]][["ALL"]] <- overall_values
      }
      
      # Add year-specific profiles
      years <- c("2002", "2006", "2010", "2014", "2018")
      for (year in years) {
        year_cols <- paste0(year, "_Profile_", 1:5)
        year_values <- as.character(df[i, year_cols])
        year_values <- year_values[!is.na(year_values) & year_values != "" & year_values != "NA"]
        if (length(year_values) > 0) {
          result[[country]][[year]] <- year_values
        }
      }
    }
    return(result)
  }
  
  profile_mapping <- convert_to_profile_mapping(hbsc_labels)
  
  # Define the years you want to include
  years <- c("ALL", "2002", "2006", "2010", "2014", "2018")
  input_country <- "Switzerland"
  
  # Get class solution
  class_solution <- hbsc_labels %>% filter(Country == input_country) %>% pull(ClassSolution)
  class_num <- gsub("C", "", class_solution)
  class_num_numeric <- as.numeric(class_num)
  
  # Initialize empty list to store data
  all_data <- list()
  
  # Read data for each year
  for(year in years) {
    tryCatch({
      # Construct file paths
      if (year != "ALL") {
        mplus_folder_path <- paste0("data/LPA ID/", input_country, "/", year)
        csv_filename <- paste0(tolower(input_country), "_", year, "_c", class_num, ".csv")
      } else {
        mplus_folder_path <- paste0("data/LPA ID/", input_country)
        csv_filename <- paste0(tolower(input_country), "_c", class_num, ".csv")
      }
      
      csv_filepath <- file.path(mplus_folder_path, csv_filename)
      
      # Read the CSV file directly
      means_df_filtered <- read.csv(csv_filepath)
      
      # Rename variables
      means_df_filtered$Variable <- gsub("undietary behavior", "Unhealthy Diet", means_df_filtered$Variable, ignore.case = TRUE)
      means_df_filtered$Variable <- gsub("alcohol", "Alcohol Consumption", means_df_filtered$Variable, ignore.case = TRUE)
      
      # Set the desired order for x-axis variables
      desired_order <- c("Alcohol Consumption", "Smoking", "Physical Inactivity", "Sleep Problems", "Unhealthy Diet")
      means_df_filtered$Variable <- factor(means_df_filtered$Variable, levels = desired_order)
      
      # Convert LatentClass to factor
      means_df_filtered$LatentClass <- factor(means_df_filtered$LatentClass,
                                              levels = 1:class_num_numeric)
      
      # Check if country and year exist in profile mapping
      year_key <- ifelse(year == "ALL", "ALL", year)
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
            return(label)
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
        means_df_filtered$ProfileOrder <- sapply(means_df_filtered$ProfileCategory, function(cat) {
          order_val <- relevant_styles$order[relevant_styles$label == cat]
          if (length(order_val) > 0) {
            return(order_val[1])
          } else {
            return(NA)
          }
        }, USE.NAMES = FALSE, simplify = TRUE)
        
        # Ensure ProfileOrder is numeric
        means_df_filtered$ProfileOrder <- as.numeric(means_df_filtered$ProfileOrder)
        
        # Order the ProfileLabelWithProp by the category order
        ordered_data <- means_df_filtered[order(means_df_filtered$ProfileOrder), ]
        ordered_levels <- unique(ordered_data$ProfileLabel)
        
        # Key change: Keep ProfileLabel as factor for styling
        means_df_filtered$ProfileLabel <- factor(
          means_df_filtered$ProfileLabel,
          levels = ordered_levels
        )
        
        # But also keep ProfileLabelWithProp for reference
        means_df_filtered$ProfileLabelWithProp <- paste0(
          means_df_filtered$ProfileLabel, 
          " (", 
          means_df_filtered$Proportion, 
          "%)"
        )
        
      } else {
        # Fallback to existing labeling logic
        means_df_filtered$ProfileLabel <- factor(
          paste("Profile", means_df_filtered$LatentClass),
          levels = paste("Profile", 1:class_num_numeric)
        )
        
        means_df_filtered$ProfileLabelWithProp <- paste0(
          means_df_filtered$ProfileLabel, 
          " (", 
          means_df_filtered$Proportion, 
          "%)"
        )
        
        means_df_filtered$ProfileCategory <- as.character(means_df_filtered$ProfileLabel)
      }
      
      # Reshape the data to long format for plotting
      means_long <- melt(means_df_filtered, id.vars = c("Variable", "LatentClass", "ProfileLabel", "ProfileLabelWithProp", "ProfileCategory"),
                         measure.vars = "Estimate")
      
      # Add Year column
      means_long$Year <- ifelse(year == "ALL", "Overall", year)
      
      # Store in list
      all_data[[year]] <- means_long
      
    }, error = function(e) {
      cat("Warning: Could not read data for year", year, "\n")
    })
  }
  
  # Combine all data
  combined_data <- do.call(rbind, all_data)
  
  # Set the order of facets with "All Years" first
  combined_data$Year <- factor(combined_data$Year, 
                               levels = c("Overall", "2002", "2006", "2010", "2014", "2018"))
  
  # CRITICAL FIX: Set ProfileCategory as ordered factor with proper levels
  combined_data$ProfileCategory <- factor(
    combined_data$ProfileCategory,
    levels = profile_styles$label,
    ordered = TRUE
  )
  
  # Create scale mappings based on ProfileCategory (base labels)
  # Get all unique ProfileCategory values in the correct order
  all_unique_categories <- levels(combined_data$ProfileCategory)[levels(combined_data$ProfileCategory) %in% combined_data$ProfileCategory]
  
  # Match to profile_styles lookup table
  style_indices <- match(all_unique_categories, profile_styles$label)
  
  # Create named vectors for the scales using ProfileCategory
  color_values <- profile_styles$color[style_indices]
  names(color_values) <- all_unique_categories
  
  linetype_values <- profile_styles$linetype[style_indices]
  names(linetype_values) <- all_unique_categories
  
  shape_values <- profile_styles$shape[style_indices]
  names(shape_values) <- all_unique_categories
  
  # Create the combined faceted plot using ProfileCategory for styling and ProfileLabelWithProp for grouping
  plot <- ggplot(combined_data, aes(x = Variable, y = value, 
                                    group = ProfileLabelWithProp, 
                                    color = ProfileCategory,
                                    linetype = ProfileCategory, 
                                    shape = ProfileCategory)) +
    geom_point(size = 8) +
    geom_line(linewidth = 1.5) +
    facet_wrap(~ Year, scales = "free_x") +
    labs(
      title = paste0("Latent Profile Analysis ", input_country, " - Overall and Individual Survey Years"),
      x = "Health Behaviors",
      y = "Means",
      color = "Risk Profile",
      linetype = "Risk Profile",
      shape = "Risk Profile"
    ) +
    scale_y_continuous(limits = c(-1, 7)) +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_linetype_manual(values = linetype_values, drop = FALSE) +
    scale_shape_manual(values = shape_values, drop = FALSE) +
    theme_minimal() +
    theme(
      title = element_text(size = 32),
      plot.title = element_text(margin = margin(b = 15)),
      text = element_text(size = 28),
      strip.text = element_text(size = 28, face = "bold"),
      axis.title.x = element_text(size = 24, margin = margin(t = 10)),
      axis.title.y = element_text(size = 24, margin = margin(r = 10)),
      axis.text.x = element_text(size = 28, angle = 45, hjust = 1, color = "black"), 
      axis.text.y = element_text(size = 28),
      legend.key.width = unit(1, "line"),
      legend.text = element_text(size = 28),
      legend.title = element_text(size = 28),
      legend.position = "top",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.direction = "horizontal"
    )
  
  # Create mini-legends within each facet showing proportions
  legend_data <- combined_data %>%
    group_by(Year, ProfileCategory) %>%
    summarise(
      Proportion = first(gsub(".*\\((.*?)%\\).*", "\\1", ProfileLabelWithProp)),
      .groups = "drop"
    ) %>%
    arrange(Year, ProfileCategory) %>%  # This will now use the factor order
    group_by(Year) %>%
    mutate(
      x_point = seq(1.5, by = 1, length.out = n()),
      y_point = 6.3,
      x_text = x_point + 0.1,
      y_text = y_point + 0.05,
      label = paste0(Proportion, "%")
    ) %>%
    ungroup()
  
  # Add legend points and text to each facet
  plot <- plot +
    geom_point(data = legend_data,
               aes(x = x_point, y = y_point, color = ProfileCategory, shape = ProfileCategory),
               size = 10, inherit.aes = FALSE) +
    geom_text(data = legend_data,
              aes(x = x_text, y = y_text, label = label),
              size = 10, hjust = 0, vjust = 0.5, inherit.aes = FALSE, color = "black")
  
  return(plot)
}

# Run the function
create_switzerland_lpa_facets()

### LPA line plots for Sensitivity Analysis

create_sensitivity_lpa_facets <- function(hbsc_def, hbsc_labels, country) {
  
  input_country <- country
  
  # Get class solution for the country
  class_solution <- hbsc_labels %>% 
    filter(Country == country) %>% 
    pull(ClassSolution)
  
  class_num <- gsub("C", "", class_solution)
  class_num_numeric <- as.numeric(class_num)
  
  # Path to country-level cprob file
  cprob_path <- file.path("LPA ID", country, paste0("c_prob_", country, "_", class_solution, ".csv"))
  
  if (!file.exists(cprob_path)) {
    stop(paste("cprob file not found:", cprob_path))
  }
  
  # Read profile assignments (contains ID, C, PHY, SLE, UND, SM, ALC)
  profiles_full <- read.csv(cprob_path, header = TRUE)
  
  # Merge with surveyyear from hbsc_def
  merged_data <- merge(profiles_full, hbsc_def[, c("ID", "surveyyear")], by = "ID")
  
  # Get unique survey years and sort them
  survey_years <- sort(unique(merged_data$surveyyear))
  
  # Initialize list to store data for all years
  all_year_data <- list()
  
  # Process Overall (all years combined)
  chunk <- merged_data
  
  # Calculate proportions
  class_counts <- table(chunk$C)
  proportions <- prop.table(class_counts) * 100
  
  # Calculate means for each class and variable
  means_list <- list()
  for (class in sort(unique(chunk$C))) {
    class_data <- chunk[chunk$C == class, ]
    means_list[[as.character(class)]] <- data.frame(
      Variable = c("PHY", "SLE", "UND", "SM", "ALC"),
      LatentClass = class,
      Estimate = c(mean(class_data$PHY, na.rm = TRUE),
                   mean(class_data$SLE, na.rm = TRUE),
                   mean(class_data$UND, na.rm = TRUE),
                   mean(class_data$SM, na.rm = TRUE),
                   mean(class_data$ALC, na.rm = TRUE)),
      Proportion = as.numeric(proportions[as.character(class)])
    )
  }
  overall_data <- do.call(rbind, means_list)
  overall_data$Year <- "Overall"
  all_year_data[["Overall"]] <- overall_data
  
  # Process each individual year
  for (year in survey_years) {
    chunk <- merged_data[merged_data$surveyyear == year, ]
    
    # Calculate proportions
    class_counts <- table(chunk$C)
    proportions <- prop.table(class_counts) * 100
    
    # Calculate means for each class and variable
    means_list <- list()
    for (class in sort(unique(chunk$C))) {
      class_data <- chunk[chunk$C == class, ]
      means_list[[as.character(class)]] <- data.frame(
        Variable = c("PHY", "SLE", "UND", "SM", "ALC"),
        LatentClass = class,
        Estimate = c(mean(class_data$PHY, na.rm = TRUE),
                     mean(class_data$SLE, na.rm = TRUE),
                     mean(class_data$UND, na.rm = TRUE),
                     mean(class_data$SM, na.rm = TRUE),
                     mean(class_data$ALC, na.rm = TRUE)),
        Proportion = as.numeric(proportions[as.character(class)])
      )
    }
    year_data <- do.call(rbind, means_list)
    year_data$Year <- as.character(year)
    all_year_data[[as.character(year)]] <- year_data
  }
  
  # Combine all data
  combined_data <- do.call(rbind, all_year_data)
  rownames(combined_data) <- NULL
  
  ### PLOTTING SECTION ###
  
  # Define the master lookup table for all possible profile labels
  profile_styles <- data.frame(
    label = c("Low risk", 
              "Slightly elevated substance use", 
              "Moderate substance use", 
              "Moderate risk (mixed)", 
              "High sleep problems", 
              "High alcohol use", 
              "Highest risk"),
    order = 1:7,
    color = c("#29af7f", "#bddf26", "#ffd92f", "#fc8d59", "#2e6f8e", "#8856a7", "#482173"),
    linetype = c("solid", "dashed", "longdash", "dotdash", "dotted", "dashed", "solid"),
    shape = c(16, 17, 18, 15, 4, 8, 16),
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
  
  # Get OVERALL profile mapping (to use for all facets)
  overall_profile_labels <- hbsc_labels %>% 
    filter(Country == input_country) %>% 
    select(starts_with("Overall_Profile_")) %>%
    unlist() %>%
    as.character()
  overall_profile_labels <- overall_profile_labels[!is.na(overall_profile_labels) & overall_profile_labels != "" & overall_profile_labels != "NA"]
  
  # Prepare the combined_data for plotting
  means_df_filtered <- combined_data
  
  # Rename variables to match plotting expectations
  means_df_filtered$Variable <- factor(means_df_filtered$Variable,
                                       levels = c("ALC", "SM", "PHY", "SLE", "UND"),
                                       labels = c("Alcohol Consumption", "Smoking", "Physical Inactivity", "Sleep Problems", "Unhealthy Diet"))
  
  # Set the desired order for x-axis variables
  desired_order <- c("Alcohol Consumption", "Smoking", "Physical Inactivity", "Sleep Problems", "Unhealthy Diet")
  means_df_filtered$Variable <- factor(means_df_filtered$Variable, levels = desired_order)
  
  # Convert LatentClass to factor
  means_df_filtered$LatentClass <- factor(means_df_filtered$LatentClass,
                                          levels = 1:class_num_numeric)
  
  # Use OVERALL profile mapping for ALL years
  means_df_filtered$ProfileLabel <- overall_profile_labels[means_df_filtered$LatentClass]
  
  # Match each profile label to its base category
  means_df_filtered$ProfileCategory <- sapply(means_df_filtered$ProfileLabel, function(label) {
    idx <- match_profile_category(label)
    if (!is.na(idx)) {
      return(profile_styles$label[idx])
    } else {
      return(label)
    }
  })
  
  # Create profile labels with proportions for display
  means_df_filtered$ProfileLabelWithProp <- paste0(
    means_df_filtered$ProfileLabel, 
    " (", 
    round(means_df_filtered$Proportion, 1), 
    "%)"
  )
  
  # Get unique categories that actually exist in the data
  existing_categories <- unique(means_df_filtered$ProfileCategory)
  
  # Filter profile_styles to only include existing categories and sort by order
  relevant_styles <- profile_styles[profile_styles$label %in% existing_categories, ]
  relevant_styles <- relevant_styles[order(relevant_styles$order), ]
  
  # Create ordered factor levels based on the category order
  means_df_filtered$ProfileOrder <- sapply(means_df_filtered$ProfileCategory, function(cat) {
    order_val <- relevant_styles$order[relevant_styles$label == cat]
    if (length(order_val) > 0) {
      return(order_val[1])
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE, simplify = TRUE)
  
  # Ensure ProfileOrder is numeric
  means_df_filtered$ProfileOrder <- as.numeric(means_df_filtered$ProfileOrder)
  
  # Order the ProfileLabelWithProp by the category order
  ordered_data <- means_df_filtered[order(means_df_filtered$ProfileOrder), ]
  ordered_levels <- unique(ordered_data$ProfileLabel)
  
  # Keep ProfileLabel as factor for styling
  means_df_filtered$ProfileLabel <- factor(
    means_df_filtered$ProfileLabel,
    levels = ordered_levels
  )
  
  # Set the order of facets with "Overall" first
  means_df_filtered$Year <- factor(means_df_filtered$Year, 
                                   levels = c("Overall", sort(unique(as.character(survey_years)))))
  
  # Set ProfileCategory as ordered factor with proper levels
  means_df_filtered$ProfileCategory <- factor(
    means_df_filtered$ProfileCategory,
    levels = profile_styles$label,
    ordered = TRUE
  )
  
  # Reshape the data to long format for plotting
  means_long <- melt(means_df_filtered, id.vars = c("Variable", "LatentClass", "ProfileLabel", "ProfileLabelWithProp", "ProfileCategory", "Year"),
                     measure.vars = "Estimate")
  
  # Create scale mappings based on ProfileCategory (base labels)
  all_unique_categories <- levels(means_long$ProfileCategory)[levels(means_long$ProfileCategory) %in% means_long$ProfileCategory]
  
  # Match to profile_styles lookup table
  style_indices <- match(all_unique_categories, profile_styles$label)
  
  # Create named vectors for the scales using ProfileCategory
  color_values <- profile_styles$color[style_indices]
  names(color_values) <- all_unique_categories
  
  linetype_values <- profile_styles$linetype[style_indices]
  names(linetype_values) <- all_unique_categories
  
  shape_values <- profile_styles$shape[style_indices]
  names(shape_values) <- all_unique_categories
  
  # Create the combined faceted plot
  plot <- ggplot(means_long, aes(x = Variable, y = value, 
                                 group = ProfileLabelWithProp, 
                                 color = ProfileCategory,
                                 linetype = ProfileCategory, 
                                 shape = ProfileCategory)) +
    geom_point(size = 8) +
    geom_line(linewidth = 1.5) +
    facet_wrap(~ Year, scales = "free_x") +
    labs(
      title = paste0("Latent Profile Analysis ", input_country, " - Sensitivity Analysis (With Overall Profiles)"),
      x = "Health Behaviors",
      y = "Means",
      color = "Risk Profile",
      linetype = "Risk Profile",
      shape = "Risk Profile"
    ) +
    scale_y_continuous(limits = c(-1, 7)) +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_linetype_manual(values = linetype_values, drop = FALSE) +
    scale_shape_manual(values = shape_values, drop = FALSE) +
    theme_minimal() +
    theme(
      title = element_text(size = 32),
      plot.title = element_text(margin = margin(b = 15)),
      text = element_text(size = 28),
      strip.text = element_text(size = 28, face = "bold"),
      axis.title.x = element_text(size = 24, margin = margin(t = 10)),
      axis.title.y = element_text(size = 24, margin = margin(r = 10)),
      axis.text.x = element_text(size = 28, angle = 45, hjust = 1, color = "black"), 
      axis.text.y = element_text(size = 28),
      legend.key.width = unit(1, "line"),
      legend.text = element_text(size = 28),
      legend.title = element_text(size = 28),
      legend.position = "top",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.direction = "horizontal"
    )
  
  # Create mini-legends within each facet showing proportions
  legend_data <- means_long %>%
    group_by(Year, ProfileCategory) %>%
    summarise(
      Proportion = first(gsub(".*\\((.*?)%\\).*", "\\1", ProfileLabelWithProp)),
      .groups = "drop"
    ) %>%
    arrange(Year, ProfileCategory) %>%
    group_by(Year) %>%
    mutate(
      x_point = seq(1.5, by = 1, length.out = n()),
      y_point = 6.3,
      x_text = x_point + 0.1,
      y_text = y_point + 0.05,
      label = paste0(Proportion, "%")
    ) %>%
    ungroup()
  
  # Add legend points and text to each facet
  plot <- plot +
    geom_point(data = legend_data,
               aes(x = x_point, y = y_point, color = ProfileCategory, shape = ProfileCategory),
               size = 10, inherit.aes = FALSE) +
    geom_text(data = legend_data,
              aes(x = x_text, y = y_text, label = label),
              size = 10, hjust = 0, vjust = 0.5, inherit.aes = FALSE, color = "black")
  
  # Return the plot
  return(plot)
}

# Read data
data_path <- "hbsc_variables.csv"
hbsc_def <- read.csv(data_path, header = TRUE)
hbsc_def <- tibble::rowid_to_column(hbsc_def, "ID")

hbsc_labels <- read_excel("hbsc_labels.xlsx")

plot <- create_sensitivity_lpa_facets(hbsc_def, hbsc_labels, "Switzerland")

print(plot)

### Trend plots split for gender / age cat

data_path = file.path(Sys.getenv("USERPROFILE"), "OneDrive - Universität Zürich UZH", "Datenanalyse", "hbsc_variables.csv")
hbsc <- read.csv(data_path, header=TRUE)
hbsc_CH <- hbsc %>% filter(countryname == 'Switzerland')

# List of variables to plot in the specified order
variables <- c("alcohol", "smoking", "physinact", "sleepprob", "undiet", 
               "lifesat", "feeling", "ache")

# Prepare data by calculating means for each variable by surveyyear and sex
data_by_sex <- hbsc %>%
  filter(!is.na(sex)) %>%
  group_by(surveyyear, sex) %>%
  summarise(across(all_of(variables), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(variables), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(sex_label = factor(sex, levels = c(1, 2), labels = c("Boys", "Girls")),
         variable = factor(variable, levels = variables))

# Find y-axis limits: separate for lifesat and common for others
y_max_common_sex <- max(data_by_sex$value[data_by_sex$variable != "lifesat"], na.rm = TRUE)

# Plot trends by sex with custom scales
plot_by_sex <- ggplot(data_by_sex, aes(x = surveyyear, y = value, color = sex_label, group = sex_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~variable, nrow = 2, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = unique(data_by_sex$surveyyear)) +
  scale_color_manual(values = c("Boys" = "#0072B2", "Girls" = "#E69F00")) +
  labs(x = "Survey Year", 
       y = "Mean Value", 
       color = "Sex",
       title = "Trends in Main Study Variables by Sex") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12, face = "bold"))

# Apply custom y-axis limits using ggh4x package (if available) or manual approach
library(ggh4x)
plot_by_sex <- plot_by_sex +
  facetted_pos_scales(
    y = list(
      variable == "lifesat" ~ scale_y_continuous(limits = c(0, 10)),
      variable != "lifesat" ~ scale_y_continuous(limits = c(0, y_max_common_sex))
    )
  )

print(plot_by_sex)

# Prepare data by calculating means for each variable by surveyyear and agecat
data_by_age <- hbsc %>%
  filter(!is.na(agecat)) %>%
  group_by(surveyyear, agecat) %>%
  summarise(across(all_of(variables), ~mean(., na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(variables), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(age_label = factor(agecat, levels = c(1, 2, 3), 
                            labels = c("11 years", "13 years", "15 years")),
         variable = factor(variable, levels = variables))

# Find y-axis limits: separate for lifesat and common for others
y_max_common_age <- max(data_by_age$value[data_by_age$variable != "lifesat"], na.rm = TRUE)

# Plot trends by age category with custom scales
plot_by_age <- ggplot(data_by_age, aes(x = surveyyear, y = value, color = age_label, group = age_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~variable, nrow = 2, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = unique(data_by_age$surveyyear)) +
  scale_color_manual(values = c("11 years" = "#0072B2",
                                "13 years" = "#E69F00",
                                "15 years" = "#CC79A7")) +
  labs(x = "Survey Year", 
       y = "Mean Value", 
       color = "Age Category",
       title = "Trends in Main Study Variables by Age Category") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        strip.text = element_text(size = 12, face = "bold"))

plot_by_age <- plot_by_age +
  facetted_pos_scales(
    y = list(
      variable == "lifesat" ~ scale_y_continuous(limits = c(0, 10)),
      variable != "lifesat" ~ scale_y_continuous(limits = c(0, y_max_common_age))
    )
  )

print(plot_by_age)