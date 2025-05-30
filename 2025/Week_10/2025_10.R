
## Challenge: #MakeoverMondnay 2025 week 10
## Data:      River Water Quality
## Author:    Steven Ponce
## Date:      2025-03-02

## Original Chart
# FRBC Fresh Water Watch by Fulham Reach Boat Club
# https://public.tableau.com/app/profile/fulham.reach.boat.club/viz/FRBCFreshWaterWatch_17116616886720/eColi

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    lubridate,      # Make Dealing with Dates a Little Easier
    ggpubr,         # 'ggplot2' Based Publication Ready Plots
    patchwork,      # The Composer of Plots
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  12,
    height =  12,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

#' The raw data for the week MakeoverMonday challenge can be downloaded 
#' here: https://data.world/makeovermonday/2025-week-10-river-water-quality/workspace/project-summary?agentid=makeovermonday&datasetid=2025-week-10-river-water-quality
#' 

frbc_fresh_water_raw <- read_csv('data/2025/FRBC FreshWater Watch - Form responses 1.csv') |> 
  clean_names()

frbc_river_cleanups_raw  <- read_csv('data/2025/FRBC River Clean ups - Form responses 1.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(frbc_fresh_water_raw)
skim(frbc_fresh_water_raw)

glimpse(frbc_river_cleanups_raw)
skim(frbc_river_cleanups_raw)


## 4. TIDYDATA ----

### |-  tidy data ----

# Clean the water quality dataset
clean_frbc_water <- function(frbc_fresh_water_raw) {
  frbc_clean <- frbc_fresh_water_raw |>
    # Convert date to proper format
    mutate(
      sample_date = dmy(sample_date),
      # Create date_time column
      date_time = if_else(
        !is.na(sample_date) & !is.na(sample_time),
        paste(sample_date, sample_time),
        NA_character_
      ),
      date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S")
    ) |>
    # Clean column names for easier reference
    rename(
      participants = total_number_of_participants,
      organisation = organisation_representing_participants_e_g_school_company,
      surface_condition = is_there_any_of_the_following_on_the_water_surface,
      algae_type = what_best_describes_the_dominant_form_of_algae_present,
      litter_presence = can_you_see_any_litter_including_litter_caught_in_vegetation,
      plastic_pollution = which_of_the_following_sources_of_plastic_pollution_can_you_see_select_all_that_apply,
      pollution_sources = are_there_any_water_pollution_sources_in_the_immediate_surroundings_select_all_that_apply,
      aquatic_life = what_aquatic_life_is_there_evidence_of_select_all_that_apply,
      recent_rain = has_there_been_any_rain_during_the_last_24_hrous,
      air_temp_c = air_temperature_degrees_celsius_to_nearest_whole_number,
      water_temp_c = water_temperature_degrees_celsius_to_nearest_whole_number,
      flow_ms = water_flow_measurement_m_s,
      nitrate_fww_mgl = fresh_water_watch_nitrate_test_mg_l,
      phosphate_fww_mgl = fresh_water_watch_phosphate_mg_l,
      turbidity_ntu = turbidity_secchi_tube_ntu,
      water_color = estimate_the_water_colour,
      nitrate_strip_mgl = test_strip_nitrate_mg_l_ppm,
      nitrite_strip_mgl = test_strip_nitrite_mg_l_ppm,
      chlorine_mgl = test_strip_chlorine_mg_l_ppm,
      hardness_mgl = test_strip_total_hardness_mg_l_ppm,
      carbonate_mgl = test_strip_carbonate_mg_l_ppm,
      ph = test_strip_p_h,
      phosphate_strip_mgl = test_strip_phosphate_mg_l_ppm_precision_laboratories,
      ecoli_cfu = alert_one_e_coli_reading_cfu_per_100ml
    ) |>
    # Extract year, month for easier analysis
    mutate(
      year = year(sample_date),
      month = month(sample_date),
      month_name = month(sample_date, label = TRUE),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Autumn",
        TRUE ~ NA_character_
      )
    ) |>
    # Handle missing values appropriately
    mutate(across(where(is.numeric), ~if_else(is.na(.), NA_real_, .))) |>
    # Create a binary rain indicator
    mutate(rain_binary = ifelse(recent_rain == "None", 0, 1))
  
  return(frbc_clean)
}

# Clean the river cleanups dataset
clean_frbc_cleanups <- function(frbc_river_cleanups_raw) {     
  frbc_cleanups_clean <- frbc_river_cleanups_raw |>
    # Convert date to proper format
    mutate(
      sample_date = dmy(sample_date),
      # Create date_time column
      date_time = if_else(
        !is.na(sample_date) & !is.na(sample_time),
        paste(sample_date, sample_time),
        NA_character_
      ),
      date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S")
    ) |>
    # Clean column names for easier reference
    rename(
      participants = total_number_of_participants,
      organisation = organisation_representing_participants_e_g_school_company,
      rubbish_sources = which_of_the_following_sources_of_rubbish_can_you_see_select_all_that_apply,
      rubbish_types = rubbish_types_collected_during_the_clean_up_select_all_that_apply,
      rubbish_weight_kg = estimated_weight_of_rubbish_collected_in_kilograms_kg
    ) |>
    # Extract year, month for easier analysis
    mutate(
      year = year(sample_date),
      month = month(sample_date),
      month_name = month(sample_date, label = TRUE),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Autumn",
        TRUE ~ NA_character_
      )
    ) |>
    # Calculate rubbish per participant
    mutate(rubbish_per_participant = rubbish_weight_kg / participants)
  
  return(frbc_cleanups_clean)
}

# Function to join the datasets for integrated analysis
join_frbc_datasets <- function(frbc_water_clean, frbc_cleanups_clean) {
  # Create a date-based lookup to find closest water quality measurement for each cleanup
  frbc_cleanups_clean <- frbc_cleanups_clean |>
    mutate(cleanup_id = row_number())
  
  frbc_water_clean <- frbc_water_clean |>
    mutate(water_id = row_number())
  
  # Extract relevant columns from each dataset with their proper names
  cleanup_dates <- frbc_cleanups_clean |>
    select(cleanup_id, cleanup_date = sample_date, year, month)
  
  water_data <- frbc_water_clean |>
    select(
      water_id, 
      water_date = sample_date, 
      water_temp_c, 
      flow_ms, 
      nitrate_fww_mgl, 
      phosphate_fww_mgl, 
      turbidity_ntu, 
      ph
    )
  
  # Create all combinations of cleanup events and water measurements
  all_combinations <- expand_grid(
    cleanup_id = cleanup_dates$cleanup_id,
    water_id = water_data$water_id
  )
  
  # Calculate time differences
  closest_measurements <- all_combinations |>
    left_join(cleanup_dates, by = "cleanup_id") |>
    left_join(water_data, by = "water_id") |>
    mutate(days_diff = abs(as.numeric(difftime(cleanup_date, water_date, units = "days")))) |>
    group_by(cleanup_id) |>
    slice_min(order_by = days_diff, n = 1) |>
    ungroup()
  
  # Join back to cleanup data
  joined_data <- frbc_cleanups_clean |>
    left_join(
      closest_measurements |>
        select(
          cleanup_id, 
          water_id, 
          days_diff, 
          water_date,
          water_temp_c, 
          flow_ms, 
          nitrate_fww_mgl, 
          phosphate_fww_mgl, 
          turbidity_ntu, 
          ph
        ),
      by = "cleanup_id"
    )
  
  return(joined_data)
}

# Cleaned and joined datasets
frbc_water_clean <- clean_frbc_water(frbc_fresh_water_raw) 
frbc_cleanups_clean <- clean_frbc_cleanups(frbc_river_cleanups_raw) 
joined_data <- join_frbc_datasets(frbc_water_clean, frbc_cleanups_clean)



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c())
  
### |-  titles and caption ----
title_text <- str_glue("The Thames in Motion: How Flow Shapes Water Quality and Pollution at FRBC")
subtitle_text <- str_glue("Analysis of water quality measurements and cleanup data at Fulham Reach Boat Club, 2023-2024")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 10,
    source_text = "River Water Quality"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Weekly-specific modifications
    legend.position = "top",
    legend.title = element_text(size = rel(0.79)),
    legend.text = element_text(size = rel(0.71)),
    
    axis.title = element_text(size = rel(1.14)),  
    axis.text = element_text(size = rel(0.86)),  
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", size = 0.5), 
    axis.ticks.length = unit(0.2, "cm"), 
    
    strip.text.y = element_text(size = rel(0.7), angle = 0), 
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),  
    panel.spacing.y = unit(0, "lines"),
    
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
# P1. River Flow Influences Pollution Accumulation ----
flow_rubbish_plot <- joined_data |>
  filter(!is.na(flow_ms), !is.na(rubbish_weight_kg)) |>  
  ggplot(aes(x = flow_ms, y = rubbish_weight_kg, color = season)) +
  # Geoms
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "black", 
              linetype = "dashed", fill = "gray80", alpha = 0.4) +
  # Scale
  scale_x_continuous(
    breaks = seq(-0.3, 0.5, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  scale_color_brewer(palette = "Set1") +
  # Labs
  labs(
    title = "River Flow Influences Pollution Accumulation",
    subtitle = "Negative correlation shows higher rubbish collection at lower flow rates",
    x = "Water Flow (m/s)",
    y = "Rubbish Collected (kg)",
    color = "Season"
  ) +
  # Theme
  theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(color = "gray95"),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Add annotation for statistical significance
# Get the linear model to extract statistics
flow_model <- lm(rubbish_weight_kg ~ flow_ms, data = joined_data)
model_p_value <- summary(flow_model)$coefficients[2,4]
model_r_squared <- summary(flow_model)$r.squared

# Format p-value and R-squared for display
p_value_text <- ifelse(model_p_value < 0.001, "p < 0.001", paste("p =", round(model_p_value, 3)))
r_squared_text <- paste("R² =", round(model_r_squared, 2))

# Add the annotation in upper right corner with improved formatting
flow_rubbish_plot <- flow_rubbish_plot +
  annotate(
    "label", 
    x = 0.3, 
    y = 110,
    label = paste(r_squared_text, p_value_text, sep = "\n"),
    hjust = 1, 
    size = 4,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.7
  )


# P2. Seasonal Nutrient Dynamics in Thames Water ----
nutrient_scatter_plot <- frbc_water_clean |>
  filter(!is.na(nitrate_fww_mgl), !is.na(phosphate_fww_mgl)) |>
  ggplot(aes(x = nitrate_fww_mgl, y = phosphate_fww_mgl, color = season)) +
  # Geoms
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  # Scale
  scale_x_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, by = 2.5)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  scale_color_brewer(palette = "Set1") +
  # Labs 
  labs(
    title = "Seasonal Nutrient Dynamics in Thames Water",
    subtitle = "Correlated nutrient levels suggest common sources vary by season",
    x = "Nitrate (mg/L)",
    y = "Phosphate (mg/L)",
    color = "Season"
  ) +
  # Facet 
  facet_wrap(~ season, scales = "fixed") +
  # Theme
  theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5),
    legend.position = "none", 
    panel.grid.minor = element_line(color = "gray95"),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12), 
    plot.margin = margin(20, 20, 20, 20)
  )

# Create a dataframe with R² values for each season
r2_values <- tibble(season = character(), r2 = numeric())

for(current_season in unique(frbc_water_clean$season)) {
  # Skip if there's no data for this season
  season_data <- frbc_water_clean |> 
    filter(season == current_season, !is.na(nitrate_fww_mgl), !is.na(phosphate_fww_mgl))
  
  if(nrow(season_data) < 3) {
    # Add placeholder if not enough data
    r2_values <- r2_values |> add_row(season = current_season, r2 = NA)
    next
  }
  
  # Calculate linear model for this season
  season_model <- lm(phosphate_fww_mgl ~ nitrate_fww_mgl, data = season_data)
  r2_values <- r2_values |> add_row(
    season = current_season, 
    r2 = summary(season_model)$r.squared
  )
}

# Add a data frame with annotation positions for each season
annotation_positions <- tibble(
  season = unique(frbc_water_clean$season),
  x = rep(2, length(unique(frbc_water_clean$season))),
  y = rep(0.7, length(unique(frbc_water_clean$season)))
)

# Join R² values with positions
annotation_data <- annotation_positions |>
  left_join(r2_values, by = "season") |>
  filter(!is.na(r2))

# Add annotations programmatically with one per facet
nutrient_scatter_plot <- nutrient_scatter_plot +
  geom_label(
    data = annotation_data,
    aes(x = x, y = y, label = sprintf("R² = %.2f", r2)),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    fill = "white",
    alpha = 0.7
  )

# Add reference line for theoretical threshold
nutrient_scatter_plot <- nutrient_scatter_plot +
  geom_hline(
    yintercept = 0.1, 
    linetype = "dashed", 
    color = "darkred", 
    alpha = 0.6
  ) +
  annotate(
    "text",
    x = 9.5,
    y = 0.16,
    label = "Theoretical threshold (0.1 mg/L)",
    hjust = 1,
    size = 3,
    fontface = "italic",
    color = "darkred"
  )


# P3. Turbidity vs Water Flow with Algae Type 
turbidity_flow_plot <- frbc_water_clean |>
  filter(!is.na(flow_ms), !is.na(turbidity_ntu)) |>
  # Clean up algae type values for better display
  mutate(
    algae_type = case_when(
      is.na(algae_type) ~ "No data",
      algae_type == "" ~ "No data",
      TRUE ~ algae_type
    )
  ) |>
  # Handle extreme outliers if present
  filter(turbidity_ntu <= 100) |>  
  ggplot(aes(x = flow_ms, y = turbidity_ntu, color = algae_type)) +
  # Geoms
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "black", 
    linetype = "dashed", 
    fill = "gray80", 
    alpha = 0.4,
    aes(group = 1)
  ) +
  # Scale
  scale_x_continuous(
    limits = c(-0.3, 0.6),
    breaks = seq(-0.3, 0.6, by = 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20)
  ) +
  scale_color_brewer(palette = "Dark2") +
  # Labs
  labs(
    title = "Water Flow Affects Clarity and Algal Growth",
    subtitle = "Different algae types dominate at specific flow conditions",
    x = "Water Flow (m/s)",
    y = "Turbidity (NTU)",
    color = "Algae Type"
  ) +
  # Theme
  theme_minimal() +
  theme(
    text = element_text(family = "sans", size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_line(color = "gray95"),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    legend.key.size = unit(0.8, "lines"),
    legend.text = element_text(size = 10)
  ) +
  # Make the legend more readable with wrapped labels
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Check if there are any blue-green scum algae points
blue_green_points <- frbc_water_clean |>
  filter(!is.na(flow_ms), !is.na(turbidity_ntu)) |>
  filter(algae_type == "Blue-green scum")

# Only add the annotation if blue-green points exist
if(nrow(blue_green_points) > 0) {
  # Find average position of blue-green scum points
  avg_x <- mean(blue_green_points$flow_ms)
  avg_y <- mean(blue_green_points$turbidity_ntu)
  
  # Add an annotation at that position
  turbidity_flow_plot <- turbidity_flow_plot +
    geom_label(
      data = data.frame(x = avg_x, y = avg_y + 5),
      aes(x = x, y = y, label = "Blue-green algae common\nat these flow rates"),
      inherit.aes = FALSE,
      size = 3,
      fontface = "bold",
      color = "black",
      fill = "white",
      alpha = 0.7
    )
}

# Add an annotation about how flow affects turbidity
turbidity_flow_plot <- turbidity_flow_plot +
  annotate(
    "label",
    x = 0.3,
    y = 80,
    label = "Moderate flow rates (around 0.3 m/s)\nshow peak turbidity with lower values\n at both slow and fast flows",
    size = 3,
    fontface = "bold",
    color = "black",
    fill = "white",
    alpha = 0.7
  )

# Combined Plots ----
plot1 <- flow_rubbish_plot 
plot2 <- nutrient_scatter_plot 
plot3 <- turbidity_flow_plot 

combined_plot <- (plot1 + plot3) / plot2 +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size   = rel(1.8),
        family = fonts$title,
        face   = "bold",
        color  = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size   = rel(0.95),
        family = fonts$subtitle,
        color  = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(
        size   = rel(0.75),
        family = fonts$caption,
        color  = colors$caption,
        hjust  = 0.5,
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
    )
  )

combined_plot 


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-02
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# P abind          1.4-8    2024-09-12 [?] RSPM (R 4.4.0)
# backports      1.5.0    2024-05-23 [1] RSPM (R 4.4.0)
# V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc      0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit            4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64          4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# broom          1.0.7    2024-09-26 [1] RSPM (R 4.4.0)
# camcorder    * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# P car            3.1-3    2024-09-27 [?] RSPM (R 4.4.0)
# P carData        3.0-5    2022-01-06 [?] RSPM (R 4.4.0)
# cli            3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace     2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark     1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler       4.4.0    2024-04-24 [?] local
# crayon         1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
# curl           6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
# P datasets     * 4.4.0    2024-04-24 [?] local
# digest         0.6.37   2024-08-19 [1] RSPM (R 4.4.0)
# dplyr        * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# evaluate       1.0.3    2025-01-10 [1] RSPM (R 4.4.0)
# farver         2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
# fastmap        1.2.0    2024-05-15 [1] RSPM (R 4.4.0)
# forcats      * 1.0.0    2023-01-29 [1] RSPM (R 4.4.0)
# P Formula        1.2-5    2023-02-24 [?] RSPM (R 4.4.0)
# generics       0.1.3    2022-07-05 [1] RSPM (R 4.4.0)
# ggplot2      * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
# P ggpubr       * 0.6.0    2023-02-10 [?] RSPM (R 4.4.0)
# P ggsignif       0.6.4    2022-10-13 [?] RSPM (R 4.4.0)
# ggtext       * 0.1.2    2022-09-16 [1] RSPM (R 4.4.0)
# gifski         1.32.0-1 2024-10-13 [1] RSPM (R 4.4.1)
# glue         * 1.8.0    2024-09-30 [1] RSPM (R 4.4.0)
# P graphics     * 4.4.0    2024-04-24 [?] local
# P grDevices    * 4.4.0    2024-04-24 [?] local
# P grid           4.4.0    2024-04-24 [?] local
# gridtext       0.1.5    2022-09-16 [1] RSPM (R 4.4.0)
# gtable         0.3.6    2024-10-25 [1] RSPM (R 4.4.0)
# here         * 1.0.1    2020-12-13 [1] RSPM (R 4.4.0)
# hms            1.1.3    2023-03-21 [1] RSPM (R 4.4.0)
# htmltools      0.5.8.1  2024-04-04 [1] RSPM (R 4.4.0)
# janitor      * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite       1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr          1.49     2024-11-08 [1] RSPM (R 4.4.0)
# P lattice        0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# lifecycle      1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate    * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick         2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr       2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown       1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P Matrix         1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods      * 4.4.0    2024-04-24 [?] local
# P mgcv           1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell        0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P nlme           3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel       4.4.0    2024-04-24 [?] local
# patchwork    * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar         1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig      2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr        * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6             2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg           1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# RColorBrewer   1.1-3    2022-04-03 [1] RSPM (R 4.4.0)
# Rcpp           1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr        * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# renv           1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# repr           1.1.7    2024-03-22 [1] RSPM (R 4.4.0)
# rlang          1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot      2.0.4    2023-11-05 [1] RSPM (R 4.4.0)
# P rstatix        0.7.2    2023-02-01 [?] RSPM (R 4.4.0)
# rstudioapi     0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg           2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# scales       * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo    1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# showtext     * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb   * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
# skimr        * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
# snakecase      0.11.1   2023-08-27 [1] RSPM (R 4.4.0)
# P splines        4.4.0    2024-04-24 [?] local
# P stats        * 4.4.0    2024-04-24 [?] local
# stringi        1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
# stringr      * 1.5.1    2023-11-14 [1] RSPM (R 4.4.0)
# svglite        2.1.3    2023-12-08 [1] RSPM (R 4.4.0)
# sysfonts     * 0.8.9    2024-03-02 [1] RSPM (R 4.4.0)
# systemfonts    1.2.1    2025-01-20 [1] RSPM (R 4.4.0)
# textshaping    1.0.0    2025-01-20 [1] RSPM (R 4.4.0)
# tibble       * 3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# tidyr        * 1.3.1    2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect     1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse    * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# timechange     0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tools          4.4.0    2024-04-24 [?] local
# tzdb           0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# utf8           1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils        * 4.4.0    2024-04-24 [?] local
# vctrs          0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# vroom          1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# withr          3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun           0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2           1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────
# > 