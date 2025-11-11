## Challenge: #MakeoverMonday 2025 week 44
## Data:      Living WWII Veterans by State 2025
## Author:    Steven Ponce
## Date:      2025-11-11

## Article
# https://www.nationalww2museum.org/war/wwii-veteran-statistics

## Data
# https://data.world/makeovermonday/week-44-2025-wwii-veteran-statistics
# https://www.nationalww2museum.org/war/wwii-veteran-statistics

## Citation
# National WWII Museum. (2025). WWII Veteran Statistics.
# https://www.nationalww2museum.org/war/wwii-veteran-statistics
#
# US Census Bureau. (2023). Annual Estimates of the Resident Population.
# Retrieved via tidycensus package.

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Easily Install and Load the 'Tidyverse'
  janitor, # Simple Tools for Examining and Cleaning Dirty Data
  skimr, # Compact and Flexible Summaries of Data
  scales, # Scale Functions for Visualization
  ggtext, # Improved Text Rendering Support for 'ggplot2'
  showtext, # Using Fonts More Easily in R Graphs
  glue, # Interpreted String Literals
  patchwork, # The Composer of Plots
  ggrepel, # Automatically Position Non-Overlapping Text Labels
  tidycensus # Load US Census Boundary and Attribute Data
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
ww2_veterans_raw <- read_csv("data/2025/Living_WWII_Veterans_by_State_2025.csv") |>
  clean_names()

### |-  Get state population data from US Census Bureau ----
# Source: US Census Bureau, Population Division
# Annual Estimates of the Resident Population: July 1, 2023
# Retrieved via tidycensus package
state_pop_census <- get_estimates(
  geography = "state",
  product = "population",
  vintage = 2023,
  year = 2023
) |>
  filter(variable == "POPESTIMATE") |>
  select(NAME, value) |>
  rename(state = NAME, population_2023 = value)


## 3. EXAMINING THE DATA ----
glimpse(ww2_veterans_raw)
skim_without_charts(ww2_veterans_raw) |> summary()


## 4. TIDY DATA ----
ww2_veterans_clean <- ww2_veterans_raw |>
  filter(state != "Island Areas & Foreign") |>
  left_join(state_pop_census, by = "state") |>
  mutate(
    veterans_per_100k = if_else(!is.na(population_2023),
      1e5 * living_wwii_veterans_2025 / population_2023,
      NA_real_
    ),
    region = case_when(
      state %in% c(
        "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island",
        "Vermont", "New Jersey", "New York", "Pennsylvania"
      ) ~ "Northeast",
      state %in% c(
        "Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri",
        "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"
      ) ~ "Midwest",
      state %in% c(
        "Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida", "Georgia",
        "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma",
        "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"
      ) ~ "South",
      state %in% c(
        "Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho", "Montana",
        "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming"
      ) ~ "West",
      state == "Puerto Rico" ~ "Territory",
      TRUE ~ "Other"
    )
  )

summary_stats <- ww2_veterans_clean |>
  summarise(
    total_veterans   = sum(living_wwii_veterans_2025, na.rm = TRUE),
    mean_per_state   = mean(living_wwii_veterans_2025, na.rm = TRUE),
    median_per_state = median(living_wwii_veterans_2025, na.rm = TRUE),
    mean_per_100k    = mean(veterans_per_100k, na.rm = TRUE)
  )

national_mean_per_100k <- as.numeric(summary_stats$mean_per_100k)
national_median_count <- as.numeric(summary_stats$median_per_state)

### |-  panel 1 data ----
# A) Veterans per 100k vs US mean
panel_1_data <- ww2_veterans_clean |>
  filter(!is.na(veterans_per_100k)) |>
  mutate(
    diff_from_mean = veterans_per_100k - national_mean_per_100k,
    state = fct_reorder(state, diff_from_mean),
    above_avg = diff_from_mean > 0
  ) |>
  slice_max(order_by = abs(diff_from_mean), n = 20)

### |-  panel 2 data ----
# B) Top states by per-capita rate
panel_2_data <- ww2_veterans_clean |>
  filter(!is.na(veterans_per_100k)) |>
  arrange(desc(veterans_per_100k)) |>
  slice_head(n = 20) |>
  mutate(state = fct_reorder(state, veterans_per_100k))

### |-  panel 3 data ----
# C) Histogram + density with labels for states ≥ 3,000 veterans
big_states <- ww2_veterans_clean |>
  filter(living_wwii_veterans_2025 >= 3000) |>
  mutate(y_pos = 0.00003)

top5_share <- ww2_veterans_clean |>
  arrange(desc(living_wwii_veterans_2025)) |>
  slice_head(n = 5) |>
  summarise(share = sum(living_wwii_veterans_2025, na.rm = TRUE) /
    sum(ww2_veterans_clean$living_wwii_veterans_2025, na.rm = TRUE)) |>
  pull(share)

### |-  panel 4 data ----
# D) Box + jitter
panel_4_data <- ww2_veterans_clean |>
  filter(!is.na(veterans_per_100k)) |>
  group_by(region) |>
  mutate(
    regional_median = median(veterans_per_100k),
    n_states = n()
  ) |>
  ungroup() |>
  mutate(region = fct_reorder(region, regional_median, .desc = TRUE))

region_levels <- levels(panel_4_data$region)
n_map <- panel_4_data |>
  distinct(region, n_states) |>
  deframe()
label_map <- setNames(
  paste0(region_levels, "\n(n=", n_map[region_levels], ")"),
  region_levels
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    below_avg = "#D97548",
    above_avg = "#4A7C8C",
    primary_accent = "#2B4C5E",
    secondary_accent = "#D97548",
    box_northeast = "#4A7C8C",
    box_west = "#8B9D57",
    box_south = "#D97548",
    box_midwest = "#6B8CAE",
    box_territory = "#999999",
    gray_dark = "#3D3D3D",
    gray_medium = "#9A9A9A",
    gray_light = "#E6E6E6"
  )
)

### |-  titles and caption ----
title_text <- "Living WWII Veterans by State (2025)"

subtitle_text <- str_glue(
  "**{comma(summary_stats$total_veterans)}** veterans remain; ",
  "rates per 100k use U.S. Census 2023 population estimates"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 44,
  source_text = "(1) National WWII Museum (2025), (2) U.S. Census Bureau (2023 Population Estimates via tidycensus, retrieved 20251111). "
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
    # # Text styling
    plot.title = element_text(
      size = rel(1.6), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.95), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "plot",
    legend.justification = "top",
    legend.margin = margin(l = 12, b = 5),
    legend.key.size = unit(0.8, "cm"),
    legend.box.margin = margin(b = 10),
    legend.title = element_text(face = "bold"),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = colors$text
    ),
    axis.text.y = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = colors$text
    ),

    # Grid lines
    panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  panel 1 plot ----
# A) Veterans per 100k vs US mean
panel_1 <- ggplot(panel_1_data, aes(diff_from_mean, state, fill = above_avg)) +
  # Geoms
  geom_col(width = 0.68) +
  geom_vline(xintercept = 0, linewidth = 0.8, color = colors$palette$gray_dark) +
  geom_text(aes(label = number(diff_from_mean, accuracy = 0.1)),
    hjust = ifelse(panel_1_data$diff_from_mean > 0, -0.15, 1.15),
    size = 2.8, color = colors$palette$gray_dark
  ) +
  # Scales
  scale_fill_manual(values = c(`TRUE` = colors$palette$above_avg, `FALSE` = colors$palette$below_avg)) +
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.1, 0.12))
  ) +
  # Labs
  labs(
    title = "A. Veterans per 100k vs US Mean",
    subtitle = "Higher values indicate greater veteran concentration (†)",
    x = "Difference from US mean", y = NULL
  ) +
  # Theme
  theme(
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(margin = margin(r = 4))
  )

### |-  panel 2 plot ----
# B) Top states by per-capita rate
panel_2 <-
  ggplot(panel_2_data, aes(veterans_per_100k, state)) +
  # Geoms
  geom_segment(aes(x = 0, xend = veterans_per_100k, yend = state),
    color = colors$palette$gray_light, linewidth = 1.1
  ) +
  geom_point(size = 3.2, color = colors$palette$primary_accent) +
  geom_vline(
    xintercept = national_mean_per_100k, linetype = "dashed",
    color = colors$palette$gray_dark, linewidth = 0.6
  ) +
  geom_text(aes(label = number(veterans_per_100k, accuracy = 0.1)),
    nudge_x = 1.0, hjust = 0, size = 2.8, color = colors$palette$gray_dark
  ) +
  # Scales
  scale_x_continuous(
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  # Labs
  labs(
    title = "B. Top States by Veterans per 100k",
    subtitle = glue(
      "Each exceeds the national average of {number(national_mean_per_100k, accuracy = 0.1)} per 100k (†)"
    ),
    x = "Veterans per 100k residents", y = NULL
  ) +
  # Theme
  theme(
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(margin = margin(r = 4))
  )

### |-  panel 3 plot ----
# C) Histogram + density with labels for states ≥ 3,000 veterans
panel_3 <-
  ggplot(ww2_veterans_clean, aes(living_wwii_veterans_2025)) +
  # Geoms
  geom_histogram(aes(y = after_stat(density)),
    binwidth = 250, boundary = 0,
    fill = colors$palette$primary_accent, alpha = 0.72,
    color = "white", linewidth = 0.3
  ) +
  geom_density(color = colors$palette$secondary_accent, linewidth = 1.0) +
  geom_vline(
    xintercept = national_median_count, linetype = "dotted",
    color = colors$palette$gray_dark, linewidth = 0.6
  ) +
  geom_label_repel(
    data = big_states,
    aes(x = living_wwii_veterans_2025, y = y_pos, label = state),
    seed = 2025, size = 3, label.size = 0.25,
    fill = alpha("white", 0.9), color = colors$palette$gray_dark,
    direction = "y", nudge_y = 0.00002, min.segment.length = 0,
    box.padding = 0.25
  ) +
  # Annotate
  annotate("label",
    x = national_median_count, y = 0.0009, label = "State median",
    size = 2.7, label.size = 0, fill = alpha("white", 0.92),
    color = colors$gray_dark
  ) +
  annotate("label",
    x = Inf, y = Inf, hjust = 1.02, vjust = 1.2,
    label = paste0("Top 5 hold ", percent(top5_share, accuracy = 1)),
    size = 3, label.size = 0, fill = alpha("white", 0.9),
    color = colors$gray_dark
  ) +
  # Scales
  scale_x_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.0001)) +
  # Labs
  labs(
    title = "C. Distribution of Veterans Across States",
    subtitle = glue(
      "Most states have fewer than 1,000; top 5 hold {percent(top5_share, accuracy = 1)} of all veterans"
    ),
    x = "Living WWII veterans (count)", y = "Density"
  ) +
  # Theme
  theme(plot.margin = margin(5, 5, 5, 5))

### |-  panel 4 plot ----
# D) Box + jitter
panel_4 <-
  ggplot(panel_4_data, aes(region, veterans_per_100k, color = region)) +
  # Geoms
  geom_boxplot(width = 0.58, alpha = 0.28, linewidth = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2.2, alpha = 0.75) +
  geom_hline(
    yintercept = national_mean_per_100k, linetype = "dashed",
    color = colors$palette$gray_dark, linewidth = 0.6
  ) +
  geom_label_repel(
    data = filter(panel_4_data, state == "New Hampshire"),
    aes(label = state),
    seed = 2025, size = 3, label.size = 0.25,
    fill = alpha("white", 0.9), color = colors$palette$gray_dark,
    nudge_y = 2, nudge_x = 0.2
  ) +
  # Scales
  scale_color_manual(values = c(
    "Northeast" = colors$palette$box_northeast,
    "West" = colors$palette$box_west,
    "South" = colors$palette$box_south,
    "Midwest" = colors$palette$box_midwest,
    "Territory" = colors$palette$box_territory
  ), guide = "none") +
  scale_x_discrete(limits = region_levels, labels = label_map[region_levels]) +
  coord_cartesian(ylim = c(0, NA)) +
  # Labs
  labs(
    title = "D. Regional Distribution of Veterans per 100k",
    subtitle = "Northeast has highest rates; South has many states but lower median (†)",
    x = "Region", y = "Veterans per 100k residents"
  ) +
  # Theme
  theme(
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_text(lineheight = 1.1)
    )

### |-  combined plot ----
combined_plots <- (panel_1 | panel_2) / (panel_3 | panel_4) +
  plot_layout(heights = c(1.2, 0.8), widths = c(1, 1))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    # tag_levels = "A",
    theme = theme(
      plot.title = element_text(
        size = rel(2.4),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1.2),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 1.2,
        margin = margin(t = 5, b = 15)
      ),
      plot.caption = element_markdown(
        size = rel(0.65),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0,
        margin = margin(t = 10)
      )
    )
  )


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #MakeoverMonday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #MakeoverMonday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across X+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/MakeoverMonday/tree/master/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/MakeoverMonday/tree/master
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-11
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
#
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder   * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# P chromote      0.4.0    2025-01-25 [?] RSPM (R 4.4.0)
# P class         7.3-22   2023-05-03 [?] CRAN (R 4.4.0)
# P classInt      0.4-11   2025-01-08 [?] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark    1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
# curl          6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# DBI           1.2.3    2024-06-02 [1] RSPM (R 4.4.0)
# digest        0.6.37   2024-08-19 [1] RSPM (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# P e1071         1.7-16   2024-09-16 [?] RSPM (R 4.4.0)
# evaluate      1.0.3    2025-01-10 [1] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
# fastmap       1.2.0    2024-05-15 [1] RSPM (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] RSPM (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] RSPM (R 4.4.0)
# P ggplot2     * 3.5.2    2025-04-09 [?] RSPM (R 4.4.0)
# P ggrepel     * 0.9.6    2024-09-07 [?] RSPM (R 4.4.0)
# ggtext      * 0.1.2    2022-09-16 [1] RSPM (R 4.4.0)
# gifski        1.32.0-1 2024-10-13 [1] RSPM (R 4.4.1)
# glue        * 1.8.0    2024-09-30 [1] RSPM (R 4.4.0)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# gridtext      0.1.5    2022-09-16 [1] RSPM (R 4.4.0)
# gtable        0.3.6    2024-10-25 [1] RSPM (R 4.4.0)
# here        * 1.0.1    2020-12-13 [1] RSPM (R 4.4.0)
# hms           1.1.3    2023-03-21 [1] RSPM (R 4.4.0)
# htmltools     0.5.8.1  2024-04-04 [1] RSPM (R 4.4.0)
# httr          1.4.7    2023-08-15 [1] RSPM (R 4.4.0)
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# P KernSmooth    2.23-22  2023-07-10 [?] CRAN (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] RSPM (R 4.4.0)
# P later         1.4.1    2024-11-27 [?] RSPM (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# processx      3.8.5    2025-01-08 [1] RSPM (R 4.4.0)
# P promises      1.3.2    2024-11-28 [?] RSPM (R 4.4.0)
# P proxy         0.4-27   2022-06-09 [?] CRAN (R 4.4.0)
# ps            1.8.1    2024-10-28 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# P R.cache       0.16.0   2022-07-21 [?] RSPM (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] RSPM (R 4.4.0)
# P R.oo          1.27.0   2024-11-01 [?] RSPM (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# rappdirs      0.3.3    2021-01-31 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# renv          1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# repr          1.1.7    2024-03-22 [1] RSPM (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot     2.0.4    2023-11-05 [1] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg          2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# rvest         1.0.4    2024-02-12 [1] RSPM (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo * 1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# P sf            1.0-21   2025-05-15 [?] RSPM (R 4.4.0)
# showtext    * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb  * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
# skimr       * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
# snakecase     0.11.1   2023-08-27 [1] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
# stringr     * 1.5.1    2023-11-14 [1] RSPM (R 4.4.0)
# P styler        1.10.3   2024-04-07 [?] RSPM (R 4.4.0)
# svglite       2.1.3    2023-12-08 [1] RSPM (R 4.4.0)
# sysfonts    * 0.8.9    2024-03-02 [1] RSPM (R 4.4.0)
# systemfonts   1.2.1    2025-01-20 [1] RSPM (R 4.4.0)
# textshaping   1.0.0    2025-01-20 [1] RSPM (R 4.4.0)
# tibble      * 3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# P tidycensus  * 1.7.3    2025-07-24 [?] RSPM (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# P tigris        2.2.1    2025-04-16 [?] RSPM (R 4.4.0)
# timechange    0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# tzdb          0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# P units         1.0-0    2025-10-09 [?] RSPM (R 4.4.0)
# utf8          1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# uuid          1.2-1    2024-07-29 [1] RSPM (R 4.4.0)
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# P websocket     1.4.2    2024-07-22 [?] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
#
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
#
# ─────────────────────────────────────────────────────────────────────────
# >
