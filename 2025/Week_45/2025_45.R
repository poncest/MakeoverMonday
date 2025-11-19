## Challenge: #MakeoverMonday 2025 week 45
## Data:      Terrorism and Political Violence in the United States: What the Data Tells Us
## Author:    Steven Ponce
## Date:      2025-11-19

## Article
# https://www.csis.org/analysis/left-wing-terrorism-and-political-violence-united-states-what-data-tells-us

## Data
# https://data.world/makeovermonday/2025w45-terrorism-and-political-violence-in-the-usa
# https://www.csis.org/analysis/left-wing-terrorism-and-political-violence-united-states-what-data-tells-us

## Data Sources
# Terrorism data: terrorism_by_ideology_1994-2025.csv
# Population data: US Census Bureau via tidycensus package

## Citation
# Center for Strategic and International Studies (CSIS). (2025). 
# Terrorism and Political Violence in the United States: What the Data Tells Us. 
# CSIS Warfare, Irregular Threats, and Terrorism Program.
#
# US Census Bureau. Population Estimates Program (PEP).
# Retrieved via tidycensus R package.
## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  glue,          # Interpreted String Literals
  patchwork,     # The Composer of Plots
  ggrepel,       # Automatically Position Non-Overlapping Text Labels
  zoo            # For rollmean function
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 16,
  height = 11,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
terrorism_enriched <- read_csv("2025/Week_45/terrorism_enriched.csv") |>
  mutate(ideology = factor(
    ideology,
    levels = c("Right", "Left", "Jihadist", "Ethnonationalist", "Other")
  ))

terrorism_yearly <- read_csv("2025/Week_45/terrorism_yearly.csv")

# Filter to 1994-2024 (exclude partial 2025)
data_1994_2024 <- terrorism_enriched |> filter(year <= 2024)

## 3. EXAMINING THE DATA ----
glimpse(terrorism_enriched)
glimpse(terrorism_yearly)


## 4. TIDY DATA ----

### |-  Data Notes ----
# The datasets loaded below have been preprocessed via data_prep.R
#
# Key preprocessing steps:
# 1. Original data: terrorism_by_ideology_1994-2025.csv (raw attack counts)
# 2. Population data: Fetched from US Census Bureau via tidycensus
#    - 1994-2009: Intercensal estimates (official reconciled data)
#    - 2010-2019: Vintage 2019 estimates via API
#    - 2020-2023: Vintage 2023 estimates
#    - 2024-2025: Linear projection (excluded from analysis)
# 3. Calculated metrics:
#    - attacks_per_million: Population-adjusted rates
#    - pct_of_year: Percentage composition
#
# To regenerate these files, run: source("2025/Week_45/data_prep.R")


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    "Right"            = "#A08843",
    "Left"             = "#4F6D78",
    "Jihadist"         = "#73896C",
    "Ethnonationalist" = "#8E7E9E",
    "Other"            = "#9B9B9B",
    "total"            = "#2C3E4C",
    "per_capita"       = "#6A5882",
    "breakdown"        = "#A08843"
  )
)

### |-  Main titles ----
title_text <- "Understanding the Rise in Domestic Terrorism: Context Matters"

subtitle_text <- str_glue(
  "While raw attack counts increased 53% (1994-2001 vs 2020-2024), <b>population growth explains half this increase</b>.<br>",
  "Per capita rates show a 26% rise—significant but less dramatic."
)

### |-  Data source caption ----
caption_text <- create_social_caption_02_mm(
  mm_year = 2025,
  mm_week = 45,
  note_text = str_glue(
    "**Note:** 2025 data excluded because it represents a partial year.<br>",
    "Per capita rates are calculated as attacks per million people.<br>",
    "The use of the 2010 Census as a population baseline may produce a slight discontinuity.<br>"
  ),
  source_text = str_glue(
    "Terrorism incidents: CSIS Warfare, Irregular Threats & Terrorism Program (1994–2024).<br>",
    "U.S. population estimates:<br>",
    "&nbsp;&nbsp;• Intercensal estimates (1994–2009) ",
    "&nbsp;&nbsp;• Vintage 2019 estimates (2010–2019) ",
    "&nbsp;&nbsp;• Vintage 2023 estimates (2020–2024)<br>"
  )
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
      size = rel(1.5), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "plot",
    legend.justification = "top",
    legend.margin = margin(l = 12, b = 5),
    legend.key.size = unit(0.8, "cm"),
    legend.box.margin = margin(b = 10),
    # legend.title = element_text(face = "bold"),

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

### |-  Panel A: Total Attacks (Raw Counts) ----
pa <-
  terrorism_yearly |>
  filter(year <= 2024) |>
  ggplot(aes(x = year, y = total_attacks)) +
  geom_hline(
    yintercept = 23.8,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 1.2, color = colors$palette$total, alpha = 0.9) +
  geom_point(size = 2.2, color = colors$palette$total) +
  annotate(
    "text",
    x = 2001,
    y = 27,
    label = "Mean: 23.8",
    size = 2.8,
    color = "grey50",
    hjust = 0,
    family = fonts$subtitle
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50)) +
  labs(
    title = "A. Total Attacks",
    subtitle = "Raw count increased 53% (1994-2001 to 2020-2024)",
    y = "Annual attacks",
    x = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

### |-  Panel B: Per Capita Rate ----
pb <-
  terrorism_yearly |>
  filter(year <= 2024) |>
  ggplot(aes(x = year, y = attacks_per_million)) +
  geom_hline(
    yintercept = 0.082,
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.4
  ) +
  geom_line(linewidth = 1.2, color = colors$palette$per_capita, alpha = 0.9) +
  geom_point(size = 2.2, color = colors$palette$per_capita) +
  annotate(
    "segment",
    x = 2010, xend = 2010,
    y = 0, yend = 0.155,
    linetype = "dotted",
    color = "grey70",
    linewidth = 0.35
  ) +
  annotate(
    "text",
    x = 2010,
    y = 0.158,
    label = "Census 2010\nbase year",
    size = 2.4,
    color = "grey60",
    hjust = 0.5,
    lineheight = 0.9,
    family = fonts$subtitle
  ) +
  annotate(
    "text",
    x = 2001,
    y = 0.093,
    label = "Mean: 0.082",
    size = 2.8,
    color = "grey50",
    hjust = 0,
    family = fonts$subtitle
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(
    breaks = seq(0, 0.16, 0.04),
    limits = c(0, 0.16)
  ) +
  labs(
    title = "B. Per Capita Rate",
    subtitle = "Population-adjusted: 26% increase (1994-2001 to 2020-2024)",
    y = "Attacks per million people",
    x = NULL
  ) +
  theme(
    panel.grid.major.x = element_blank()
  )

### |-  Panel C: Attacks by Ideology (Top 3, 3-year avg)  ----
data_smoothed <- data_1994_2024 |>
  filter(ideology %in% c("Right", "Left", "Jihadist")) |>
  arrange(ideology, year) |>
  group_by(ideology) |>
  mutate(attacks_smooth = rollmean(attacks, k = 3, fill = NA, align = "center")) |>
  ungroup()

label_data <- data_smoothed |>
  filter(!is.na(attacks_smooth)) |>
  group_by(ideology) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(
    label = case_when(
      ideology == "Right" ~ "Right-wing",
      ideology == "Left" ~ "Left-wing",
      ideology == "Jihadist" ~ "Jihadist"
    )
  )

pc <-
  data_smoothed |>
  ggplot(aes(x = year, y = attacks_smooth, color = ideology)) +
  geom_line(linewidth = 1.2, alpha = 0.95) +
  geom_vline(
    xintercept = 2014,
    linetype = "dotted",
    alpha = 0.4,
    color = "grey40"
  ) +
  geom_text_repel(
    data = label_data,
    aes(label = label),
    hjust = 0,
    nudge_x = 1.2,
    direction = "y",
    segment.size = 0.3,
    segment.color = "grey60",
    size = 3,
    fontface = "bold",
    family = fonts$subtitle,
    show.legend = FALSE,
    min.segment.length = 0
  ) +
  annotate(
    "text",
    x = 2014,
    y = 32,
    label = "2014",
    size = 2.5,
    color = "grey40",
    family = fonts$subtitle
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(breaks = seq(0, 35, 10), limits = c(0, 35)) +
  scale_color_manual(
    values = c(
      "Right"    = colors$palette$Right,
      "Left"     = colors$palette$Left,
      "Jihadist" = colors$palette$Jihadist
    ),
    labels = c("Right-wing", "Left-wing", "Jihadist")
  ) +
  labs(
    title = "C. Breakdown by Ideology",
    subtitle = "Right-wing attacks increased notably after 2014 (3-year rolling average)",
    y = "Annual attacks (3-year avg)",
    x = "Year"
  ) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

### |-  Panel D: Right-Wing Share of Total (3-year avg) ----
pd <-
  data_1994_2024 |>
  group_by(year) |>
  mutate(
    year_total = sum(attacks),
    pct = attacks / year_total * 100
  ) |>
  ungroup() |>
  filter(ideology == "Right") |>
  arrange(year) |>
  mutate(pct_smooth = rollmean(pct, k = 3, fill = NA, align = "center")) |>
  ggplot(aes(x = year, y = pct_smooth)) +
  annotate(
    "rect",
    xmin = 1994, xmax = 2024,
    ymin = 50, ymax = 100,
    fill = "#FFF7EB",
    alpha = 0.7
  ) +
  geom_hline(
    yintercept = 50,
    linetype   = "dashed",
    color      = "grey50",
    linewidth  = 0.4
  ) +
  geom_hline(
    yintercept = 75,
    linetype   = "dotted",
    color      = "grey60",
    linewidth  = 0.35
  ) +
  geom_line(
    linewidth = 1.2,
    color = colors$palette$Right,
    alpha = 0.95
  ) +
  annotate(
    "text",
    x = 1995,
    y = 52,
    label = "Majority (50%) threshold",
    size = 2.5,
    color = "grey50",
    hjust = 0,
    family = fonts$subtitle
  ) +
  annotate(
    "text",
    x = 1995,
    y = 77,
    label = "Supermajority (75%) threshold",
    size = 2.5,
    color = "grey50",
    hjust = 0,
    family = fonts$subtitle
  ) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  scale_y_continuous(
    breaks = seq(0, 100, 25),
    limits = c(0, 100),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "D. Right-Wing Share of Total",
    subtitle = "Typically 60–90%, with a dip to ~50% in the late 2000s (3-year rolling average)",
    y = "Right-wing % of annual attacks",
    x = "Year"
  ) +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

### |-  combined plot ----
combined_plots <- (pa | pb) / (pc | pd) +
  plot_layout(heights = c(1, 1.05))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(2.3),
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
        lineheight = 1.2,
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

# ─ Session info ────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-18
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
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
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
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
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
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
# P zoo         * 1.8-12   2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────
# > 