## Challenge: #MakeoverMonday 2025 week 49
## Data:      London crimes by income deprivation decile
## Author:    Steven Ponce
## Date:      2025-12-16

## Article
# https://trustforlondon.org.uk/data/crime-and-income-deprivation/

## Data
# https://data.world/makeovermonday/2025w49-london-crimes-by-income-deprivation-decile

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
  glue           # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

## Additional Data Sources
# Population estimates calculated from:
#   - London LSOA count: 4,835 LSOAs (confirmed from multiple sources)
#   - Average LSOA population: 1,722 residents (London Datastore)
#   - Source: https://data.london.gov.uk/dataset/lsoa-atlas-2n8zy/
#
# Note: Trust for London uses London-rebased income deprivation deciles
#       (ranking only London's 4,835 LSOAs into deciles, not England-wide rankings)

## CRITICAL LIMITATIONS AND ASSUMPTIONS
#
# 1. POPULATION ESTIMATES:
#    - We assume approximately equal population across income deprivation deciles
#    - Each decile: ~483-484 LSOAs × 1,722 residents = ~833,000 people
#    - Total estimated: 8.33 million (vs. actual 8.9 million in 2021 Census)
#
# 2. WHY ESTIMATES WERE NECESSARY:
#    - IoD2025 File 7 (official population denominators) was not accessible in analysis environment
#    - No readily available population data aggregated by income deprivation decile
#    - Per capita analysis requires population denominators for meaningful comparison
#
# 3. ASSUMPTION VALIDITY:
#    - Equal distribution is a simplification - actual populations likely vary by decile
#    - More deprived areas may have different population density than affluent areas
#    - Under equal-population assumption: 134% difference persists (raw counts = per capita rates)
#    - This suggests the crime disparity is real, not merely a population artifact
#
# 4. IMPLICATIONS FOR ANALYSIS:
#    - Per capita rates shown are based on estimated, not actual, populations
#    - True rates may vary if actual population distribution differs significantly
#    - All visualizations will clearly note this limitation in titles/subtitles/annotations

## DATA PREPARATION NOTE
#
# The CSV files loaded in this script (london_crime_by_deprivation_tidy.csv and
# london_total_crimes_by_deprivation.csv) were generated using a Python script
# that calculated per capita crime rates with population estimates.
#
# To reproduce these files from the original data:
#   1. See: `mm_2025_week49_prep.py` in this directory
#   2. Input: mom_week49_data.csv (raw crime counts from data.world)
#   3. Output: Two tidy CSV files with per capita rates
#
# The Python script documents:
#   • Population estimation methodology (4,835 LSOAs × 1,722 avg residents)
#   • Per capita rate calculations (crimes per 10,000 residents)
#   • All assumptions and limitations
#
# For full reproducibility, run the Python script first, then this R script.


## 2. READ IN THE DATA ----
total_crimes_by_decile <- read_csv("data/2025/london_total_crimes_by_deprivation.csv")


## 3. EXAMINING THE DATA ----
glimpse(total_crimes_by_decile)
skim(total_crimes_by_decile) |> summary()


## 4. TIDY DATA ----
# Baseline = least deprived (decile 10)
baseline_rate <- total_crimes_by_decile |>
  filter(decile_num == 10) |>
  pull(total_crimes_per_10k)

crime_gradient <- total_crimes_by_decile |>
  mutate(
    baseline_rate = baseline_rate,
    rate_ratio = total_crimes_per_10k / baseline_rate,
    pct_difference = ((total_crimes_per_10k - baseline_rate) / baseline_rate) * 100
  )

# Values for annotations
most_deprived_rate <- crime_gradient |> filter(decile_num == 1)  |> pull(total_crimes_per_10k)
least_deprived_rate <- crime_gradient |> filter(decile_num == 10) |> pull(total_crimes_per_10k)
overall_pct_diff <- crime_gradient |> filter(decile_num == 1) |> pull(pct_difference)

# Identify peak decile 
peak_decile <- crime_gradient |>
  slice_max(total_crimes_per_10k, n = 1, with_ties = FALSE) |>
  pull(decile_num)

peak_rate <- crime_gradient |>
  filter(decile_num == peak_decile) |>
  pull(total_crimes_per_10k)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    col_primary = "#1E3A5F",
    col_danger  = "#B5532F"
  )
)

### |-  Main titles ----
title_text <- "Estimated crime rates are ~134% higher in London’s most<br>deprived neighborhoods"

subtitle_text <- str_glue(
  "Per 10,000 residents by income deprivation decile (2024). ",
  "Rates use an equal-population estimate (~833k residents<br>per decile; interpret comparatively). "
)

caption_text <- create_social_caption_02_mm(
  mm_year = 2025, mm_week = 49,
  source_text = str_glue(
    "**Crime Data:** Trust for London, DataPoliceUK (2024) | ",
    "**Deprivation Rankings:** Indices of Multiple Deprivation 2025<br>"
  ),
  note_text = str_glue(
    "**Methodology:** Rates are normalized per 10,000 residents assuming equal population per decile (~833k). ",
    "Because true populations by decile may vary,<br>treat values as **comparative** rather than exact. ",
    "London-rebased deciles rank neighborhoods within London only.<br>"
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
    legend.justification = "right",
    legend.margin = margin(l = 12, b = 5),
    legend.key.size = unit(0.8, "cm"),
    legend.box.margin = margin(b = 10),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10), family = fonts$subtitle,
      color = "gray40" 
    ),
    axis.title.y = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(r = 10), family = fonts$subtitle,
      color = "gray40" 
    ),
    axis.text.x = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = "gray40"  
    ),
    axis.text.y = element_markdown(
      size = rel(0.85), family = fonts$subtitle,
      color = "gray40"
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

### |-  main plot ----
ggplot(crime_gradient, aes(x = decile_num, y = total_crimes_per_10k)) +
  # Geoms
  geom_area(fill = colors$palette$col_primary, alpha = 0.20) +
  geom_line(color = colors$palette$col_primary, linewidth = 1.5) +
  geom_point(color = colors$palette$col_primary, size = 3) +
  geom_point(
    data = crime_gradient |> filter(decile_num %in% c(1, 10)),
    size = 6, color = colors$palette$col_danger
  ) +
  # Annotations
  annotate(
    "text",
    x = 1.1, y = 1350,
    label = glue(
      "Most deprived areas (Decile 1):\n",
      "{comma(round(most_deprived_rate))} crimes per 10k\n",
      "({round(overall_pct_diff)}% higher than least deprived)"
    ),
    size = 3.6, hjust = 0,
    fontface = "bold", lineheight = 1.1,
    color = colors$palette$col_danger
  ) +
  annotate(
    "text",
    x = 10.0, y = 1050,
    label = glue(
      "Least deprived areas (Decile 10):\n",
      "{comma(round(least_deprived_rate))} crimes per 10k\n",
      "(Baseline for comparison)"
    ),
    size = 3.6, hjust = 1,
    fontface = "bold", lineheight = 1.1,
    color = colors$palette$col_primary
  ) +
  annotate(
    "text",
    x = peak_decile, y = 2700,
    label = "Peak occurs around deciles 2–3",
    size = 3.2,
    fontface = "bold",
    color = colors$palette$col_primary,
    vjust = -0.2
  ) +
  annotate(
    "text",
    x = 5.5, y = 200,
    label = str_wrap(
      "Note: Income deprivation deciles rank London's neighborhoods from most (1) to least (10) deprived. Each decile represents ~10% of areas.",
      width = 86
    ),
    size = 3,
    color = "gray30",
    lineheight = 1.1,
    hjust = 0.5
  ) +
  # Scales
  scale_x_continuous(
    breaks = 1:10,
    labels = c(
      "1\nMost\nDeprived", "2", "3", "4", "5", "6", "7", "8", "9",
      "10\nLeast\nDeprived"
    )
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, 2700),
    breaks = seq(0, 2500, 500)
  ) +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Income Deprivation Decile",
    y = "Crimes per 10,000 Residents"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.9),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.88),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.5,
      margin = margin(t = 5, b = 20)
    ),
    plot.caption = element_markdown(
      size = rel(0.65),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
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

# ─ Session info ──────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-12-15
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark    1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
# curl          6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] RSPM (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# evaluate      1.0.3    2025-01-10 [1] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
# fastmap       1.2.0    2024-05-15 [1] RSPM (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] RSPM (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] RSPM (R 4.4.0)
# P ggplot2     * 3.5.2    2025-04-09 [?] RSPM (R 4.4.0)
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# renv          1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# repr          1.1.7    2024-03-22 [1] RSPM (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot     2.0.4    2023-11-05 [1] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg          2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# showtext    * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb  * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
# skimr       * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
# snakecase     0.11.1   2023-08-27 [1] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
# stringr     * 1.5.1    2023-11-14 [1] RSPM (R 4.4.0)
# svglite       2.1.3    2023-12-08 [1] RSPM (R 4.4.0)
# sysfonts    * 0.8.9    2024-03-02 [1] RSPM (R 4.4.0)
# systemfonts   1.2.1    2025-01-20 [1] RSPM (R 4.4.0)
# textshaping   1.0.0    2025-01-20 [1] RSPM (R 4.4.0)
# tibble      * 3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# timechange    0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# tzdb          0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────
# > 