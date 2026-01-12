## Challenge: #MakeoverMonday 2026 week 02
## Data:      The Biggest Housing Bubble Risks Globally
## Author:    Steven Ponce
## Date:      2025-01-12

## Article
# https://www.visualcapitalist.com/sp/ter01-the-biggest-housing-bubble-risks-globally/

## Data
# https://data.world/makeovermonday/2026wk2-the-biggest-housing-bubble-risks-globally

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, janitor, skimr, scales, ggtext, showtext, glue,
  patchwork, ggrepel
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

### |- Current data (2025) ----
housing_bubble_raw <- readxl::read_excel("data/2026/Housing bubbles.xlsx") |>
  clean_names()

### |- Historical data (2020-2025) from UBS reports ----

# Source: UBS Global Real Estate Bubble Index reports 2020-2025
# https://www.ubs.com/global/en/wealth-management/insights/2024/global-real-estate-bubble-index.html
# Note: Historical data compiled from annual UBS reports

historical_raw <- tribble(
  ~city, ~year_2020, ~year_2021, ~year_2022, ~year_2023, ~year_2024, ~year_2025,
  "Miami", 0.5, 0.8, 1.4, 1.8, 1.8, 1.7,
  "Tokyo", 0.7, 0.7, 0.6, 0.9, 1.2, 1.6,
  "Zurich", 1.5, 1.8, 1.7, 1.7, 1.5, 1.6,
  "Los Angeles", 0.7, 1.0, 1.2, 1.2, 1.2, 1.1,
  "Toronto", 1.8, 2.0, 2.2, 1.2, 0.9, 0.8,
  "Frankfurt", 1.5, 2.2, 2.2, 1.1, 0.8, 0.8,
  "Munich", 1.8, 2.3, 2.0, 1.1, 0.9, 0.6,
  "Hong Kong", 1.8, 1.7, 1.7, 1.2, 0.7, 0.5,
  "Vancouver", 1.0, 1.6, 1.7, 1.1, 0.9, 0.8
)

## 3. EXAMINING THE DATA ----
glimpse(housing_bubble_raw)
skim(housing_bubble_raw) |> summary()


## 4. TIDY DATA ----
slope_data <- historical_raw |>
  mutate(
    change     = year_2025 - year_2020,
    direction  = if_else(change < 0, "Cooling", "Rising"),
    label_2020 = sprintf("%s  %.1f", city, year_2020),
    label_2025 = sprintf("%s  %.1f", city, year_2025)
  ) |>
  arrange(desc(year_2025)) |>
  mutate(city = factor(city, levels = unique(city)))


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    cooling    = "#0D7377",
    rising     = "#C4C4C4"
  )
)

### |-  Main titles ----
title_text <- "The Global Housing Bubble is Losing Air"

subtitle_text <- str_glue(
  "While Miami and Tokyo continue to heat up, the primary global trend is a **significant cooling** of major hubs.<br>",
  "Former 'Bubble Risk' leaders like ",
  "<span style='color:{colors$palette$cooling};'>**Toronto, Frankfurt, and Hong Kong**</span>",
  " have seen their risk scores halved since 2020."
)

caption_text <- create_social_caption(
  mm_year = 2026, mm_week = 02,
  source_text = str_glue(
    "UBS Global Real Estate Bubble Index 2025"
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

### |-  p1: slope chart ----
p1 <-
ggplot(slope_data) +
  # Geoms
  geom_segment(
    aes(
      x = 1, xend = 2,
      y = year_2020, yend = year_2025,
      color = direction, alpha = direction, linewidth = direction
    )
  ) +
  geom_point(aes(x = 1, y = year_2020, color = direction, alpha = direction), size = 2) +
  geom_point(aes(x = 2, y = year_2025, color = direction, alpha = direction), size = 3) +
  geom_text_repel(
    aes(x = 1, y = year_2020, label = label_2020),
    direction = "y",
    hjust = 1,
    nudge_x = -0.15,
    size = 3.5,
    segment.color = NA,
    family = fonts$text,
    color = colors$text     
  ) +
  geom_text_repel(
    aes(x = 2, y = year_2025, label = label_2025, color = direction),
    direction = "y",
    hjust = 0,
    nudge_x = 0.15,
    size = 3.8,
    fontface = "bold",
    segment.color = NA,
    family = fonts$text,
    seed = 123
  ) +
  # Scales
  scale_color_manual(values = c("Cooling" = colors$palette$cooling, "Rising" = colors$palette$rising)) +
  scale_alpha_manual(values = c("Cooling" = 1, "Rising" = 0.4)) +
  scale_linewidth_manual(values = c("Cooling" = 1.2, "Rising" = 0.5)) +
  scale_x_continuous(
    limits = c(0.4, 2.6),
    breaks = c(1, 2),
    labels = c("2020", "2025")
  ) +
  # Labs
  labs(subtitle = "Bubble Risk Index: 5-Year Trajectory", x = NULL, y = NULL) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
    )

### |-  p2: quadrant chart ----
p2 <-
ggplot(slope_data, aes(x = year_2025, y = change)) +
  # Geoms
  geom_hline(yintercept = 0, color = "grey80", linetype = "dashed") +
  geom_vline(xintercept = 1, color = "grey80", linetype = "dashed") +
  geom_point(aes(color = direction, alpha = direction, size = abs(change))) +
  geom_text_repel(
    aes(label = city, color = direction),
    size = 3.5,
    fontface = "bold",
    box.padding = 0.5, 
    seed = 123
  ) +
  # Scales
  scale_color_manual(values = c("Cooling" = colors$palette$cooling, "Rising" = colors$palette$rising)) +
  scale_alpha_manual(values = c("Cooling" = 1, "Rising" = 0.4)) +
  scale_size(range = c(2.5, 8)) +
  coord_cartesian(xlim = c(0, 2)) +
  # Labs
  labs(
    subtitle = "Current Risk vs. Velocity of Change",
    x = "Risk Score in 2025",
    y = "5-Year Net Change"
  ) +
  # Theme
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "grey95")
  )

### |-  combined plot ----
combined_plot <- p1 + p2 +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(2.4),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 5, b = 10)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.85),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.5,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.55),
        family = fonts$subtitle,
        color = colors$caption,
        hjust = 0,
        lineheight = 1.4,
        margin = margin(t = 20, b = 5)
      ),
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

# ─ Session info ───────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-01-12
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cellranger    1.1.0    2016-07-27 [1] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark    1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] RSPM (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# readxl        1.4.3    2023-07-06 [1] RSPM (R 4.4.0)
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
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────