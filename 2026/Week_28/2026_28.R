## Challenge: #MakeoverMonday 2026 week 28
## Data:      Park & Recreation Amenities
## Author:    Steven Ponce
## Date:      2026-07-13

## Article
# https://www.tpl.org/city-park-facts

## Data
# https://makeovermonday.vercel.app/datasets (2026 week 28)

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_tsv("data/2026/Amenities_Data__Clean.csv",
    locale = locale(encoding = "UTF-16LE")
  ) |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- pivot measures wide ----
df_wide <- df_raw |>
  pivot_wider(
    names_from = measure_names, 
    values_from = measure_values
    )

### |- restrict to climate zones with adequate sample size ----
usable_zones <- df_raw |>
  distinct(city, climate_zone) |>
  filter(!is.na(climate_zone)) |>
  count(climate_zone) |>
  filter(n >= 10) |>
  pull(climate_zone)

### |- the 5 amenities that survived FDR correction in Phase 0 ----
sig_amenities <- c(
  "Outdoor tennis courts, dedicated",
  "Basketball Hoops (Includes schoolyards)",
  "Combined fields and diamonds",
  "Baseball and Softball Diamonds",
  "Playgrounds (includes schoolyards)"
)

### |- analysis-ready subset with cooler/warmer grouping ----
df_climate <- df_wide |>
  filter(amenity_type %in% sig_amenities, climate_zone %in% usable_zones) |>
  mutate(
    climate_zone = factor(climate_zone,
      levels = c("Cold", "Mixed-Humid", "Hot-Humid", "Hot-Dry")
    ),
    climate_group = if_else(
      climate_zone %in% c("Cold", "Mixed-Humid"),
      "Cooler", "Warmer"
    )
  )

### |- short display labels ----
amenity_labels <- c(
  "Basketball Hoops (Includes schoolyards)" = "Basketball Hoops",
  "Outdoor tennis courts, dedicated" = "Tennis Courts",
  "Playgrounds (includes schoolyards)" = "Playgrounds",
  "Baseball and Softball Diamonds" = "Baseball & Softball",
  "Combined fields and diamonds" = "Combined Fields"
)

### |- individual zone medians ----
plot_data_zones_raw <- df_climate |>
  group_by(amenity_type, climate_zone, climate_group) |>
  summarise(median_pc = median(`Per Capita (10k)`, na.rm = TRUE), .groups = "drop")

### |- cooler/warmer group averages ----
group_avg_data <- plot_data_zones_raw |>
  group_by(amenity_type, climate_group) |>
  summarise(group_avg = mean(median_pc), .groups = "drop")

### |- effect size + row order, computed from group_avg_data only ----
effect_order <- group_avg_data |>
  pivot_wider(names_from = climate_group, values_from = group_avg) |>
  mutate(
    ratio = Cooler / Warmer,
    abs_diff = Cooler - Warmer,
    pct_diff = (Cooler - Warmer) / Warmer * 100,
    magnitude = (Cooler + Warmer) / 2
  ) |>
  arrange(desc(magnitude))

amenity_order <- effect_order$amenity_type

### |- apply shared factor levels to both plotting data frames ----
plot_data_zones <- plot_data_zones_raw |>
  mutate(
    amenity_type = factor(amenity_type, levels = rev(amenity_order)),
    amenity_label = amenity_labels[as.character(amenity_type)]
  )

plot_data_groups <- group_avg_data |>
  mutate(
    amenity_type = factor(amenity_type, levels = rev(amenity_order)),
    amenity_label = amenity_labels[as.character(amenity_type)]
  )

### |- ratio annotation for every row ----
ratio_label_data <- effect_order |>
  mutate(
    amenity_type = factor(amenity_type, levels = rev(amenity_order)),
    amenity_label = amenity_labels[as.character(amenity_type)],
    ratio_label = paste0(scales::number(ratio, accuracy = 0.1), "x"),
    x_mid = (Cooler + Warmer) / 2
  )

### |- separate callout for the largest ABSOLUTE gap (basketball) ----
abs_gap_label_data <- effect_order |>
  mutate(
    amenity_type  = factor(amenity_type, levels = rev(amenity_order)),
    amenity_label = amenity_labels[as.character(amenity_type)]
  ) |>
  slice_max(abs_diff, n = 1) |>
  mutate(callout = paste0("Largest raw gap (+", scales::number(abs_diff, accuracy = 0.1), " per 10k)"))


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    cooler  = "#1B3A4B",
    warmer  = "#A85C36",
    neutral = "gray70"
  )
)

group_colors <- c(Cooler = "#1B3A4B", Warmer = "#A85C36")

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- caption ----
caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 28,
  source_text = paste0(
    "Trust for Public Land, City Park Facts<br>",
    "Note: Cooler/Warmer is a descriptive grouping of TPL's own climate_zone ",
    "categories, shown for context; all four source zones are plotted. Filled ",
    "points represent the average of the two climate-zone medians within each ",
    "group; open circles show the individual climate-zone medians. ",
    "Comparison reflects per-capita inventory only — not spend, acreage, or demand."
  )
)

### |- base theme ----
base_theme <- create_base_theme(colors)

### |- titles ----
title_text <- str_glue("Traditional Park Amenities Are More Common in Cooler-Climate Cities")

subtitle_text <- str_glue(
  "Across five traditional park amenities, cooler-climate cities (Cold + ",
  "Mixed-Humid) consistently provide more facilities per capita than ",
  "warmer-climate cities (Hot-Humid + Hot-Dry). The pattern holds across ",
  "all five amenities, with no exceptions"
)

### |- plot theme ----
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(size = 19, face = "bold", margin = margin(b = 6), family = fonts$title_1),
    plot.subtitle = element_textbox_simple(
      size = 11.5, color = "gray30", margin = margin(b = 18), lineheight = 1.3
    ),
    plot.caption = element_textbox_simple(
      size = 8, color = "gray45", hjust = 0, margin = margin(t = 14), lineheight = 1.3, family = fonts$caption
    ),
    axis.text.y = element_text(size = 11.5, face = "bold", hjust = 1),
    axis.text.x = element_text(size = 9, color = "gray40"),
    axis.title.x = element_text(size = 9.5, color = "gray40", margin = margin(t = 8)),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )
)
theme_set(weekly_theme)

### |- plot ----
p <- ggplot() +
  geom_segment(
    data = plot_data_groups |>
      select(amenity_type, climate_group, group_avg) |>
      pivot_wider(names_from = climate_group, values_from = group_avg),
    aes(x = Warmer, xend = Cooler, y = amenity_type, yend = amenity_type),
    color = "gray75", linewidth = 0.9
  ) +
  geom_segment(
    data = plot_data_zones |>
      left_join(
        plot_data_groups |> select(amenity_type, climate_group, group_avg),
        by = c("amenity_type", "climate_group")
      ),
    aes(
      x = median_pc, xend = group_avg, y = amenity_type, yend = amenity_type,
      color = climate_group
    ),
    linewidth = 0.4, alpha = 0.5
  ) +
  geom_point(
    data = plot_data_zones,
    aes(x = median_pc, y = amenity_type, color = climate_group),
    shape = 21, fill = "#F4F3EE", size = 1.8, stroke = 0.5, alpha = 0.55
  ) +
  geom_point(
    data = plot_data_groups,
    aes(x = group_avg, y = amenity_type, color = climate_group),
    size = 6
  ) +
  geom_text(
    data = ratio_label_data,
    aes(x = x_mid, y = amenity_type, label = ratio_label),
    nudge_y = 0.32, family = fonts$text, fontface = "bold",
    size = 3.6, color = "gray30"
  ) +
  geom_text(
    data = abs_gap_label_data,
    aes(x = Cooler, y = amenity_type, label = callout),
    nudge_y = -0.32, nudge_x = 0.15, hjust = 0,
    family = fonts$text, fontface = "italic",
    size = 3, color = "gray45"
  ) +
  scale_y_discrete(labels = amenity_labels) +
  scale_color_manual(
    values = group_colors,
    breaks = c("Warmer", "Cooler"),
    labels = c("Warmer climates", "Cooler climates")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(
    title = title_text, subtitle = subtitle_text, caption = caption_text,
    x = "Facilities per 10,000 residents (median)"
  )


### |-  Preview ----
snap(p)


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

# ─ Session info ───────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-07-13
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────