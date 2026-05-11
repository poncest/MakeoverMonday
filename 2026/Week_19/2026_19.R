## Challenge: #MakeoverMonday 2026 week 19
## Data:      US Population Migration
## Author:    Steven Ponce
## Date:      2026-05-11

## Article
# https://netmigration.wisc.edu/

## Data
# https://data.world/makeovermonday/2026w19-us-population-migration

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, patchwork, sf, tigris
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
df_raw <- read_csv("data/2026/US County PopMigration.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- state shapefile ----
states_sf <- states(cb = TRUE, resolution = "20m", year = 2020) |>
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS")) |>
  select(STATEFP, NAME, geometry)

### |- state-level aggregation ----
df_states <- df_raw |>
  filter(county == "State Total") |>
  transmute(
    state,
    state_fips,
    female_pop   = female_population_2020,
    male_pop     = male_population_2020,
    total_pop    = female_pop + male_pop,
    net_female   = net_female_migrants,
    net_male     = net_male_migrants,
    net_total    = net_female + net_male,

    # Rates per 100 residents (2020 population as denominator)
    female_rate  = net_female / female_pop * 100,
    male_rate    = net_male / male_pop * 100,
    net_rate     = net_total / total_pop * 100,

    # Gender gap: positive = female rate exceeds male rate
    gender_gap   = female_rate - male_rate
  )

### |- winsorize net_rate at 5th/95th ----
rate_lo <- quantile(df_states$net_rate, 0.05, na.rm = TRUE)
rate_hi <- quantile(df_states$net_rate, 0.95, na.rm = TRUE)

### |- winsorize gender gap at 2nd/98th percentile ----
gap_lo <- quantile(df_states$gender_gap, 0.02, na.rm = TRUE)
gap_hi <- quantile(df_states$gender_gap, 0.98, na.rm = TRUE)

df_states <- df_states |>
  mutate(
    net_rate_w    = pmin(pmax(net_rate, rate_lo), rate_hi),
    gender_gap_w  = pmin(pmax(gender_gap, gap_lo), gap_hi)
  )

### |- join to shapefile ----
df_map <- states_sf |>
  left_join(df_states, by = c("STATEFP" = "state_fips")) |>
  filter(!is.na(net_rate))



## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    gain_hi     = "#1D5E8C",
    gain_lo     = "#9DBFD6",
    neutral     = "#F0EDE8",
    loss_lo     = "#D4A882",
    loss_hi     = "#8C3D1D",
    female_hi   = "#5C4B8A",
    female_lo   = "#C4B8DC",
    gap_neutral = "#F0EDE8",
    male_lo     = "#D4A87A",
    male_hi     = "#8A5C1D",
    text_dark   = "#2C2C2A",
    text_mid    = "#5F5E5A",
    background  = "#FAFAF7"
  )
)

col_gain_hi <- colors$palette$gain_hi
col_gain_lo <- colors$palette$gain_lo
col_neutral <- colors$palette$neutral
col_loss_lo <- colors$palette$loss_lo
col_loss_hi <- colors$palette$loss_hi
col_female_hi <- colors$palette$female_hi
col_female_lo <- colors$palette$female_lo
col_gap_neu <- colors$palette$gap_neutral
col_male_lo <- colors$palette$male_lo
col_male_hi <- colors$palette$male_hi
col_text <- colors$palette$text_dark
col_sub <- colors$palette$text_mid
col_bg <- colors$palette$background

### |-  titles and caption ----
title_text <- str_glue("The Sun Belt Drove America's Net Migration Gains")

subtitle_text <- str_glue(
  "State-level migration rates during the 2010s and differences between female and male migration patterns"
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 19,
  source_text = "University of Wisconsin Applied Population Laboratory<br>
  Note: Rates per 100 residents using 2020 Census population as denominator"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Background
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),

    # Grid 
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),

    # Legend 
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 7.5, color = col_sub),
    legend.text = element_text(size = 7, color = col_sub),
    legend.key.width = unit(1.6, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.margin = margin(t = 2, b = 0), 
    legend.box.margin = margin(t = -8), 

    strip.text = element_blank(),
    plot.margin = margin(2, 8, 2, 8) 
  )
)

theme_set(weekly_theme)


### |-  Panel A scale limits ----
rate_abs_max <- max(abs(c(rate_lo, rate_hi))) |> ceiling()

### |-  Panel A: Net Migration Rate ----
p_net <- ggplot(df_map) +
  geom_sf(
    aes(fill = net_rate_w),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c(col_loss_hi, col_loss_lo, col_neutral, col_gain_lo, col_gain_hi),
    values = rescale(c(-rate_abs_max, -2, 0, 2, rate_abs_max)),
    limits = c(-rate_abs_max, rate_abs_max),
    oob = squish,
    breaks = c(-rate_abs_max, 0, rate_abs_max),
    labels = c("Loss", "0", "Gain"),
    name = "Net migrants per 100 residents",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10,
      barheight = 0.35,
      ticks = FALSE
    )
  ) +
  annotate(
    "text",
    x = -78, y = 30,
    label = "Strongest gains concentrated\nin the Sun Belt",
    size = 2.6,
    color = col_sub,
    fontface = "plain",
    hjust = 0.5,
    lineheight = 1.3
  ) +
  labs(
    title = "Where Americans moved",
    subtitle = "Net migration rate, 2010s"
  ) +
  coord_sf(crs = 5070) +
  theme(
    plot.title = element_text(
      size = 11, face = "bold", color = col_text,
      margin = margin(b = 2), family = fonts$title
    ),
    plot.subtitle = element_text(
      size = 8, color = col_sub, family = fonts$subtitle,
      margin = margin(b = 4)
    )
  )

### |-  Panel B: Gender Gap (female rate − male rate) ----
gap_abs_max <- max(abs(c(gap_lo, gap_hi))) |> ceiling()

p_gap <- ggplot(df_map) +
  geom_sf(
    aes(fill = gender_gap_w),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c(col_male_hi, col_male_lo, col_gap_neu, col_female_lo, col_female_hi),
    values = rescale(c(-gap_abs_max, -0.5, 0, 0.5, gap_abs_max)),
    limits = c(-gap_abs_max, gap_abs_max),
    oob = squish,
    breaks = c(-gap_abs_max, 0, gap_abs_max),
    labels = c("More male-led", "Parity", "More female-led"),
    name = "Female minus male net rate (pp)",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 10,
      barheight = 0.35,
      ticks = FALSE
    )
  ) +
  annotate(
    "text",
    x = -78, y = 30,
    label = "Most Southern states\nskewed female",
    size = 2.6,
    color = col_sub,
    fontface = "plain",
    hjust = 0.5,
    lineheight = 1.3
  ) +
  labs(
    title = "Men and women didn't always move equally",
    subtitle = "Gender gap in net migration rate (female − male)"
  ) +
  coord_sf(crs = 5070) +
  theme(
    plot.title = element_text(
      size = 11, face = "bold", color = col_text,
      margin = margin(b = 2), family = fonts$title
    ),
    plot.subtitle = element_text(
      size = 8, color = col_sub,
      margin = margin(b = 4), family = fonts$subtitle
    )
  )

### |-  Combine plots ----
p_combined <- (p_net | p_gap) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = 16, face = "bold", color = col_text,
        margin = margin(b = 4), family = fonts$title
      ),
      plot.subtitle = element_markdown(
        size = 9, color = col_sub, lineheight = 1.4,
        margin = margin(b = 6), family = fonts$subtitle          
      ),
      plot.caption = element_markdown(
        size = 6.5, color = col_sub, hjust = 0,
        margin = margin(t = 6), family = fonts$caption
      ),
      plot.background = element_rect(fill = col_bg, color = NA),
      plot.margin = margin(12, 12, 8, 12)  
    )
  ) +
  plot_layout(widths = c(1, 1))


### |-  Preview ----
snap(p_combined)


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
 
# ─ Session info ────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-11
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# class          7.3-23   2025-01-01 [2] CRAN (R 4.5.3)
# classInt       0.4-11   2025-01-08 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# DBI            1.3.0    2026-02-25 [1] CRAN (R 4.5.3)
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# e1071          1.7-17   2025-12-18 [1] CRAN (R 4.5.3)
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
# httr           1.4.8    2026-02-13 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# KernSmooth     2.23-26  2025-01-01 [2] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
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
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# proxy          0.4-29   2025-12-29 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# rappdirs       0.3.4    2026-01-17 [1] CRAN (R 4.5.3)
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
# sf           * 1.1-0    2026-02-24 [1] CRAN (R 4.5.3)
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
# tigris       * 2.2.1    2025-04-16 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# units          1.0-1    2026-03-11 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# uuid           1.2-2    2026-01-23 [1] CRAN (R 4.5.2)
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
# ───────────────────────────────────────────────────────────────────────────────────