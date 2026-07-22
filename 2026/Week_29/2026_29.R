## Challenge: #MakeoverMonday 2026 week 29
## Data:      US Data Center Locations
## Author:    Steven Ponce
## Date:      2026-07-22

## Article
# https://experience.arcgis.com/experience/5a4d072ad01449bba5698a80103fb909

## Data
# https://pub-cee805df54de4b6c8f93bee984e3c725.r2.dev/datasets/us-data-center-locations/Data_Centers_Database.xlsx

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
  height = 5.5,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel("data/2026/Data_Centers_Database.xlsx", sheet = 1) |>
  clean_names()

## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- clean status / sizerank, collapse pipeline categories ----
df_tidy <- df_raw |>
  mutate(
    status = str_squish(status),
    sizerank = str_squish(sizerank),
    sizerank = if_else(
      sizerank == "Hyperscale (101-999 MW)",
      "Hyperscale (100-999 MW)",
      sizerank
    )
  ) |>
  filter(sizerank != "Unknown") |>
  mutate(
    status_bucket = case_when(
      status == "Operating" ~ "Operating",
      status %in% c("Proposed", "Approved/Permitted/Under construction") ~ "In pipeline",
      TRUE ~ "Stalled/Other"
    ),
    sizerank = factor(
      sizerank,
      levels = c(
        "Small (0-10 MW)",
        "Medium (11-50 MW)",
        "Large (51-99 MW)",
        "Hyperscale (100-999 MW)",
        "Mega campus (>1,000 MW)"
      )
    )
  )

### |- summarize: % operating by size class ----
plot_data <- df_tidy |>
  summarise(
    n           = n(),
    n_operating = sum(status_bucket == "Operating"),
    .by = sizerank
  ) |>
  mutate(
    pct_operating = n_operating / n * 100,
    label_pct     = paste0(round(pct_operating), "%"),
    label_n       = paste0("n=", n)
  ) |>
  arrange(sizerank) |>
  ungroup()  


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    accent     = "#1E3A5F",
    track      = "#F2F2F2",
    text       = "#1A1A1A",
    muted      = "#6B6B6B",
    background = "#FFFFFF"
  )
)$palette

### |- annotation / layout scalars ----
n_known      <- nrow(df_tidy)
n_total      <- nrow(df_raw)
pct_known    <- round(n_known / n_total * 100, 1)

track_max    <- 100
n_label_x    <- 112
x_axis_max   <- 130
bar_width    <- 0.62

### |- titles and caption ----
title_text <- "The biggest data centers are mostly still in the pipeline"

subtitle_text <- glue(
  "The share operating falls from {round(plot_data$pct_operating[plot_data$sizerank == 'Small (0-10 MW)'])}% ",
  "among small facilities to {round(plot_data$pct_operating[plot_data$sizerank == 'Mega campus (>1,000 MW)'])}% ",
  "among mega campuses"
)

caption_lead <- glue(
  "Larger facilities are far less likely to be operating: **",
  "{round(plot_data$pct_operating[plot_data$sizerank == 'Small (0-10 MW)'])}% of small ",
  "facilities are operational, compared with just ",
  "{round(plot_data$pct_operating[plot_data$sizerank == 'Mega campus (>1,000 MW)'])}% ",
  "of mega campuses.**"
)

caption_note <- glue(
  "Size class was available for {n_known} of {scales::comma(n_total)} facilities ",
  "({pct_known}%); facilities with unknown size were excluded. Operating share is shown ",
  "directly; the remainder includes pipeline and other statuses. \u2018In pipeline\u2019 combines ",
  "Proposed and Approved/Permitted/Under\u00A0construction statuses. Status reflects the database ",
  "snapshot, not a construction forecast.<br>"
)

caption_text <- glue(
  "{caption_lead}<br>",
  "{caption_note}<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 29,
    source_text = "US Data Center Database, ArcGIS"
  )
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = clrs$background, color = NA),
    panel.background = element_rect(fill = clrs$background, color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(
      family = fonts$text, color = clrs$text, size = 11, hjust = 1
    ),
    plot.title = element_text(
      family = fonts$title_1, color = clrs$text, size = 22, face = "bold"
    ),
    plot.subtitle = element_text(
      family = fonts$subtitle, color = clrs$muted, size = 10,
      margin = margin(b = 14)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, color = clrs$muted, size = 6,
      margin = margin(t = 12)
    ),
    plot.margin = margin(20, 30, 20, 20)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- ggplot(plot_data, aes(y = sizerank)) +
  geom_col(
    aes(x = track_max),
    fill  = clrs$track,
    width = bar_width
  ) +
  geom_col(
    aes(x = pct_operating),
    fill  = clrs$accent,
    width = bar_width
  ) +
  geom_text(
    aes(x = pct_operating, label = label_pct),
    hjust = -0.18,
    size = 3.6,
    fontface = "bold",
    color = clrs$accent,
    family = fonts$text
  ) +
  geom_text(
    aes(x = n_label_x, label = label_n),
    hjust = 0,
    size = 3.2,
    color = clrs$muted,
    family = fonts$text
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(
    limits = c(0, x_axis_max),
    expand = c(0, 0)
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

### |- Preview ----
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

# ─ Session info ──────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-07-22
# rstudio  2026.07.1+147 Pacific Dogwood (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.6.1    2026-06-25 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.6.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.6.0)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1    2026-06-25 [1] local
# curl           7.1.0    2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1    2026-06-25 [1] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.6.0)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.6.0)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.6.0)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.6.0)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.6.0)
# glue         * 1.8.1    2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1    2026-06-25 [1] local
# P grDevices    * 4.6.1    2026-06-25 [1] local
# P grid           4.6.1    2026-06-25 [1] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.6.0)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.6.0)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.6.0)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.6.0)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.6.0)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10     2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.6.0)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.6.0)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1    2026-06-25 [1] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.6.0)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2    2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.6.0)
# readxl         1.5.0    2026-05-16 [1] CRAN (R 4.6.0)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.6.0)
# rlang          1.3.0    2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0   2026-06-11 [1] CRAN (R 4.6.0)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.6.0)
# S7             0.2.2    2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4    2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.6.0)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.6.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1    2026-06-25 [1] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.6.0)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1    2026-06-25 [1] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.6.0)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1    2026-06-25 [1] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.6.0)
# withr          3.0.3    2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60     2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0    2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────
