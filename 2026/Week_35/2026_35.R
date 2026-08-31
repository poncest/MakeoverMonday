## Challenge: #MakeoverMonday 2026 week 35
## Data:      Taylor Swift Tours

## Author:    Steven Ponce
## Date:      2026-08-31

## Article
# https://adashofdata.com/2023/03/01/a-data-scientist-breaks-down-all-10-taylor-swift-albums-the-extended-version/

## Data
# https://pub-cee805df54de4b6c8f93bee984e3c725.r2.dev/datasets/taylor-swift-tours/MM2026%20wk35.xlsx

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel(
  "data/2026/MM2026_wk35.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- I am HERE

# Row for "You Belong with Me" (The Red Tour) carries Speak Now Tour's
# dates (2011-02-09 / 2012-03-18) instead of Red Tour's own
# (2013-03-13 / 2014-06-12) -- confirmed against the other 16 Red Tour
# rows, which are internally consistent.
df_raw <- df_raw |>
  mutate(
    start_date = if_else(
      tour == "The Red Tour", as_datetime("2013-03-13"), start_date
    ),
    end_date = if_else(
      tour == "The Red Tour", as_datetime("2014-06-12"), end_date
    )
  )

### |- chronological ordering ----
tour_levels <- c(
  "Fearless Tour", "Speak Now Tour", "The Red Tour",
  "The 1989 Tour", "Reputation Tour", "The Eras Tour"
)

album_levels <- c(
  "Taylor Swift", "Fearless", "Speak Now", "Red", "1989",
  "Reputation", "Lover", "Folklore", "Evermore", "Midnights"
)

### |- guard against silent level-order corruption ----
album_mismatch <- setdiff(unique(df_raw$album), album_levels)
if (length(album_mismatch) > 0) {
  stop(
    "Album value(s) in data not found in album_levels: ",
    paste(album_mismatch, collapse = ", ")
  )
}

### |- build matrix data ----
matrix_data <- df_raw |>
  mutate(
    tour  = factor(tour, levels = tour_levels),
    album = factor(album, levels = album_levels)
  ) |>
  summarise(
    n_songs = n_distinct(song),
    .by = c(tour, album)
  ) |>
  complete(tour, album, fill = list(n_songs = 0)) |>
  mutate(
    tour_total = sum(n_songs),
    album_share = if_else(tour_total > 0, n_songs / tour_total, 0),
    .by = tour
  )

### |- verification ----
stopifnot(n_distinct(matrix_data$tour) == 6)
stopifnot(n_distinct(matrix_data$album) == 10)
stopifnot(nrow(matrix_data) == 60)
stopifnot(
  matrix_data |>
    summarise(total_share = sum(album_share), .by = tour) |>
    pull(total_share) |>
    (\(x) all(near(x, 1)))()
)

### |- split for layered encoding (explicit absence vs. intensity) ----
zero_data <- matrix_data |> filter(n_songs == 0)
tile_data <- matrix_data |> filter(n_songs > 0)

### |- label every nonzero cell with its share ----
label_data <- tile_data |>
  mutate(label = label_percent(accuracy = 1)(album_share))


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors()

zero_fill_col   <- "#FAFAF9"
zero_border_col <- "#E8E6E1"
label_dark_col  <- "#2B2B2B"
label_light_col <- "#FDFBF9"

### |- titles and caption ----
title_text <- str_glue("Broader Reach, Same Story -- Until Eras")

subtitle_text <- str_glue(
  "Taylor Swift's tours had been reaching across more albums for years. ",
  "But until Eras, most songs still came from one album."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 35,
  source_text = str_glue(
    "A Dash of Data<br>",
    "Note: cell shows the share of distinct songs from that album on the ",
    "tour; empty cells indicate no songs from that album."
  )
)

### |- typography color hierarchy ----
title_col    <- "#1A1A1A"
subtitle_col <- "#595959"
axis_col     <- "#595959"
caption_col  <- "#9C9C9C"

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 0, vjust = 0, color = axis_col),
    axis.text.y = element_text(hjust = 1, color = axis_col),
    legend.position = "top",
    legend.justification = "right",
    legend.title = element_text(size = rel(0.7), color = axis_col),
    legend.text = element_text(size = rel(0.65), color = axis_col),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.18, "cm"),
    plot.title = element_textbox_simple(
      size = rel(1.6), face = "bold", color = title_col,
      margin = margin(b = 6), family = fonts$title_1
    ),
    plot.subtitle = element_textbox_simple(
      size = rel(0.85), color = subtitle_col,
      margin = margin(b = 8), family = fonts$title_1
    ),
    plot.caption = element_textbox_simple(
      size = rel(0.6), color = caption_col,
      margin = margin(t = 12), family = fonts$caption
    )
  )
)

theme_set(weekly_theme)

### |-  plot ----
p <- matrix_data |>
  ggplot(aes(x = album, y = tour)) +
  geom_tile(
    data = zero_data,
    fill = zero_fill_col,
    color = zero_border_col,
    linewidth = 0.3
  ) +
  geom_tile(
    data = tile_data,
    aes(fill = album_share),
    color = "white",
    linewidth = 0.7
  ) +
  geom_text(
    data = label_data,
    aes(
      label = label,
      color = album_share >= 0.5
    ),
    size = 3.3,
    family = fonts$text,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_discrete(position = "top", limits = album_levels) +
  scale_y_discrete(limits = rev(tour_levels)) +
  scale_fill_gradient(
    low = "#F4F1ED", high = "#722F37",
    limits = c(0, 1), breaks = c(0, 0.5, 1),
    labels = label_percent(accuracy = 1),
    name = "Share of distinct songs",
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0,
      barwidth = unit(2.4, "cm"), barheight = unit(0.16, "cm")
    )
  ) +
  scale_color_manual(
    values = c(`TRUE` = label_light_col, `FALSE` = label_dark_col)
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL, y = NULL
  ) +
  coord_cartesian(clip = "off")

### |- Preview ----
p +
  canvas(width = 10, height = 6, units = "in")

### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_35", "2026_35.png"),
  width = 10, height = 6, dpi = 320
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
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-31
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# evaluate       1.0.5   2025-08-27 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
# fastmap        1.2.0   2024-05-15 [1] CRAN (R 4.6.0)
# forcats      * 1.0.1   2025-09-25 [1] CRAN (R 4.6.0)
# generics       0.1.4   2025-05-09 [1] CRAN (R 4.6.0)
# ggplot2      * 4.0.3   2026-04-22 [1] CRAN (R 4.6.0)
# ggtext       * 0.1.2   2022-09-16 [1] CRAN (R 4.6.0)
# ggview       * 0.2.2   2025-07-05 [1] CRAN (R 4.6.0)
# glue         * 1.8.1   2026-04-17 [1] CRAN (R 4.6.0)
# P graphics     * 4.6.1   2026-06-25 [1] local
# P grDevices    * 4.6.1   2026-06-25 [1] local
# P grid           4.6.1   2026-06-25 [1] local
# gridtext       0.1.6   2026-02-19 [1] CRAN (R 4.6.0)
# gtable         0.3.6   2024-10-25 [1] CRAN (R 4.6.0)
# haven          2.5.5   2025-05-30 [1] CRAN (R 4.6.0)
# here         * 1.0.2   2025-09-15 [1] CRAN (R 4.6.0)
# hms            1.1.4   2025-10-17 [1] CRAN (R 4.6.0)
# htmltools      0.5.9   2025-12-04 [1] CRAN (R 4.6.0)
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# knitr          1.51    2025-12-20 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# readxl         1.5.0   2026-05-16 [1] CRAN (R 4.6.0)
# repr           1.1.7   2024-03-22 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
# skimr          2.2.2   2026-01-10 [1] CRAN (R 4.6.0)
# snakecase      0.11.1  2023-08-27 [1] CRAN (R 4.6.0)
# P stats        * 4.6.1   2026-06-25 [1] local
# stringi        1.8.7   2025-03-27 [1] CRAN (R 4.6.0)
# stringr      * 1.6.0   2025-11-04 [1] CRAN (R 4.6.0)
# styler         1.11.0  2025-10-13 [1] CRAN (R 4.6.0)
# sysfonts     * 0.8.9   2024-03-02 [1] CRAN (R 4.6.0)
# systemfonts    1.3.2   2026-03-05 [1] CRAN (R 4.6.0)
# textshaping    1.0.5   2026-03-06 [1] CRAN (R 4.6.0)
# tibble       * 3.3.1   2026-01-11 [1] CRAN (R 4.6.0)
# tidyr        * 1.3.2   2025-12-19 [1] CRAN (R 4.6.0)
# tidyselect     1.2.1   2024-03-11 [1] CRAN (R 4.6.0)
# tidyverse    * 2.0.0   2023-02-22 [1] CRAN (R 4.6.0)
# timechange     0.4.0   2026-01-29 [1] CRAN (R 4.6.0)
# P tools          4.6.1   2026-06-25 [1] local
# tzdb           0.5.0   2025-03-15 [1] CRAN (R 4.6.0)
# utf8           1.2.6   2025-06-08 [1] CRAN (R 4.6.0)
# P utils        * 4.6.1   2026-06-25 [1] local
# vctrs          0.7.3   2026-04-11 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# >
