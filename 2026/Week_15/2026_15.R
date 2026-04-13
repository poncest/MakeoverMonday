## Challenge: #MakeoverMonday 2026 week 15
## Data:      Big Tech Hiring
## Author:    Steven Ponce
## Date:      2026-04-13

## Article
# https://www.trueup.io/big-tech-hiring

## Data
# https://data.world/makeovermonday/2026w15-tech-hiring

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, ggrepel
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
  height = 7.5,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel("data/2026/MM 2026wk15.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
df <- df_raw |>
  mutate(
    layoff_rate = layoffs_past_2yr_people / employees,
    hiring_rate = open_jobs / employees,
    net_signal  = hiring_rate - layoff_rate,
    expanding = if_else(net_signal >= 0, "expanding", "contracting"),
    is_nvidia   = company == "NVIDIA"
  )

### |- extract point coordinates for segments ----
nvidia_x <- filter(df, company == "NVIDIA")$layoff_rate
nvidia_y <- filter(df, company == "NVIDIA")$hiring_rate
msft_x <- filter(df, company == "Microsoft")$layoff_rate
msft_y <- filter(df, company == "Microsoft")$hiring_rate


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_expansion   = "#2A8C7A",
    col_contraction = "#8B2E3A", 
    col_diagonal    = "#C8C8C8",  
    col_annotation  = "#3D3D3D",   
    col_segment     = "gray55"    
  )
)

### |- axis ceiling ----
axis_max <- 0.13  

### |- titles and caption ----
title_text <- "Big Tech Is Hiring, But Not Always Growing"

subtitle_text <- str_glue(
  "Hiring vs. layoffs as % of workforce. Companies ",
  "**<span style='color:{colors$palette$col_expansion}'>above the line</span>** ",
  "are expanding; those ",
  "**<span style='color:{colors$palette$col_contraction}'>below</span>** ",
  "are still shrinking.<br>",
  "Layoffs reflect cumulative reductions over two years."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 15,
  source_text = "trueup.io | Note: Layoff rate = layoffs (2yr) ÷ employees;
  Hiring rate = open jobs ÷ employees"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme   <- create_base_theme()

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major  = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(size = 10, color = "gray30"),
    axis.text         = element_text(size = 9, color = "gray40"),
    axis.ticks        = element_blank(),
    legend.position   = "none",
    plot.margin       = margin(t = 20, r = 24, b = 12, l = 16)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <-
  ggplot(df, aes(x = layoff_rate, y = hiring_rate)) +

  # Geoms
  geom_abline(
    slope = 1, intercept = 0,
    color = colors$palette$col_diagonal,
    linewidth = 0.65,
    linetype = "dashed"
  ) +
  geom_point(
    data = filter(df, is_nvidia),
    aes(x = layoff_rate, y = hiring_rate),
    inherit.aes = FALSE,
    color = colors$palette$col_expansion,
    size = 12, alpha = 0.12
  ) +
  geom_point(
    aes(color = expanding, size = is_nvidia),
    alpha = 0.92,
    show.legend = FALSE
  ) +
  geom_text_repel(
    aes(label = company, color = expanding),
    seed = 123,
    size = 3.2,
    fontface = "bold",
    box.padding = 0.45,
    point.padding = 0.35,
    min.segment.length = 0.2,
    segment.color = "gray78",
    segment.size = 0.28,
    show.legend = FALSE
  ) +

  # Annotate
  annotate(
    "text",
    x = axis_max * 0.74, y = axis_max * 0.68,
    label = "Net growth threshold",
    color = "gray65", size = 2.9, angle = 42, hjust = 0
  ) +
  annotate(
    "text",
    x = 0.002, y = axis_max * 0.96,
    label = "NET EXPANSION",
    color = colors$palette$col_expansion,
    size = 2.5, fontface = "bold", hjust = 0, alpha = 0.6
  ) +
  annotate(
    "richtext",
    x = 0.001, y = 0.070,
    label = "<b style='font-size:12pt'>NVIDIA</b><br>The only major firm with<br>zero layoffs in 2 years",
    size = 2.9,
    color = "#2A8C7A",
    fill = NA,
    label.color = NA,
    label.padding = unit(0.3, "lines"),
    hjust = 0,
    lineheight = 1.25
  ) +
  annotate(
    "richtext",
    x = msft_x + 0.006, y = msft_y + 0.012,
    label = "<b style='font-size:10pt'>Microsoft</b><br>Highest layoff rate<br>despite active hiring",
    size = 2.9,
    color = "#8B2E3A",
    fill = NA,
    label.color = NA,
    label.padding = unit(0.3, "lines"),
    hjust = 0,
    lineheight = 1.25
  ) +

  # Scales
  scale_color_manual(values = c("expanding" = "#2A8C7A", "contracting" = "#8B2E3A")) +
  scale_size_manual(values = c("TRUE" = 8, "FALSE" = 5)) +
  scale_x_continuous(
    name   = "Layoff Rate (% of workforce, cumulative 2yr)",
    labels = label_percent(accuracy = 1),
    limits = c(-0.005, axis_max),
    breaks = c(0, 0.03, 0.06, 0.09, 0.12),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    name   = "Hiring Rate (open jobs as % of workforce)",
    labels = label_percent(accuracy = 1),
    limits = c(0, axis_max),
    breaks = c(0, 0.03, 0.06, 0.09, 0.12),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  coord_cartesian(clip = "off") +

  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +

  # Theme
  theme(
    axis.title.x = element_text(
      angle = 0,
      vjust = 1.04,
      hjust = 0.5,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      angle = 0,
      vjust = 1.04,
      hjust = 0.5,
      margin = margin(r = -150)
    ),
    plot.title = element_text(
      size = rel(1.7),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.7),
      family = fonts$subtitle,
      color = "gray25",
      lineheight = 1.5,
      margin = margin(t = 5, b = 30)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$caption,
      color = "gray50",
      hjust = 0,
      lineheight = 1.3,
      margin = margin(t = 20, b = 5)
    )
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
 
# ─ Session info ─────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-13
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.3.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.3.3)
# R.oo           1.27.0   2024-11-01 [1] CRAN (R 4.3.3)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.3.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# readxl         1.4.5    2025-03-07 [1] CRAN (R 4.3.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.3.1)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────