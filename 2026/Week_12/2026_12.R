## Challenge: #MakeoverMonday 2026 week 12
## Data:      London Unemployment Estimates
## Author:    Steven Ponce
## Date:      2026-03-23

## Article
# https://data.london.gov.uk/blog/unemployment-in-key-charts-young-londoners-hit-hardest-by-labour-market-slowdown/

## Data
# https://data.world/makeovermonday/2026w12-uk-unemployment-estimates

## Original Source
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/regionalunemploymentbyagex02

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## NOTE: Unemployment counts are estimated numbers of unemployed people from the
##       ONS Quarterly Labour Force Survey. Series shown are 4-period rolling
##       averages. Absolute values are counts (thousands), not labor-force rates.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, readxl, zoo 
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
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_xlsx("data/2026/London Unemployment Estimates.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
### |- filter to counts only ----
df_counts <- df_raw |>
  filter(unit == "Estimated Number of People")

### |- focal series: youth (16-24) and overall (16+) ----
df_main <- df_counts |>
  mutate(date = as.Date(period_midpoint_date)) |>
  filter(date >= as.Date("2019-01-01")) |>
  mutate(
    youth_k   = x16_24 / 1000,
    overall_k = all_aged_16_over / 1000
  ) |>
  arrange(date) |>
  select(date, period, youth_k, overall_k)

### |- 4-period rolling average ----
df_main <- df_main |>
  mutate(
    youth_smooth   = rollmean(youth_k,   k = 4, fill = NA, align = "right"),
    overall_smooth = rollmean(overall_k, k = 4, fill = NA, align = "right")
  ) |>
  filter(!is.na(youth_smooth), !is.na(overall_smooth)) |>
  mutate(
    gap_k      = overall_smooth - youth_smooth,
    gap_mid_k  = youth_smooth + gap_k / 2
  )

### |- latest values for labels ----
latest <- df_main |>
  slice_max(date, n = 1)

latest_youth   <- round(latest$youth_smooth, 1)
latest_overall <- round(latest$overall_smooth, 1)
latest_gap     <- round(latest$gap_k, 1)

### |- x positions for labels / callouts ----
x_label_end <- latest$date + 35
x_gap_label <- latest$date - 60

### |- y positions for stacked end labels ----
y_overall_lab_top <- latest$overall_smooth + 7
y_overall_lab_val <- latest$overall_smooth - 3

y_youth_lab_top   <- latest$youth_smooth + 7
y_youth_lab_val   <- latest$youth_smooth - 3

### |- policy event windows ----
announce_start  <- as.Date("2024-10-15")
announce_end    <- as.Date("2024-11-15")
implement_start <- as.Date("2025-03-15")
implement_end   <- as.Date("2025-05-15")


## 5. VISUALIZATION ----

### |- Colors ----
colors <- get_theme_colors(
  palette = list(
    youth   = "#7B1E3A",
    overall = "#2C3E50",
    gap     = "#F8E6E3",
    zone    = "#D5DBDB",
    gap_txt = "#A93226"
  )
)

### |- titles and caption ----
title_text <- "Youth unemployment in London is rising faster than the overall trend"

subtitle_text <- paste0(
  "Since late 2024, unemployment among 16–24 year-olds has risen faster than the overall 16+ total, widening the gap.<br>",
  "<span style='font-size:9pt;'>4-quarter moving average of estimated unemployed counts (thousands)</span>"
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 12,
  source_text = "GLA Economics, ONS Quarterly Labour Force Survey<br>
**Note:** Counts shown are estimated unemployed people, not unemployment rates.<br>
Policy zones mark the Oct 2024 Budget announcement and Apr 2025 NIC & minimum wage implementation."
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme   <- create_base_theme(colors)
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title         = element_blank(),
    axis.text.x        = element_text(size = 9, color = "gray40"),
    axis.text.y        = element_text(size = 9, color = "gray40"),
    axis.line.x        = element_line(color = "gray82", linewidth = 0.4),
    
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    legend.position    = "none",
    
    plot.margin = margin(t = 16, r = 55, b = 12, l = 16),
    
    plot.title = element_text(
      size        = rel(1.3),
      family      = fonts$title,
      face        = "bold",
      color       = colors$title,
      lineheight  = 1.05,
      hjust       = 0,
      margin      = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size        = rel(0.75),
      family      = fonts$subtitle,
      face        = "italic",
      color       = alpha(colors$subtitle, 0.92),
      lineheight  = 1.08,
      margin      = margin(t = 0, b = 20)
    ),
    plot.caption = element_markdown(
      size        = rel(0.5),
      family      = fonts$subtitle,
      color       = colors$caption,
      hjust       = 0,
      lineheight  = 1.35,
      margin      = margin(t = 20, b = 5)
    )
  )
)

theme_set(weekly_theme)

### |- build plot ----
p <- ggplot(df_main, aes(x = date)) +
  
  # Annotate
  annotate(
    "rect",
    xmin  = announce_start, xmax = announce_end,
    ymin  = -Inf, ymax = Inf,
    fill  = colors$palette$zone,
    alpha = 0.28
  ) +
  annotate(
    "rect",
    xmin  = implement_start, xmax = implement_end,
    ymin  = -Inf, ymax = Inf,
    fill  = colors$palette$zone,
    alpha = 0.28
  ) +
  # Geoms
  geom_ribbon(
    aes(
      ymin = pmin(youth_smooth, overall_smooth),
      ymax = pmax(youth_smooth, overall_smooth)
    ),
    fill  = colors$palette$gap,
    alpha = 0.45
  ) +
  geom_line(
    aes(y = overall_smooth),
    color     = colors$palette$overall,
    linewidth = 0.95,
    lineend   = "round"
  ) +
  geom_line(
    aes(y = youth_smooth),
    color     = colors$palette$youth,
    linewidth = 1.25,
    lineend   = "round"
  ) +
  geom_point(
    data  = latest,
    aes(y = overall_smooth),
    color = colors$palette$overall,
    size  = 3.6
  ) +
  geom_point(
    data  = latest,
    aes(y = youth_smooth),
    color = colors$palette$youth,
    size  = 3.6
  ) +
  
  # Annotate
  annotate(
    "text",
    x        = x_label_end,
    y        = y_overall_lab_top,
    label    = "16+ overall",
    color    = colors$palette$overall,
    hjust    = 0,
    vjust    = 0.5,
    size     = 2.8,
    family   = fonts$text
  ) +
  annotate(
    "text",
    x        = x_label_end,
    y        = y_overall_lab_val,
    label    = glue("{latest_overall}k"),
    color    = colors$palette$overall,
    hjust    = 0,
    vjust    = 0.5,
    size     = 3.8,
    fontface = "bold",
    family   = fonts$text
  ) +
  annotate(
    "text",
    x        = x_label_end,
    y        = y_youth_lab_top,
    label    = "16–24 youth",
    color    = colors$palette$youth,
    hjust    = 0,
    vjust    = 0.5,
    size     = 2.8,
    family   = fonts$text
  ) +
  annotate(
    "text",
    x        = x_label_end,
    y        = y_youth_lab_val,
    label    = glue("{latest_youth}k"),
    color    = colors$palette$youth,
    hjust    = 0,
    vjust    = 0.5,
    size     = 3.8,
    fontface = "bold",
    family   = fonts$text
  ) +
  annotate(
    "text",
    x      = announce_start,
    y      = Inf,
    label  = "Budget\n(Oct 2024)",
    color  = "gray48",
    hjust  = 0.5,
    vjust  = 1.35,
    size   = 2.7,
    family = fonts$text
  ) +
  annotate(
    "text",
    x      = implement_start,
    y      = Inf,
    label  = "Implementation\n(Apr 2025)",
    color  = "gray48",
    hjust  = 0.5,
    vjust  = 3.7,
    size   = 2.7,
    family = fonts$text
  ) +
  annotate(
    "curve",
    x     = x_gap_label + 12,
    xend  = latest$date - 5,
    y     = latest$gap_mid_k + 10,
    yend  = latest$gap_mid_k + 2,
    curvature = -0.2,
    linewidth = 0.35,
    color = alpha(colors$palette$gap_txt, 0.8)
  ) +
  annotate(
    "text",
    x      = x_gap_label,
    y      = latest$gap_mid_k + 12,
    label  = glue("+{latest_gap}k gap"),
    color  = colors$palette$gap_txt,
    hjust  = 0,
    vjust  = 0.5,
    size   = 3.1,
    fontface = "bold",
    family = fonts$text
  ) +
  annotate(
    "text",
    x      = as.Date("2025-08-01"),
    y      = latest$gap_mid_k - 12,
    label  = "Largest gap in the recent period",
    color  = alpha(colors$palette$gap_txt, 0.9),
    hjust  = 0,
    vjust  = 0.5,
    size   = 2.7,
    family = fonts$text
  ) +
  
  # Scales
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "Q4 %Y",
    expand      = expansion(mult = c(0.01, 0.13))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "k", accuracy = 1),
    breaks = c(100, 200, 300, 400),
    expand = expansion(mult = c(0.06, 0.12))
  ) +
  
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  )

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
 
# ─ Session info ────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-23
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmp0epg1O/file6448518841ff". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
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
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# haven          2.5.5    2025-05-30 [1] CRAN (R 4.3.1)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# lattice        0.21-8   2023-04-05 [2] CRAN (R 4.3.1)
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
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.3.3)
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
# zoo          * 1.8-15   2025-12-15 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────