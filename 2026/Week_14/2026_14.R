## Challenge: #MakeoverMonday 2026 week 14
## Data:      Global Oil Production
## Author:    Steven Ponce
## Date:      2026-04-06

## Article
# https://ourworldindata.org/grapher/oil-production-by-country

## Data
# https://data.world/makeovermonday/2026w14

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
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
df_raw <- read_csv("data/2026/oil-production-by-country.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
df_two <- df_raw |>
  filter(
    entity %in% c("United States", "Saudi Arabia"),
    year >= 1965
  ) |>
  mutate(oil_k = oil / 1000) |>
  select(entity, year, oil_k)

df_wide <- df_two |>
  pivot_wider(names_from = entity, values_from = oil_k) |>
  rename(us = `United States`, saudi = `Saudi Arabia`) |>
  mutate(era = if_else(year < 2009, "rivalry", "gap"))

# Endpoint values for labels
us_2024    <- df_wide |> filter(year == 2024) |> pull(us)
saudi_2024 <- df_wide |> filter(year == 2024) |> pull(saudi)

# Ratio for "nearly 2×" annotation 
ratio_2024 <- round(us_2024 / saudi_2024, 1)

# Label anchor positions
us_1988    <- df_wide |> filter(year == 1988) |> pull(us)
saudi_1988 <- df_wide |> filter(year == 1988) |> pull(saudi)
rival_mid  <- (us_1988 + saudi_1988) / 2

# Gap label
us_2018    <- df_wide |> filter(year == 2018) |> pull(us)
saudi_2018 <- df_wide |> filter(year == 2018) |> pull(saudi)
gap_mid    <- (us_2018 + saudi_2018) / 2


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    us         = "#2E5B8A",
    saudi      = "#722F37",
    fill_gap   = "#C8DAE8",
    fill_rival = "#E8E4DC",
    annot      = "#5A5A5A",
    text       = "#2C2C2A"
  )
)

### |-  titles and caption ----
title_text    <- str_glue("The Rivalry That Became a Gap")

subtitle_text <- str_glue(
  "<span style='color:{colors$palette$us}'>**U.S.**</span> and ",
  "<span style='color:{colors$palette$saudi}'>**Saudi**</span> oil production ",
  "tracked closely for decades. After 2009, U.S. production surged — ",
  "creating a gap that has continued to widen."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 14,
  source_text = "Energy Institute — Statistical Review of World Energy (2025);<br>The Shift Data Portal (2019) via Our World in Data"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme   <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title        = element_blank(),
    axis.text.x       = element_text(size = 9, color = "gray45"),
    axis.text.y       = element_text(size = 9, color = "gray45"),
    axis.line.x       = element_line(color = "gray80", linewidth = 0.3),
    axis.ticks.x      = element_line(color = "gray80", linewidth = 0.3),
    axis.ticks.length = unit(3, "pt"),
    
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    
    plot.margin = margin(t = 20, r = 90, b = 10, l = 20)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(df_wide, aes(x = year)) +
  
  # Geoms
  geom_ribbon(
    data    = df_wide |> filter(era == "rivalry"),
    mapping = aes(ymin = saudi, ymax = us),
    fill    = colors$palette$fill_rival,
    alpha   = 0.9
  ) +
  geom_ribbon(
    data    = df_wide |> filter(era == "gap"),
    mapping = aes(ymin = saudi, ymax = us),
    fill    = colors$palette$fill_gap,
    alpha   = 0.85
  ) +
  annotate(
    "segment",
    x = 2009, xend = 2009,
    y = 0, yend = 10,
    color = "gray70",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = saudi),
    color = colors$palette$saudi,
    linewidth = 1.1,
    lineend = "round"
  ) +
  geom_line(
    aes(y = us),
    color = colors$palette$us,
    linewidth = 1.4,
    lineend = "round"
  ) +
  
  # Annotate
  annotate(
    "text",
    x = 2024.5,
    y = us_2024,
    label = glue("U.S.\n{round(us_2024, 1)}k TWh"),
    hjust = 0,
    size = 3.1,
    fontface = "bold",
    color = colors$palette$us,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = 2024.5,
    y = saudi_2024,
    label = glue("Saudi\n{round(saudi_2024, 1)}k TWh"),
    hjust = 0,
    size = 3.1,
    fontface = "bold",
    color = colors$palette$saudi,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x        = 2024.5,
    y        = us_2024 - 1.2,
    label    = glue("\u007e{ratio_2024}\u00d7 Saudi Arabia"),
    hjust    = 0,
    size     = 2.7,
    color    = "gray50",
    fontface = "italic"
  ) +
  annotate(
    "text",
    x          = 1988,
    y          = rival_mid,
    label      = "Production\ntracked closely",
    hjust      = 0.5,
    size       = 2.8,
    color      = "gray55",
    fontface   = "italic",
    lineheight = 1.3
  ) +
  annotate(
    "text",
    x          = 2017,
    y          = gap_mid + 0.8,
    label      = "U.S. pulls ahead",
    hjust      = 0.5,
    size       = 2.8,
    color      = colors$palette$us,
    fontface   = "italic"
  ) +
  annotate(
    "text",
    x          = 2009,
    y          = 9.7,
    label      = "Shale boom\nbegins",
    hjust      = 0.5,
    size       = 2.7,
    color      = "gray50",
    lineheight = 1.2
  ) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(1970, 2020, by = 10),
    expand = expansion(mult = c(0.01, 0.16))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "k TWh", accuracy = 1),
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10.5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  
  # Theme
  theme(
    plot.title = element_text(
      size = 24, face = "bold", color = colors$palette$text,
      margin = margin(b = 6), family = fonts$title
    ),
    plot.subtitle = element_textbox_simple(
      size = 11, color = "gray35",
      margin = margin(b = 16), family = fonts$subtitle,
      lineheight = 1.4
    ),
    plot.caption = element_textbox_simple(
      size = 8, color = "gray55", family = fonts$caption,
      margin = margin(t = 12),
      lineheight = 1.3
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
 
# ─ Session info ───────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-06
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.3.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.3.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.3.3)
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
# P parallel       4.3.1    2023-06-16 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
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
# vroom          1.7.0    2026-01-27 [1] CRAN (R 4.3.1)
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
# ──────────────────────────────────────────────────────────────────────────
