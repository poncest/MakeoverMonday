## Challenge: #MakeoverMonday 2026 week 10
## Data:      Mario Game Sales
## Author:    Steven Ponce
## Date:      2026-03-09

## Article
# https://vgsales.fandom.com/wiki/Mario#cite_note-vc-27

## Data
# https://data.world/makeovermonday/2026w10-mario-game-sales

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, readxl      
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_xlsx("data/2026/MM 2026 wk10.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- "All Consoles" aggregate rows only ----
df_all <- df_raw |>
  filter(console == "All Consoles") |>
  filter(!is.na(sales_units), !is.na(gross_est)) |>
  filter(!str_detect(str_to_lower(title), "^total")) |>
  mutate(
    gross_rank = rank(-gross_est, ties.method = "first"),
    units_rank = rank(-sales_units, ties.method = "first"),
    rank_delta = units_rank - gross_rank # positive = unit over-performer
  )

### |- Top 20 by gross rank ----
plot_data <- df_all |>
  slice_min(gross_rank, n = 20) |>
  mutate(
    title = fct_reorder(title, gross_rank, .desc = TRUE),
    seg_type = case_when(
      rank_delta >= 4 ~ "unit_over",
      rank_delta <= -4 ~ "gross_over",
      TRUE ~ "neutral"
    )
  )

### |- Annotation x-anchor ----
annot_x_fixed <- 23

annot_data <- plot_data |>
  filter(title %in% c(
    "Super Mario World 2: Yoshi's Island",
    "Super Mario All-Stars",
    "Super Mario Galaxy 2"
  )) |>
  mutate(
    annot_label = case_when(
      str_detect(title, "Yoshi") ~
        "SNES budget re-release\nboosted unit count\nwithout matching revenue",
      str_detect(title, "All-Stars") ~
        "Compilation sold widely\nbut at lower per-title\nrevenue",
      str_detect(title, "Galaxy 2") ~
        "Strong unit seller\nbut priced at a\npremium — revenue\nclosely matches"
    ),
    seg_xend = annot_x_fixed - 0.2
  )

### |- Highlight strip rows ----
highlight_rows <- plot_data |>
  filter(seg_type == "unit_over") |>
  pull(title)


## 5. VISUALIZATION ----

### |- Colors ----
colors <- get_theme_colors(
  palette = list(
    burgundy      = "#6D1A36",
    steel         = "#4A6FA5",
    neutral_dark  = "#2B2B2B",
    neutral_mid   = "#6B6B6B",
    neutral_light = "#E5E5E5",
    segment_neut  = "#C8C8C8",
    highlight_bg  = "#F5EEF1",
    background    = "#FAFAF9"
  )
)

### |- Titles and caption ----
title_text <- "Older Mario Titles Sold More Copies Than Their Revenue Rank Suggests"

subtitle_text <- glue(
  "For each title, the <b style='color:{colors$palette$steel}'>● revenue rank</b> and ",
  "<b style='color:{colors$palette$burgundy}'>● units rank</b> are shown — longer gaps mean ",
  "the two metrics disagree more.<br>",
  "Re-releases and budget pricing explain why some classics rank much higher in copies than in dollars."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 10,
  source_text = "Fandom — Video Game Sales Wiki"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.ticks         = element_blank(),
    # axis.text.y handled per-panel (p_left uses selective bold; p_right hides it)
    axis.text.x        = element_text(size = 9,   color = colors$palette$gray_mid),
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10), family = fonts$subtitle,
      color = "gray40"
    ),
    plot.title = element_text(
      size = rel(1.3), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 3)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 8)
    ),
  )
)

theme_set(weekly_theme)


### |- Plot ----
p <- plot_data |>
  ggplot(aes(y = title)) +
  # Geoms 
  geom_rect(
    data = plot_data |> filter(seg_type == "unit_over") |>
      mutate(ynum = as.integer(title)),
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = ynum - 0.45, ymax = ynum + 0.45
    ),
    fill = colors$palette$highlight_bg,
    inherit.aes = FALSE
  ) +
  geom_segment(
    aes(
      x         = gross_rank,
      xend      = units_rank,
      yend      = title,
      color     = seg_type,
      linewidth = seg_type
    ),
    lineend = "round"
  ) +
  geom_point(
    aes(x = gross_rank),
    color = colors$palette$steel,
    size  = 3.4, shape = 16
  ) +
  geom_point(
    aes(x = units_rank),
    color = colors$palette$burgundy,
    size  = 3.4, shape = 16
  ) +
  geom_segment(
    data = annot_data,
    aes(
      x    = units_rank + 0.2,
      xend = seg_xend,
      y    = title,
      yend = title
    ),
    color       = colors$palette$neutral_mid,
    linewidth   = 0.35,
    linetype    = "dotted",
    inherit.aes = FALSE
  ) +
  geom_text(
    data   = annot_data,
    aes(x = annot_x_fixed, y = title, label = annot_label),
    hjust      = 0,
    vjust      = 0.5,
    size       = 2.75,
    family     = fonts$text,
    color      = colors$palette$neutral_mid,
    lineheight = 1.3,
    inherit.aes = FALSE
  ) +
  # Scales 
  scale_color_manual(
    values = c(
      "unit_over"  = colors$palette$burgundy,
      "gross_over" = colors$palette$steel,
      "neutral"    = colors$palette$segment_neut
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "unit_over"  = 1.5,
      "gross_over" = 1.5,
      "neutral"    = 0.75
    )
  ) +
  scale_x_continuous(
    breaks = seq(2, 20, by = 2),
    # Extra right expansion to accommodate annotation text
    expand = expansion(mult = c(0.04, 0.38))
  ) +
  
  scale_y_discrete(
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = "Rank  (1 = best)",
    y        = NULL
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = rel(1.6),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.85),
      family = 'sans',                     
      face = "italic",
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
    plot.margin = margin(15, 15, 10, 15)
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
# date     2026-03-09
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpugSVKD/file8060490a6d42". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ─────────────────────────────────────────────────────────────────
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
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.3.3)
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
# pillar         1.10.2   2025-04-05 [1] CRAN (R 4.3.3)
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
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.3)
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
