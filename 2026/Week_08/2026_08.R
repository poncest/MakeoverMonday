## Challenge: #MakeoverMonday 2026 week 08
## Data:      Periodic Table of AI
## Author:    Steven Ponce
## Date:      2026-02-23

## Article
# https://www.voronoiapp.com/investing/The-Periodic-Table-of-AI-Startups--14-categories-of-AI-companies-foundedfunded-Feb-2025-Feb-2026-7663

## Data
# https://data.world/makeovermonday/2026wk-8-the-periodic-table-of-ai-startups

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  janitor, lubridate
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 9,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel("data/2026/Periodic Table of AI.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)


## 4. TIDY DATA ----

### |- Enrich ----
df_enriched <- df_raw |>
  mutate(
    group_label = factor(
      group_label,
      levels = c("Foundation Layer", "Horizontal AI", "Vertical AI", "Emerging & Frontier")
    ),
    is_fm = name == "Foundation Models & LLMs",
    funding_per_startup_m = round(funding_billions * 1000 / startup_count, 0)
  )

### |- Key stats for annotations ----
fm_fps <- df_enriched |>
  filter(is_fm) |>
  pull(funding_per_startup_m)

others_fps <- df_enriched |>
  filter(!is_fm) |>
  summarise(avg = round(mean(funding_per_startup_m), 0)) |>
  pull(avg)

multiplier <- round(fm_fps / others_fps, 0)

### |- Plot data ----
plot_data <- df_enriched |>
  group_by(group_label) |>
  arrange(funding_billions, .by_group = TRUE) |>
  ungroup() |>
  mutate(
    name_within = paste0(name, "___", group_label),
    name_within = fct_inorder(name_within),
    highlight = if_else(is_fm,
                        "Foundation Models & LLMs",
                        "All other categories"
    ),
    funding_label = if_else(
      funding_billions >= 3,
      paste0("$", funding_billions, "B"),
      NA_character_
    )
  )

### |- Annotation data ----
ann_line1 <- tibble(
  group_label = factor("Foundation Layer", levels = levels(plot_data$group_label)),
  x = 18, y_num = 3,
  label = glue("~${fm_fps}M per startup")
)
ann_line2 <- tibble(
  group_label = factor("Foundation Layer", levels = levels(plot_data$group_label)),
  x = 18, y_num = 2.58,
  label = glue("vs. ~${others_fps}M avg for all other categories ({multiplier}x more)")
)
ann_line3 <- tibble(
  group_label = factor("Foundation Layer", levels = levels(plot_data$group_label)),
  x = 46, y_num = 1.70,
  label = "Led by OpenAI ($40B in a single round)"
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    highlight  = "#5C2D91",  
    default    = "#9BA8B5",   
    annotation = "#3D1A6E",   
    note       = "#6B7280",  
    grid       = "#EFEFEF",
    text_dark  = "#1A1A2E",
    text_mid   = "#4A4A6A",
    text_light = "#9A9AB0"
  )
)

### |- titles and caption ----
title_text    <- "AI Funding Is Not Competitive \u2014 It\u2019s Concentrated"

subtitle_text <- glue(
  "Total venture funding by AI startup category (Feb 2025\u2013Feb 2026). ",
  "Foundation Models & LLMs raised **$80B**<br>more than all other 13 categories combined."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 8,
  source_text = "Voronoi App<br>**Note:** Funding per startup = total funding / startup count"
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
      size = rel(1.3), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, face = "italic",
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
    # axis.line.x  = element_line(color = "#252525", linewidth = .1),
    # axis.ticks.y = element_blank(),
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
    # panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    # panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
    panel.grid.major.x = element_line(color = colors$palette$grid, linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- Final Plot ----
p <- ggplot(plot_data, aes(x = funding_billions, y = name_within)) +
  # Geoms
  geom_segment(
    aes(x = 0, xend = funding_billions, yend = name_within, color = highlight),
    linewidth = 0.85,
    show.legend = FALSE
  ) +
  geom_point(
    aes(color = highlight),
    size = 4.5,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = funding_label),
    nudge_x = 4.5,
    size = 3.0,
    color = colors$palette$text_mid,
    fontface = "bold",
    family = fonts$text,
    na.rm = TRUE
  ) +
  geom_text(
    data = ann_line1,
    aes(x = x, y = y_num, label = label),
    inherit.aes = FALSE,
    color = colors$palette$annotation,
    size = 3.3,
    fontface = "bold",
    hjust = 0,
    family = fonts$text
  ) +
  geom_text(
    data = ann_line2,
    aes(x = x, y = y_num, label = label),
    inherit.aes = FALSE,
    color = colors$palette$note,
    size = 2.85,
    hjust = 0,
    family = fonts$text
  ) +
  geom_text(
    data = ann_line3,
    aes(x = x, y = y_num, label = label),
    inherit.aes = FALSE,
    color = colors$palette$annotation,
    size = 2.85,
    fontface = "italic",
    hjust = 1,
    family = fonts$text
  ) +
  # Facet
  facet_grid(
    group_label ~ .,
    scales = "free_y",
    space  = "free_y",
    switch = "y"
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Foundation Models & LLMs" = colors$palette$highlight,
      "All other categories"     = colors$palette$default
    )
  ) +
  scale_y_discrete(
    labels = function(x) sub("___.*$", "", x),
    expand = expansion(add = c(0.5, 1.2))
  ) +
  scale_x_continuous(
    labels = label_dollar(suffix = "B", accuracy = 1),
    breaks = c(0, 10, 20, 40, 60, 80),
    expand = expansion(mult = c(0, 0.12))
  ) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    x        = "Total Funding (USD Billions)",
    y        = NULL,
    caption  = caption_text
  ) +
  # Theme
  theme(
    # Facet strips — left side, clean
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle  = 0,
      hjust  = 1,
      face   = "bold",
      size   = 8.5,
      color  = colors$palette$text_mid
    ),
    strip.background = element_blank(),
    
    # Spacing between facets
    panel.spacing.y = unit(12, "pt"),
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
      family = fonts$subtitle,
      face = "italic",
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.5,
      margin = margin(t = 5, b = 25)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    )
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

# ─ Session info ──────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-02-23
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [1] local (on disk 4.4.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.4.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.4.0)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.4.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.4.3)
# compiler       4.4.0    2024-04-24 [1] local
# curl           5.2.1    2024-03-01 [1] CRAN (R 4.4.0)
# datasets     * 4.4.0    2024-04-24 [1] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.4.1)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.4.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.4.1)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.4.3)
# generics       0.1.3    2022-07-05 [1] CRAN (R 4.4.0)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.4.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.4.0)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.4.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.4.3)
# graphics     * 4.4.0    2024-04-24 [1] local
# grDevices    * 4.4.0    2024-04-24 [1] local
# grid           4.4.0    2024-04-24 [1] local
# gridtext       0.1.5    2022-09-16 [1] CRAN (R 4.4.0)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.4.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.4.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.4.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.4.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.4.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.4.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.4.3)
# magick         2.9.0    2025-09-08 [1] CRAN (R 4.4.3)
# magrittr       2.0.4    2025-09-12 [1] CRAN (R 4.4.3)
# markdown       1.12     2023-12-06 [1] CRAN (R 4.4.0)
# methods      * 4.4.0    2024-04-24 [1] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.4.1)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.4.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.4.0)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.4.3)
# R.cache        0.16.0   2022-07-21 [1] CRAN (R 4.4.0)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.4.0)
# R.oo           1.26.0   2024-01-24 [1] CRAN (R 4.4.0)
# R.utils        2.12.3   2023-11-18 [1] CRAN (R 4.4.0)
# R6             2.5.1    2021-08-19 [1] CRAN (R 4.4.0)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.4.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.4.0)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.4.3)
# readr        * 2.1.6    2025-11-14 [1] CRAN (R 4.4.3)
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.4.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.4.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.4.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.4.3)
# rsvg           2.6.0    2023-10-08 [1] CRAN (R 4.4.0)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.4.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.4.3)
# sessioninfo    1.2.2    2021-12-06 [1] CRAN (R 4.4.0)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.4.0)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.4.0)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.4.0)
# stats        * 4.4.0    2024-04-24 [1] local
# stringi        1.8.3    2023-12-11 [1] CRAN (R 4.4.0)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.4.3)
# styler         1.10.3   2024-04-07 [1] CRAN (R 4.4.1)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.4.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.4.0)
# systemfonts    1.3.1    2025-10-01 [1] CRAN (R 4.4.3)
# textshaping    0.3.7    2023-10-09 [1] CRAN (R 4.4.0)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.4.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.4.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.4.0)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.4.0)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.4.3)
# tools          4.4.0    2024-04-24 [1] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.4.3)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.4.0)
# utils        * 4.4.0    2024-04-24 [1] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.4.3)
# withr          3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# xfun           0.47     2024-08-17 [1] CRAN (R 4.4.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.4.3)
# 
# [1] C:/Users/poncest/AppData/Local/Programs/R/R-4.4.1/library
# 
# V ── Loaded and on-disk version mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────
# > 