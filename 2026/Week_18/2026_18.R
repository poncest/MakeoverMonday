## Challenge: #MakeoverMonday 2026 week 18
## Data:      Eater San Diego
## Author:    Steven Ponce
## Date:      2026-05-02

## Article
# https://sandiego.eater.com/maps/san-diego-best-local-tacos
# https://gemini.google.com/share/672e1babcb27

## Data
# https://data.world/makeovermonday/2026w18-best-tacos-in-san-diego/

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
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel("data/2026/top_tacos_in_san_diego_tc26.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

field_mean <- mean(df_raw$score)

# Score band boundaries (from data distribution)
elite_cut <- 4.6 # ~75th percentile
trailing_cut <- 4.4 # ~25th percentile

df <- df_raw |>
  mutate(
    score_vs_mean = score - field_mean,
    tier = case_when(
      score >= elite_cut ~ "elite",
      score >= trailing_cut ~ "competitive",
      TRUE ~ "trailing"
    ),
    tier = factor(tier, levels = c("elite", "competitive", "trailing")),
    rank = rank(-score, ties.method = "first"),

    # Label: top 5 + 2 notable below-mean spots for contrast
    label = case_when(
      rank <= 5 ~ restaurant_name,
      restaurant_name == "Cafe Coyote" ~ restaurant_name, # lowest rated
      restaurant_name == "El Chingon" ~ restaurant_name, # 2nd lowest
      TRUE ~ NA_character_
    )
  ) |>
  arrange(score_vs_mean)

# Tier n counts for zone annotations
n_elite <- sum(df$tier == "elite")
n_competitive <- sum(df$tier == "competitive")
n_trailing <- sum(df$tier == "trailing")

# X axis bounds
x_min <- floor(min(df$score_vs_mean) * 10) / 10 - 0.02
x_max <- max(df$score_vs_mean) + 0.06



## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    elite       = "#8B2331",   
    competitive = "#4A7BA7",   
    trailing    = "#BBBBBB",   
    mean_line   = "#444444",   
    band_elite  = "#FDF5F5",   
    band_trail  = "#F8F8F8",   
    zone_text   = "#888888"    
  )
)

### |-  titles and caption ----
title_text <- "San Diego taco ratings barely separate the best from the rest"

subtitle_text <- glue(
  "Scores cluster tightly around the average ({round(field_mean, 2)}), ",
  "with most restaurants within ±0.2 points. ",
  "Just **<span style='color:{colors$palette$elite}'>{n_elite} of 50 spots</span>** ",
  "reach 4.6 or higher."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 18,
  source_text = "Eater San Diego | data.world/makeovermonday"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.title.x = element_text(
      size = 8.5, color = "gray45",
      margin = margin(t = 8), family = fonts$text
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 7.5, color = "gray50"),
    axis.text.y = element_text(size = 7, color = "gray35", hjust = 1),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 35, b = 20, l = 10)
  )
)

theme_set(weekly_theme)

### |-  zone thresholds on relative scale ----
elite_rel    <- elite_cut    - field_mean
trailing_rel <- trailing_cut - field_mean


### |-  Main plot ----
p <- df |>
  ggplot(aes(x = score_vs_mean, y = reorder(restaurant_name, score_vs_mean))) +

  # Rect
  annotate("rect",
    xmin = elite_rel, xmax = x_max,
    ymin = -Inf, ymax = Inf,
    fill = colors$palette$band_elite, alpha = 0.07
  ) +
  annotate("rect",
    xmin = x_min, xmax = trailing_rel,
    ymin = -Inf, ymax = Inf,
    fill = colors$palette$band_trail, alpha = 0.07
  ) +

  # Geoms
  geom_vline(
    xintercept = 0,
    color      = colors$palette$mean_line,
    linewidth  = 0.55,
    linetype   = "dashed"
  ) +
  geom_segment(
    aes(
      x = 0, xend = score_vs_mean,
      y = reorder(restaurant_name, score_vs_mean),
      yend = reorder(restaurant_name, score_vs_mean),
      color = tier
    ),
    linewidth = 0.5,
    alpha = 0.55
  ) +
  geom_point(
    aes(color = tier, size = tier),
    alpha = 0.88
  ) +
  geom_text_repel(
    aes(label = label),
    size = 2.55,
    color = "gray20",
    family = fonts$text,
    hjust = 0,
    direction = "y",
    nudge_x = 0.01,
    segment.color = "gray75",
    segment.size = 0.25,
    max.overlaps = 15,
    na.rm = TRUE
  ) +

  # Annotate
  annotate("text",
    x = 0.13,
    y = 40.5,
    label = glue("Elite\n≥{elite_cut} · n = {n_elite}"),
    hjust = 0,
    size = 2.5,
    color = colors$palette$elite,
    family = fonts$text,
    fontface = "bold",
    lineheight = 1.25
  ) +
  annotate("text",
    x = 0.03,
    y = n_trailing + (n_competitive * 0.5),
    label = glue("Competitive\n{trailing_cut}–{elite_cut - 0.01} · n = {n_competitive}"),
    hjust = 0,
    size = 2.5,
    color = colors$palette$zone_text,
    family = fonts$text,
    fontface = "bold",
    lineheight = 1.25
  ) +
  annotate("text",
    x = -0.41,
    y = 9.5,
    label = glue("Trailing\n<{trailing_cut} · n = {n_trailing}"),
    hjust = 0,
    vjust = 0,
    size = 2.5,
    color = colors$palette$zone_text,
    family = fonts$text,
    fontface = "bold",
    lineheight = 1.25
  ) +
  annotate("text",
    x = 0.06,
    y = 27,
    label = "Most ratings fall\nwithin a 0.2-point band",
    hjust = 0,
    size = 2.6,
    color = "gray50",
    family = fonts$text,
    fontface = "italic",
    lineheight = 1.3
  ) +
  annotate("text",
    x = -0.41,
    y = 7.8,
    label = "Even the lowest-rated spots\nare within ~0.4 points of average",
    hjust = 0,
    vjust = 0,
    size = 2.3,
    color = "gray60",
    family = fonts$text,
    fontface = "italic",
    lineheight = 1.3
  ) +
  annotate("text",
    x = -0.05,
    y = 49.2,
    label = glue("Average\n({round(field_mean, 2)})"),
    hjust = 0,
    size = 2.2,
    color = colors$palette$mean_line,
    family = fonts$text,
    lineheight = 1.2
  ) +

  # Scales
  scale_color_manual(
    values = c(
      "elite" = colors$palette$elite,
      "competitive" = colors$palette$competitive,
      "trailing"= colors$palette$trailing
    ),
    guide = "none"
  ) +
  scale_size_manual(
    values = c(
      "elite" = 2.8,
      "competitive" = 2.2,
      "trailing"= 1.9
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%+.2f", x),
    breaks = seq(-0.4, 0.4, by = 0.1),
    expand = expansion(mult = c(0.02, 0.12))
  ) +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = glue("Difference from average ({round(field_mean, 2)})")
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = 26, face = "bold", color = "gray10",
      family = fonts$title, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 11, color = "gray35", family = fonts$subtitle,
      lineheight = 1.4, margin = margin(b = 18)
    ),
    plot.caption = element_markdown(
      size = 9, color = "gray55", family = fonts$captio,
      hjust = 0, margin = margin(t = 15)
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
 
# ─ Session info ──────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-04
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
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
# ggrepel      * 0.9.8    2026-03-17 [1] CRAN (R 4.5.3)
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
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R.cache        0.17.0   2025-05-02 [1] CRAN (R 4.5.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.5.2)
# R.oo           1.27.1   2025-05-02 [1] CRAN (R 4.5.2)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.5.3)
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
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.5.3)
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
# ───────────────────────────────────────────────────────────────