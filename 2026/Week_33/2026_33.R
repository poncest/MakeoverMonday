## Challenge: #MakeoverMonday 2026 week 33
## Data:      Sports Betting in Gen Z Financial Plans

## Author:    Steven Ponce
## Date:      2026-08-17

## Article
# https://x.com/EricBalchunas/status/2087618932597305457/photo/1

## Data
# https://www.betterment.com/retail-report

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
df_raw <- read_csv(
  "data/2026/gen_z_sports_betting_investing.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
df_plot <- df_raw |>
  filter(is_overall == "No") |>
  mutate(
    metric_short = case_when(
      str_detect(metric, regex("deliberate", ignore_case = TRUE)) ~ "deliberate",
      str_detect(metric, regex("redirected|directed", ignore_case = TRUE)) ~ "redirected",
      TRUE ~ metric
    )
  ) |>
  select(generation, metric_short, percent, respondents) |>
  pivot_wider(names_from = metric_short, values_from = percent) |>
  mutate(
    generation = factor(
      generation,
      levels = c("Boomers", "Gen X", "Millennials", "Gen Z")
    ),
    is_hero = generation == "Gen Z",
    gap = redirected - deliberate
  ) |>
  arrange(generation) |>
  mutate(y_pos = as.numeric(generation))


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    hero      = "#E8720C",
    neutral   = "#ABB2B9",
    text_dark = "#1A1A1A"
  )
)
clrs <- colors$palette

### |- titles and caption ----
title_text <- str_glue("Half of Gen Z Investors Have Redirected Investing Dollars to Sports Betting")

subtitle_text <- str_glue("Redirecting investment funds to sports betting becomes far less common with age")

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 33,
  source_text = "Betterment 2026 Retail Investor Survey<br>Note: n=250 per generation. \"Overall\" omitted because equal generation quotas make it an unweighted average, not a population estimate."
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
### |- legend coordinates ----
legend_y <- max(df_plot$y_pos) + 0.5
legend_open_x <- 2
legend_open_label_x <- 4.5
legend_filled_x <- 25
legend_filled_label_x <- 27.5

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_text(family = fonts$title_1, size = 16, face = "bold"),
    plot.subtitle = element_text(family = fonts$subtitle, size = 11, color = "gray40"),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 6, color = "gray45",
      lineheight = 1.35, margin = margin(t = 10)
    ),
    axis.text.y = element_text(family = fonts$body, size = 10),
    axis.text.x = element_text(family = fonts$body, size = 10, color = "gray50"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 20, b = 10, l = 10)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- ggplot(df_plot) +
  geom_segment(
    aes(
      x = deliberate, xend = redirected, y = y_pos, yend = y_pos,
      color = is_hero, linewidth = is_hero
    ),
    lineend = "round"
  ) +
  geom_point(
    aes(x = deliberate, y = y_pos, color = is_hero),
    shape = 21, fill = "grey98", size = 4.2, stroke = 1.6
  ) +
  geom_point(
    aes(x = redirected, y = y_pos, color = is_hero, fill = is_hero),
    shape = 21, size = 4.2, stroke = 1.2
  ) +
  geom_text(
    aes(x = deliberate, y = y_pos + 0.2, label = paste0(deliberate, "%")),
    family = fonts$caption, size = 2.8, color = "gray35", fontface = "bold"
  ) +
  geom_text(
    aes(x = redirected, y = y_pos + 0.2, label = paste0(redirected, "%")),
    family = fonts$caption, size = 2.8, color = "gray35", fontface = "bold"
  ) +
  annotate(
    "point",
    x = legend_open_x, y = legend_y,
    shape = 21, fill = "grey98", color = "gray40", size = 3, stroke = 1.4
  ) +
  annotate(
    "point",
    x = legend_filled_x + 3, y = legend_y,
    shape = 21, fill = "gray40", color = "gray40", size = 3, stroke = 1.2
  ) +
  geom_text(
    data = tibble(
      x = c(legend_open_label_x, legend_filled_label_x + 3),
      y = c(legend_y, legend_y),
      label = c("Long-term financial strategy", "Redirected investing funds")
    ),
    aes(x = x, y = y, label = label),
    family = fonts$caption, size = 2.85, color = "gray40", hjust = 0,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c(`TRUE` = clrs[["hero"]], `FALSE` = clrs[["neutral"]])) +
  scale_fill_manual(values = c(`TRUE` = clrs[["hero"]], `FALSE` = clrs[["neutral"]])) +
  scale_linewidth_manual(values = c(`TRUE` = 1.7, `FALSE` = 1.2)) +
  scale_x_continuous(limits = c(0, 68), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(
    breaks = df_plot$y_pos, labels = df_plot$generation,
    limits = c(min(df_plot$y_pos) - 0.5, max(df_plot$y_pos) + 0.7)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL, y = NULL
  )

## |- Preview ----
p +
  canvas(width = 8, height = 6, units = "in")

### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_33", "2026_33.png"),
  width = 8, height = 6, dpi = 320
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

# ─ Session info ─────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-17
# rstudio  2026.08.0+187 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
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
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
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
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────