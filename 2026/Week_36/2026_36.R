## Challenge: #MakeoverMonday 2026 week 36
## Data:      Bears Will Be Boys

## Author:    Steven Ponce
## Date:      2026-09-07

## Article
# https://pudding.cool/2025/07/kids-books/

## Data
# https://pub-cee805df54de4b6c8f93bee984e3c725.r2.dev/datasets/bears-will-be-boys/kids-book-animals.csv

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
  "data/2026/kids-book-animals.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- book-level composition ----
book_composition_raw <- df_raw |>
  summarise(
    n_male = sum(pronoun == "he/him"),
    n_female = sum(pronoun == "she/her"),
    n_it = sum(pronoun == "it"),
    .by = c(goodreads_link, title, pub_year, decade)
  ) |>
  mutate(
    category = case_when(
      n_male > 0 & n_female == 0 & n_it == 0 ~ "Male-only",
      n_female > 0 & n_male == 0 & n_it == 0 ~ "Female-only",
      n_male > 0 & n_female > 0 ~ "Mixed male + female",
      TRUE ~ "Other"
    )
  )

### |- summary counts for the chart ----
book_composition <- book_composition_raw |>
  count(category, name = "n") |>
  mutate(
    pct = n / sum(n),
    category = fct_relevel(category, "Male-only", "Female-only", "Mixed male + female", "Other")
  ) |>
  arrange(category)

male_only_n <- book_composition$n[book_composition$category == "Male-only"]
female_only_n <- book_composition$n[book_composition$category == "Female-only"]
exclusion_ratio <- male_only_n / female_only_n

### |- icon-grid geometry ----
make_icon_grid <- function(n, ncol) {
  tibble(i = 1:n) |> mutate(col = (i - 1) %% ncol, row = (i - 1) %/% ncol)
}

layout_spec <- tribble(
  ~category, ~ncol, ~x_offset, ~y_offset,
  "Male-only", 15, 0, 0,
  "Female-only", 5, 19, 0,
  "Mixed male + female", 20, 0, -12,
  "Other", 4, 23, -12
)

icon_data <- book_composition |>
  left_join(layout_spec, by = "category") |>
  mutate(grid = map2(n, ncol, make_icon_grid)) |>
  unnest(grid) |>
  mutate(x = col + x_offset, y = -row + y_offset)

frame_data <- icon_data |>
  summarise(
    n_rows = max(row) + 1, ncol = first(ncol),
    x_offset = first(x_offset), y_offset = first(y_offset),
    n = first(n), pct = first(pct),
    .by = category
  ) |>
  mutate(
    xmin = x_offset - 0.6, xmax = x_offset + ncol - 0.4,
    ymin = y_offset - (n_rows - 0.6), ymax = y_offset + 0.6
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    male_only   = "#4C6B8A",
    female_only = "#B5532F",
    mixed       = "#8C8C8C",
    other       = "#BFBFBF"
  )
)
clrs <- colors$palette

label_color_map <- c(
  "Male-only"           = clrs[["male_only"]],
  "Female-only"         = clrs[["female_only"]],
  "Mixed male + female" = "grey40",
  "Other"               = "grey45"
)

### |- titles and caption ----
title_text <- str_glue("Nearly half of these children's books leave female animal characters out entirely")

subtitle_text <- str_glue(
  "Of 284 children's books analyzed, {percent(book_composition$pct[book_composition$category=='Male-only'], accuracy = 0.1)} ",
  "contain only he/him animal characters, compared with just ",
  "**{percent(book_composition$pct[book_composition$category=='Female-only'], accuracy = 0.1)}** containing only **she/her characters**."
)

caption_text <- create_social_caption(
  mm_year = 2026, mm_week = 36,
  source_text = "The Pudding, \"Bears Will Be Boys\" (2025)<br>Note: 'Other' = it-pronoun or he/him+it books, no she/her present"
)

annotation_text <- glue("{round(exclusion_ratio, 1)}\u00d7\nas common")

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 24, face = "bold", family = fonts$title_1,
      width = grid::unit(1, "npc"), lineheight = 1.05,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      size = 12.5, family = fonts$subtitle, color = "grey30",
      width = grid::unit(1, "npc"), margin = margin(t = 2, b = 22)
    ),
    plot.caption = element_textbox_simple(
      size = 8, family = fonts$caption, color = "grey45",
      margin = margin(t = 14)
    ),
    axis.text = element_blank(), axis.title = element_blank(),
    axis.ticks = element_blank(), panel.grid = element_blank(),
    plot.margin = margin(24, 24, 16, 24),
    plot.background  = element_rect(fill = clrs[["background"]], color = NA),
    panel.background = element_rect(fill = clrs[["background"]], color = NA)
  )
)

theme_set(weekly_theme)

## |- label positions ----
label_data <- frame_data |>
  mutate(
    label = glue("{category}\n{n} books \u00b7 {percent(pct, accuracy = 0.1)}"),
    label_y = ymax + 1.6
  )

### |- ratio annotation, named scalar coordinates ----
male_frame   <- frame_data |> filter(category == "Male-only")
female_frame <- frame_data |> filter(category == "Female-only")

ratio_annotation_x <- (male_frame$xmax + female_frame$xmin) / 2
ratio_annotation_y <- female_frame$ymax - 2.4

### |- plot ----
p <- ggplot() +
  geom_rect(
    data = frame_data,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = category),
    fill = NA, linewidth = 0.9
  ) +
  geom_point(
    data = icon_data,
    aes(x = x, y = y, color = category),
    size = 2.4
  ) +
  geom_text(
    data = label_data,
    aes(x = x_offset + ncol / 2, y = label_y, label = label, color = category),
    fontface = "bold", size = 3.6, lineheight = 0.95, hjust = 0.5
  ) +
  annotate(
    "text",
    x = ratio_annotation_x, y = ratio_annotation_y,
    label = annotation_text,
    fontface = "bold", size = 5, lineheight = 0.9,
    family = fonts$title_2, color = clrs[["male_only"]],
    hjust = 0.5, vjust = 1
  ) +
  scale_color_manual(values = label_color_map, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |- Preview ----
p +
  canvas(width = 10, height = 8, units = "in")

### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_36", "2026_36.png"),
  width  = 10, height = 8.5,
  bg     = clrs[["background"]] %||% "grey98"
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
# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-07
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────────────────────
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
# ───────────────────────────────────────────────────────────────────────────

