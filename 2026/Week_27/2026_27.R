## Challenge: #MakeoverMonday 2026 week 27
## Data:      Historical Tropical Cyclones
## Author:    Steven Ponce
## Date:      2026-07-06

## Article
# https://www.bbc.com/news/articles/cz913gxlw3jo

## Data
# https://data.world/makeovermonday/2026w27-historical-tropical-cyclones
# https://www.ncei.noaa.gov/products/international-best-track-archive

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, patchwork, binom, purrr
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_csv("data/2026/ibtracs_lite.csv", na = "") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- 4a. Recover blank Atlantic basin field ----
df_raw <- df_raw |>
  mutate(
    basin = case_when(
      !is.na(basin) ~ basin,
      subbasin %in% c("GM", "CS") ~ "NA",
      is.na(subbasin) & lat >= 0 & lat <= 50 &
        lon >= -100 & lon <= 0 ~ "NA",
      TRUE ~ basin
    )
  )

### |- 4b. Collapse to storm level ----
storms <- df_raw |>
  filter(season_year <= 2025) |>
  summarise(
    basin = first(basin[!is.na(basin)]),
    max_sshs = if (all(is.na(usa_sshs))) NA_real_ else max(usa_sshs, na.rm = TRUE),
    max_wind_kts = if (all(is.na(usa_wind_kts))) NA_real_ else max(usa_wind_kts, na.rm = TRUE),
    .by = c(sid, season_year)
  ) |>
  mutate(max_sshs = na_if(max_sshs, -5))

storms_na <- storms |>
  filter(basin == "NA", !is.na(max_sshs)) |>
  mutate(major = as.integer(max_sshs >= 3))

### |- 4c. THE THREE ERAS ---

era_windows <- tribble(
  ~era_label, ~yr_start, ~yr_end, ~era_type,
  "Early record (1910\u20131959)", 1910, 1959, "normal",
  "Late-century lull (1965\u20131989)", 1965, 1989, "trough",
  "Modern record (1995\u20132025)", 1995, 2025, "normal"
)

era_stats <- era_windows |>
  mutate(
    n_total = map2_int(yr_start, yr_end, ~ storms_na |>
      filter(season_year >= .x, season_year <= .y) |>
      nrow()),
    n_major = map2_int(yr_start, yr_end, ~ storms_na |>
      filter(season_year >= .x, season_year <= .y) |>
      pull(major) |>
      sum())
  ) |>
  mutate(
    ci = map2(n_major, n_total, ~ binom.confint(.x, .y, methods = "wilson"))
  ) |>
  unnest(ci) |>
  transmute(era_label, era_type, n_total, n_major,
    pct_major = mean * 100, ci_lower = lower * 100, ci_upper = upper * 100
  )

# Baseline: pooled peak + recent (the two eras confirmed statistically
# indistinguishable, p = 1.0) -- the honest "normal" reference
baseline_data <- storms_na |>
  filter((season_year >= 1910 & season_year <= 1959) |
    (season_year >= 1995 & season_year <= 2025))
baseline <- 100 * mean(baseline_data$major)

### |- 4d. Full decade table ---
decade_stats <- storms_na |>
  mutate(decade = (season_year %/% 10) * 10) |>
  summarise(n_total = n(), n_major = sum(major), .by = decade) |>
  arrange(decade) |>
  mutate(
    ci = map2(n_major, n_total, ~ binom::binom.confint(.x, .y, methods = "wilson"))
  ) |>
  unnest(ci) |>
  select(decade, n_total, n_major, pct_major = mean, ci_lower = lower, ci_upper = upper) |>
  mutate(
    pct_major = pct_major * 100, ci_lower = ci_lower * 100, ci_upper = ci_upper * 100,
    decade_label = paste0(decade, "s"),
    in_trough = decade %in% c(1970, 1980)
  )


## 5. VISUALIZATION ----

### |- 5a. plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    trough   = "#B5532F",
    normal   = "#5378A6",
    context  = "#DEDED8",
    baseline = "#8C8C8C"
  )
)
if (is.null(colors$trough)) colors$trough <- "#B5532F"
if (is.null(colors$normal)) colors$normal <- "#5378A6"
if (is.null(colors$context)) colors$context <- "#DEDED8"
if (is.null(colors$baseline)) colors$baseline <- "#8C8C8C"

### |- 5b. titles and caption ----
title_text <- "Atlantic Hurricanes: The Late-Century Lull"

subtitle_text <- str_glue(
  "For much of the historical record, about one in five Atlantic storms became a major ",
  "hurricane.<br>The exception was a prolonged lull from 1965 to 1989, when the rate ",
  "fell to about one in ten."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 27,
  source_text = paste0(
    "US National Hurricane Center via IBTrACS (NOAA NCEI)<br>",
    "Top: comparison of the three periods tested in the analysis. Bottom: decade ",
    "estimates shown for historical context. Error bars represent 95% confidence intervals."
  )
)

### |- 5c. fonts ----
setup_fonts()
fonts <- get_font_families()

### |- 5d. plot theme ----
base_theme <- create_base_theme(colors)

headline_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_markdown(size = rel(1.3), face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_markdown(size = rel(0.9), color = "gray30", margin = margin(b = 14)),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = rel(1.05), face = "bold"),
    legend.position = "none"
  )
)

context_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.25),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = rel(0.68), color = "gray45"),
    axis.title.x = element_text(size = rel(0.8), color = "gray40"),
    legend.position = "none"
  )
)

### |- 5e. headline panel ----
p_headline <- era_stats |>
  ggplot(aes(x = pct_major, y = era_label)) +
  geom_vline(
    xintercept = baseline, color = colors$baseline,
    linewidth = 0.6, linetype = "dashed"
  ) +
  annotate(
    "text",
    x = baseline + 1.3, y = "Late-century lull (1965\u20131989)",
    label = paste0("Reference rate\n\u2248", round(baseline), "%"),
    size = 2.9, color = "gray40", family = fonts$text,
    hjust = 0, vjust = 0.5, lineheight = 0.9
  ) +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper, color = era_type),
    orientation = "y", width = 0.15, linewidth = 0.9
  ) +
  geom_point(aes(color = era_type), size = 5.5) +
  geom_text(
    data = ~ filter(.x, era_type == "trough"),
    aes(label = paste0(round(pct_major), "%")),
    nudge_y = 0.22, size = 3.6, fontface = "bold", family = fonts$text,
    color = "gray20"
  ) +
  scale_color_manual(values = c(normal = colors$normal, trough = colors$trough)) +
  scale_y_discrete(limits = c(
    "Modern record (1995\u20132025)",
    "Late-century lull (1965\u20131989)",
    "Early record (1910\u20131959)"
  )) +
  scale_x_continuous(
    labels = \(x) paste0(x, "%"), limits = c(0, 30),
    breaks = pretty_breaks(n = 6)
  ) +
  labs(x = NULL, y = NULL) +
  headline_theme

### |- 5f. context panel: full decade record, muted except trough decades ----
p_context <- decade_stats |>
  mutate(decade_label = fct_inorder(decade_label)) |>
  ggplot(aes(x = pct_major, y = decade_label)) +
  geom_vline(
    xintercept = baseline, color = colors$baseline,
    linewidth = 0.4, linetype = "dashed"
  ) +
  geom_errorbar(
    aes(xmin = ci_lower, xmax = ci_upper, color = in_trough),
    orientation = "y", width = 0.12, linewidth = 0.35, alpha = 0.5
  ) +
  geom_point(aes(color = in_trough, size = in_trough)) +
  scale_color_manual(values = c(`TRUE` = colors$trough, `FALSE` = colors$context)) +
  scale_size_manual(values = c(`TRUE` = 1.8, `FALSE` = 1.5), guide = "none") +
  scale_x_continuous(
    labels = \(x) paste0(x, "%"), limits = c(0, 30),
    breaks = pretty_breaks(n = 6)
  ) +
  labs(x = "Share of Atlantic storms reaching Category 3+ (with 95% CI)", y = NULL) +
  context_theme


### |- 5g. combine ----
p_combined <- p_headline / p_context +
  plot_layout(heights = c(1, 2.5)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(size = rel(1.5), face = "bold", margin = margin(b = 6), family = "title_1"),
      plot.subtitle = element_markdown(size = rel(1.0), color = "gray30", margin = margin(b = 14)),
      plot.caption = element_markdown(size = rel(0.75), color = "gray40", hjust = 0)
    )
  )

### |-  Preview ----
snap(p_combined)


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

# ─ Session info ─────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-07-06
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.5.3)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# cachem         1.1.0    2024-05-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# conflicted     1.2.0    2023-02-01 [1] CRAN (R 4.5.3)
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
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
# memoise        2.0.1    2021-11-26 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
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
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
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
# ────────────────────────────