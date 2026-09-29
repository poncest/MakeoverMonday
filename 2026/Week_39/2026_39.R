## Challenge: #MakeoverMonday 2026 week 39
## Data:      World Population

## Author:    Steven Ponce
## Date:      2026-09-29

## Article
# https://data.worldbank.org/indicator/SP.POP.TOTL

## Data
# https://data.worldbank.org/indicator/SP.POP.TOTL

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview, readxl, countrycode
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 13
fig_h <- 4.0 

## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel(
  "data/2026/World-Population.xls") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- long format ----
df_long <- df_raw |>
  pivot_longer(
    starts_with("x"),
    names_to = "year", names_prefix = "x",
    names_transform = as.integer, values_to = "pop"
  )

### |- 5-year windows: 13 complete windows tile 1961-2025 exactly ----
win_starts <- seq(1961, 2021, by = 5)
win_label <- function(start) as.character(glue("{start}–{str_sub(start + 4, 3, 4)}"))

annual <- df_long |>
  filter(country_code %in% c("WLD", "SSF", "EAS")) |>
  select(code = country_code, year, pop) |>
  arrange(code, year) |>
  mutate(add = pop - lag(pop), .by = code) |>
  filter(year >= 1961) |>
  mutate(win_start = 1961 + (year - 1961) %/% 5 * 5)

### |- window means: SSF / EAS / Rest (= WLD - SSF - EAS, exact) ----
three <- annual |>
  summarise(add_m = mean(add) / 1e6, .by = c(code, win_start)) |>
  pivot_wider(names_from = code, values_from = add_m) |>
  mutate(Rest = WLD - SSF - EAS) |>
  arrange(win_start)

### |- copy guards: every number quoted in title/subtitle/annotations ----
plateau <- three |> filter(between(win_start, 1981, 2016))
w <- \(start, col) three[[col]][three$win_start == start]

crossover_start <- three |>
  filter(SSF > EAS) |>
  slice_min(win_start, n = 1) |>
  pull(win_start)

### |- growth rate by window ----
wld_rate <- df_long |>
  filter(country_code == "WLD") |>
  arrange(year) |>
  mutate(rate = pop / lag(pop) - 1) |>
  filter(year >= 1961) |>
  mutate(win_start = 1961 + (year - 1961) %/% 5 * 5) |>
  summarise(rate = mean(rate), .by = win_start)

rate_81 <- wld_rate$rate[wld_rate$win_start == 1981]
rate_16 <- wld_rate$rate[wld_rate$win_start == 2016]

### |- squares: 1 square = 1M net growth / yr, largest-remainder rounding ----
alloc_units <- function(x) {
  base <- floor(x)
  rem <- round(sum(x)) - sum(base)
  base + (rank(-(x - base), ties.method = "first") <= rem)
}

n_cols <- 9 # grid width in squares
gap_in <- 2 # gap between windows within an era
gap_era <- 7 # gap between eras (Ma: the void marks the boundary)

win_meta <- tibble(win_start = win_starts) |>
  mutate(
    k = row_number(),
    era = case_when(
      win_start < 1981 ~ "before",
      win_start < 2021 ~ "plateau",
      .default = "break"
    ),
    era_idx = match(era, c("before", "plateau", "break")),
    x0 = (k - 1) * (n_cols + gap_in) + (era_idx - 1) * (gap_era - gap_in),
    window = win_label(win_start)
  )

squares <- three |>
  pivot_longer(c(SSF, EAS, Rest), names_to = "group", values_to = "add_m") |>
  mutate(group = factor(group, levels = c("SSF", "EAS", "Rest"))) |> # fixed fill order
  arrange(win_start, group) |>
  mutate(n_sq = alloc_units(add_m), .by = win_start)

# Row-wise from bottom-left (column-wise fill would rebuild stacked columns)
tiles <- squares |>
  uncount(n_sq) |>
  mutate(
    i = row_number() - 1,
    col = i %% n_cols,
    row = i %/% n_cols,
    .by = win_start
  ) |>
  left_join(win_meta |> select(win_start, x0), by = "win_start") |>
  mutate(x = x0 + col, y = row)

max_row <- max(tiles$row)

### |- labels ----
window_labels <- win_meta |>
  mutate(x = x0 + (n_cols - 1) / 2, y = -1.3)

era_labels <- win_meta |>
  slice_min(k, n = 1, by = era) |>
  mutate(
    x = x0 - 0.5,
    y = max_row + 1.6,
    label = case_match(
      era,
      "before" ~ "BEFORE\n1961–80",
      "plateau" ~ "THE PLATEAU\n1981–2020",
      "break" ~ "POSSIBLE BREAK\n2021–25"
    ),
    hjust = if_else(era == "break", 1, 0),
    x = if_else(era == "break", x0 + n_cols - 0.5, x)
  )

x0_cross <- win_meta$x0[win_meta$win_start == crossover_start]
x0_break <- win_meta$x0[win_meta$win_start == 2021]

annotations <- tibble(
  x = c(x0_cross + (n_cols - 1) / 2, x0_break + n_cols - 0.5),
  y = c(-2.9, -2.9),
  hjust = c(0.5, 1),
  label = c(
    "Sub-Saharan Africa overtakes\nEast Asia & Pacific",
    glue(
      "World: {round(w(2021, 'WLD'))}M a year\n",
      "East Asia & Pacific: {round(w(2021, 'EAS'))}M"
    )
  )
)

### |- layer for the caption: largest SSF contributors, 2021-25 ----
ssf_codes <- countrycode::codelist |>
  filter(region == "Sub-Saharan Africa") |>
  pull(iso3c)

top_ssf <- df_long |>
  filter(country_code %in% ssf_codes, year %in% 2020:2025) |>
  arrange(country_code, year) |>
  mutate(add = pop - lag(pop), .by = country_code) |>
  filter(year >= 2021) |>
  summarise(add_m = mean(add) / 1e6, .by = country_name) |>
  slice_max(add_m, n = 3, with_ties = FALSE) |>
  mutate(country_name = str_replace(country_name, "Congo, Dem\\. Rep\\.", "DR Congo"))


## 5. VISUALIZATION ----

#### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    text = "#2B2B2B",
    subtext = "#5A5A5A"
  )
)

# Encoding colors hardcoded at the geoms 
col_ssf <- "#B8652B"
col_eas <- "#2A6475"
col_rest <- "#DAD6CE"
col_label <- "#6B6B6B"

### |- titles and caption ----
title_text <- "Growth Slowed. The Yearly Additions Didn't. Where They Happen Did."

subtitle_text <- str_glue(
  "From 1981 to 2020, the growth rate fell from {percent(rate_81, 0.1)} to ",
  "{percent(rate_16, 0.1)} a year, yet the world still added roughly 83–90 million people a year.<br>",
  "<span style='color:{col_ssf}'>**Sub-Saharan Africa's**</span> contribution rose from ",
  "12M to 29M a year as <span style='color:{col_eas}'>**East Asia & Pacific's**</span> ",
  "fell from 25M to 14M.<br>",
  "<span style='font-size:9pt; color:{col_label}'>",
  "1 square = 1 million people of net population growth per year (5-year average) · ",
  "gray = rest of world</span>"
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 39,
  source_text = str_glue(
    "World Bank, World Development Indicators (SP.POP.TOTL), midyear estimates<br>",
    "Note: Net growth = births − deaths ± migration. Current World Bank regions (the 2024 ",
    "Afghanistan/Pakistan move affects neither highlighted region); no region shrank in any window.<br>",
    "Low 1961–65 total partly reflects China's 1959–61 famine. ",
    "Largest Sub-Saharan contributors, 2021–25: {str_flatten_comma(top_ssf$country_name, ', and ')}."
  )
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      family = fonts$title_1, size = 24, face = "bold", 
      color = "#2B2B2B", lineheight = 1.05,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 11.5,
      color = "#3F3F3F", lineheight = 1.35,
      margin = margin(b = 18)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 6,
      color = "#9A9A9A", lineheight = 1.35,
      margin = margin(t = 14)
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(20, 25, 12, 25)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- ggplot() +
  # Geoms
  geom_tile(
    data = tiles,
    aes(x = x, y = y, fill = group),
    width = 0.86, height = 0.86
  ) +
  geom_text(
    data = window_labels,
    aes(x = x, y = y, label = window),
    family = fonts$text, size = 2.7, color = col_label
  ) +
  geom_text(
    data = era_labels,
    aes(x = x, y = y, label = label, hjust = hjust),
    family = fonts$text, size = 2.8, fontface = "bold",
    color = "#4A4A4A", vjust = 0, lineheight = 1.05
  ) +
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = label, hjust = hjust),
    family = fonts$text, size = 2.6, color = "#5A5A5A",
    vjust = 1, lineheight = 1.1
  ) +
  # Scales
  scale_fill_manual(values = c(SSF = col_ssf, EAS = col_eas, Rest = col_rest)) +
  scale_x_continuous(expand = expansion(add = 1)) +
  scale_y_continuous(
    limits = c(-5.5, max_row + 4.6),
    expand = expansion(add = 0)
  ) +
  coord_equal(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")


### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_39", "2026_39.png"),
  width = fig_w, height = fig_h,
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

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-29
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# countrycode  * 1.9.0   2026-08-20 [1] CRAN (R 4.6.1)
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
# ggtext       * 0.2.0   2026-08-28 [1] CRAN (R 4.6.1)
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
# readxl       * 1.5.0   2026-05-16 [1] CRAN (R 4.6.0)
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
# ────────────────────────────────────────────────────────────────────────────────

