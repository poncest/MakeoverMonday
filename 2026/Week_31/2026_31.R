## Challenge: #MakeoverMonday 2026 week 31
## Data:      America’s Services Trade Balances with Its Free Trade Partners
## Author:    Steven Ponce
## Date:      2026-08-03

## Article
# https://www.voronoiapp.com/geopolitics/Ranked-Americas-Services-Trade-Balances-with-Its-Free-Trade-Partners-4860

## Data
# https://pub-cee805df54de4b6c8f93bee984e3c725.r2.dev/datasets/america-s-services-trade-balances-with-its-free-trade-partners/Americas%20Services%20Trade%20Balances.xlsx

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview, patchwork
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel(
  "data/2026/Americas_Services_Trade_Balances.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- verify the central claim ----
# "Canada, South Korea, and Mexico are the only 3 of 15 partners with a
# services surplus AND an overall deficit"
n_total <- nrow(df_raw)

df_selected <- df_raw |>
  filter(services_b > 0, goods_and_services_b < 0)

n_selected <- nrow(df_selected)

### |- scope to the three hero countries ----
hero_countries <- c("Canada", "South Korea", "Mexico")

df3 <- df_raw |>
  filter(country %in% hero_countries) |>
  mutate(country = factor(country, levels = hero_countries))

### |- long format, sign-coded ----
# "Overall" -> "Goods + services"
df_sign <- df3 |>
  select(country, Services = services_b, `Goods + services` = goods_and_services_b) |>
  pivot_longer(cols = -country, names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(metric, levels = c("Services", "Goods + services")),
    sign = if_else(value >= 0, "Positive", "Negative"),
    label = label_dollar(accuracy = 0.1, style_positive = "plus")(value) |>
      str_replace("^-", "\u2212") |>
      paste0("B")
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    positive  = "#1E3A5F",
    negative  = "#B5532F",
    zero_line = "gray20",
    grid      = "gray92"
  )
)
clrs <- colors$palette
col_positive <- clrs[["positive"]]
col_negative <- clrs[["negative"]]
col_zero     <- clrs[["zero_line"]]
col_grid     <- clrs[["grid"]]

### |- titles and caption ----
title_text <- "Large U.S. Services Surpluses Mask Overall Trade Deficits"

subtitle_text <- str_glue(
  "Only {n_selected} of {n_total} U.S. free trade partners have a services ",
  "surplus but an overall trade deficit (2024):<br>",
  "{glue_collapse(hero_countries, sep = ', ', last = ', and ')}."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 31,
  source_text = paste0(
    "Positive values indicate a U.S. surplus; negative values indicate a ",
    "U.S. deficit.<br>",
    "Source: U.S. Bureau of Economic Analysis (2024), via Visual Capitalist ",
    "and the Hinrich Foundation."
  )
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    legend.position = "none",
    axis.text = element_text(size = 9, family = fonts$text),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11.5, margin = margin(b = 4), family = fonts$title_2),
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold", size = 22, family = fonts$title_1, color = colors$title,
      margin = margin(b = 4), lineheight = 1.15
    ),
    plot.subtitle = element_textbox_simple(
      color = colors$subtitle, size = 10, family = fonts$subtitle,
      lineheight = 1.25, margin = margin(b = 8)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0, size = 6, color = colors$caption,
      family = fonts$caption, margin = margin(t = 8)
    ),
    panel.spacing.x = unit(1.1, "lines"),
    plot.margin = margin(t = 10, r = 16, b = 6, l = 12)
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- ggplot(df_sign, aes(x = metric, y = value, fill = sign)) +
  geom_col(
    data = df_sign |> filter(sign == "Positive"),
    aes(fill = sign), width = 0.70
  ) +
  geom_col(
    data = df_sign |> filter(sign == "Negative"),
    aes(fill = sign), width = 0.62
  ) +
  geom_hline(yintercept = 0, color = col_zero, linewidth = 0.6) +
  geom_text(
    data = df_sign |> filter(sign == "Positive"),
    aes(label = label), vjust = -0.5, size = 3.6, fontface = "bold", color = "gray15"
  ) +
  geom_text(
    data = df_sign |> filter(sign == "Negative"),
    aes(label = label), vjust = 1.5, size = 3.6, fontface = "bold", color = "gray15"
  ) +
  facet_wrap(~country, nrow = 1) +
  scale_fill_manual(values = c(Positive = col_positive, Negative = col_negative)) +
  scale_y_continuous(
    labels = label_dollar(suffix = "B"),
    breaks = c(-200, -100, 0, 50),
    expand = expansion(mult = 0.13)
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL, y = NULL
  ) +
  canvas(width = 10, height = 5.6, units = "in")

### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_31", "2026_31.png"),
  width = 10, height = 5.6, dpi = 320
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

# ─ Session info ────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.5.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-08-03
# rstudio  2026.07.1+147 Pacific Dogwood (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
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
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# patchwork    * 1.3.2   2025-08-25 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# readxl         1.5.0   2026-05-16 [1] CRAN (R 4.6.0)
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
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────

