## Challenge: #MakeoverMonday 2026 week 17
## Data:      2024 EIU Democracy Index
## Author:    Steven Ponce
## Date:      2026-04-27

## Article
# https://www.eiu.com/n/campaigns/democracy-index-2024/
# https://ourworldindata.org/grapher/democracy-index-eiu

## Data
# https://data.world/makeovermonday/2026w17-2024-eiu-democracy-index/activity

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, patchwork, ggbeeswarm
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
df_raw <- readxl::read_excel("data/2026/2024 Democracy Index.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- regime type factor levels ----
regime_levels <- c(
  "Authoritarian regime",
  "Hybrid regime",
  "Flawed democracy",
  "Full democracy"
)

### |- clean and encode ----
df <- df_raw |>
  mutate(
    regime_classification = factor(regime_classification, levels = regime_levels),
    region_short = case_when(
      world_region_according_to_owid == "North America" ~ "North\nAmerica",
      world_region_according_to_owid == "South America" ~ "South\nAmerica",
      world_region_according_to_owid == "Europe" ~ "Europe",
      world_region_according_to_owid == "Asia" ~ "Asia",
      world_region_according_to_owid == "Africa" ~ "Africa",
      world_region_according_to_owid == "Oceania" ~ "Oceania",
      TRUE ~ world_region_according_to_owid
    )
  )

### |- Panel A data: global median trend ----
df_trend <- df |>
  group_by(year) |>
  summarise(
    global_median = median(democracy_index, na.rm = TRUE),
    .groups = "drop"
  )

# 2006 and 2024 endpoints for annotation
trend_2006 <- df_trend |>
  filter(year == 2006) |>
  pull(global_median)
trend_2024 <- df_trend |>
  filter(year == 2024) |>
  pull(global_median)

### |- Panel B data: 2024 cross-section with region medians ----
df_2024 <- df |>
  filter(year == 2024)

# Region medians for sorting and overlay dots
df_region_medians <- df_2024 |>
  group_by(region_short) |>
  summarise(
    region_median = median(democracy_index, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(region_median))

# Sorted region order (highest median → top of chart after coord_flip)
region_order <- df_region_medians$region_short

df_2024 <- df_2024 |>
  mutate(region_short = factor(region_short, levels = region_order))


## 5. VISUALIZATION ----

## |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    full_dem    = "#1D9E75",
    flawed_dem  = "#74ADD1",
    hybrid      = "#E09338",
    authoritar  = "#4A1A2C",
    trend_line  = "#1A1A2E",
    country_bg  = "#D9D9D9",
    region_dot  = "#F7F5F2",
    background  = "#F7F5F2"
  )
)

# Convenience aliases
col_full_dem <- colors$palette$full_dem
col_flawed_dem <- colors$palette$flawed_dem
col_hybrid <- colors$palette$hybrid
col_authoritar <- colors$palette$authoritar
col_trend <- colors$palette$trend_line
col_country_bg <- colors$palette$country_bg
col_region_dot <- colors$palette$region_dot
col_background <- colors$palette$background

# Named vector for regime color scale
regime_colors <- c(
  "Full democracy"       = col_full_dem,
  "Flawed democracy"     = col_flawed_dem,
  "Hybrid regime"        = col_hybrid,
  "Authoritarian regime" = col_authoritar
)

### |- titles and caption ----
title_text <- "Democracy Has Declined\u2013But Remains Deeply Divided"

subtitle_text <- "Global scores have declined since 2006, while countries remain sharply divided between democratic\nand authoritarian systems"

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 17,
  source_text = "Economist Intelligence Unit (2006–2024), Democracy Index<br>with major processing by Our World in Data"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base and weekly theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Panel
    panel.background = element_rect(fill = col_background, color = NA),
    plot.background = element_rect(fill = col_background, color = NA),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.25),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Axes
    axis.ticks = element_blank(),
    axis.title = element_text(size = 9, color = "gray40"),
    axis.text = element_text(size = 8, color = "gray30"),

    # Legend
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 8, color = "gray20"),
    legend.key.size = unit(8, "pt"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_rect(fill = NA, color = NA),
    legend.spacing.x = unit(10, "pt"),

    # Plot margin
    plot.margin = margin(6, 10, 6, 10)
  )
)

theme_set(weekly_theme)

### |- Panel A: Global Median Trend (2006–2024) ----
p_trend <- ggplot() +

  # Geoms
  geom_line(
    data = df |> filter(!is.na(democracy_index)),
    aes(x = year, y = democracy_index, group = entity),
    color = col_country_bg,
    linewidth = 0.25,
    alpha = 0.5
  ) +
  geom_line(
    data = df_trend,
    aes(x = year, y = global_median),
    color = col_trend,
    linewidth = 1.2
  ) +
  geom_point(
    data  = df_trend |> filter(year %in% c(2006, 2024)),
    aes(x = year, y = global_median),
    color = col_trend,
    size  = 3
  ) +
  geom_hline(
    yintercept = 5.0,
    linetype   = "dashed",
    color      = "gray60",
    linewidth  = 0.3
  ) +
  # Annnotate
  annotate(
    "text",
    x = 2006, y = trend_2006 + 0.4,
    label = glue("{round(trend_2006, 2)}"),
    size = 3, hjust = 0.5,
    color = col_trend,
    fontface = "bold",
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 2024, y = trend_2024 + 0.4,
    label = glue("{round(trend_2024, 2)}"),
    size = 3, hjust = 0.5,
    color = col_trend,
    fontface = "bold",
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 2011.5, y = 4.35,
    label = "Global democracy has declined\nfor nearly two decades",
    size = 2.9, hjust = 0,
    color = "gray20",
    fontface = "bold",
    lineheight = 1.2,
    fill = "#F7F5F2",
    label.size = 0,
    label.padding = unit(0.25, "lines"),
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 2011.5, y = 3.38,
    label = glue("({round(trend_2006 - trend_2024, 2)} point drop, {min(df_trend$year)}\u2013{max(df_trend$year)})"),
    size = 2.5, hjust = 0,
    color = "gray45",
    lineheight = 1.2,
    family = fonts$text
  ) +
  annotate(
    "text",
    x = 2006.2, y = 5.19,
    label = "Scale midpoint (5.0)",
    size = 2.3, hjust = 0,
    color = "gray50",
    family = fonts$text
  ) +
  # Scales
  scale_x_continuous(
    breaks = seq(2006, 2024, by = 4),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, by = 2),
    labels = label_number(accuracy = 1)
  ) +
  # Labs
  labs(
    x = NULL,
    y = "Democracy Index (0–10)",
    title = "**Global median has declined steadily since 2006**",
    subtitle = "Gray lines = individual countries · Dark line = global median"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(size = 12, color = "gray10", family = fonts$title, margin = margin(b = 3)),
    plot.subtitle = element_text(size = 9, color = "gray40", family = fonts$subtitle, margin = margin(b = 6))
  )

### |- Panel B: 2024 Beeswarm by Region ----
p_beeswarm <- ggplot() +

  # Geoms
  geom_beeswarm(
    data = df_2024 |>
      mutate(dot_alpha = if_else(
        regime_classification %in% c("Full democracy", "Authoritarian regime"),
        0.95, 0.60
      )),
    aes(
      x = region_short, y = democracy_index, color = regime_classification,
      alpha = I(dot_alpha)
    ),
    size = 2.2,
    cex = 2.8,
    method = "swarm"
  ) +
  geom_point(
    data = df_region_medians |>
      mutate(region_short = factor(region_short, levels = region_order)),
    aes(x = region_short, y = region_median),
    shape = 23,
    size = 4,
    fill = col_region_dot,
    color = col_trend
  ) +
  geom_hline(yintercept = 8.0, linetype = "dotted", color = "gray55", linewidth = 0.3) +
  geom_hline(yintercept = 6.0, linetype = "dotted", color = "gray55", linewidth = 0.3) +
  geom_hline(yintercept = 4.0, linetype = "dotted", color = "gray55", linewidth = 0.3) +
  # Annotate
  annotate("text",
    x = 6.6, y = 8.18, label = "Full democracy \u2265 8.0",
    size = 2.6, hjust = 1, color = "gray35", family = fonts$text
  ) +
  annotate("text",
    x = 6.6, y = 6.18, label = "Flawed democracy \u2265 6.0",
    size = 2.6, hjust = 1, color = "gray35", family = fonts$text
  ) +
  annotate("text",
    x = 6.6, y = 4.18, label = "Hybrid regime \u2265 4.0",
    size = 2.6, hjust = 1, color = "gray35", family = fonts$text
  ) +
  annotate(
    "text",
    x = 5.15, y = 1.3,
    label = "Most African countries\ncluster below 4.0",
    size = 2.5, hjust = 0.5,
    color = col_authoritar,
    lineheight = 1.2,
    family = fonts$text
  ) +
  # Scales
  scale_color_manual(
    values = regime_colors,
    guide = guide_legend(
      override.aes = list(size = 4),
      nrow = 1
    )
  ) +
  scale_y_continuous(
    limits = c(0, 10.5),
    breaks = seq(0, 10, by = 2),
    labels = label_number(accuracy = 1)
  ) +
  # Labs
  labs(
    x = NULL,
    y = "Democracy Index (0–10)",
    title = "**2024: Countries cluster at the extremes\u2013few sit in the middle**",
    subtitle = "Each dot = one country \u00b7 \u25c6 = regional median \u00b7 Regions sorted by median score"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(size = 12, color = "gray10", family = fonts$title, margin = margin(b = 3)),
    plot.subtitle = element_text(size = 9, color = "gray40", family = fonts$subtitle, margin = margin(b = 6)),
    legend.position = "top",
    legend.justification = "left"
  )

### |- Combined plots ----
p_combined <- p_trend / p_beeswarm +
  plot_layout(heights = c(2, 3)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = 24, face = "bold",
        color = "gray10",
        family = fonts$title,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 12, color = "gray30",
        family = fonts$subtitle,
        margin = margin(b = 12)
      ),
      plot.caption = element_markdown(
        size = 7, color = "gray50",
        family = fonts$caption,
        hjust = 0,
        margin = margin(t = 10)
      ),
      plot.background = element_rect(fill = col_background, color = NA),
      plot.margin = margin(16, 16, 10, 16)
    )
  )


### |- Preview ----
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
 
# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-27
# rstudio  2026.01.2+418 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# beeswarm       0.4.0    2021-06-01 [1] CRAN (R 4.5.2)
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
# ggbeeswarm   * 0.7.3    2025-11-29 [1] CRAN (R 4.5.3)
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
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# readxl         1.4.5    2025-03-07 [1] CRAN (R 4.5.3)
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
# vipor          0.4.7    2023-12-18 [1] CRAN (R 4.5.3)
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
# ──────────────────────────────────────────────────────────────────