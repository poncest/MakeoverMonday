## Challenge: #MakeoverMonday 2026 week 07
## Data:      Living Planet Index
## Author:    Steven Ponce
## Date:      2026-02-16

## Article
# https://livingplanet.panda.org/en-GB/
# https://www.livingplanetindex.org/latest_results

## Data
# https://data.world/makeovermonday/2026w7-living-planet-index

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  janitor, lubridate, patchwork
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
LPI_2024_raw <- readxl::read_excel("data/2026/LPI 2024.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(LPI_2024_raw)


## 4. TIDY DATA ----

### |-  prepare data ----
lpi_raw <- readxl::read_excel("data/2026/LPI 2024.xlsx") |>
  clean_names()

lpi_data <- lpi_raw |>
  mutate(
    category = str_trim(category),
    lpi_pct = lpi_final * 100,
    ci_low = ci_low * 100,
    ci_high = ci_high * 100,
    category_label = case_when(
      category == "Europe and Central Asia" ~ "Europe & Central Asia",
      category == "Latin America and Caribbean" ~ "Latin America & Caribbean",
      TRUE ~ category
    )
  )

endpoints <- lpi_data |>
  filter(year == 2020) |>
  mutate(
    pct_remaining = round(lpi_pct),
    pct_lost = round(100 - lpi_pct)
  )

global_end <- endpoints |> filter(category == "Global")


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    danger        = "#C0392B",
    primary       = "#2C3E50",
    freshwater    = "#2BA8A0",
    terrestrial   = "#8E6F47",
    marine        = "#2471A3",
    neutral_dark  = "#5D6D7E",
    neutral_mid   = "#AEB6BF",
    highlight     = "#E67E22"
  )
)

### |-  Main titles ----
title_text <- NULL

subtitle_text <- NULL

caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 07,
  source_text = glue(
    "Bars show % lost; lines show % remaining.<br>",
    "Living Planet Index tracks population trends, not species counts.<br>",
    "ZSL & WWF Living Planet Index 2024 | ",
    "34,836 populations across 5,495 vertebrate species"
  )
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
    panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- PANEL 1 — DAMAGE REPORT (% LOST) ----

bars <- endpoints |>
  filter(category != "Global") |>
  mutate(
    category_label = fct_reorder(category_label, pct_lost),
    bar_color = case_when(
      pct_lost >= 80 ~ colors$palette$danger,
      pct_lost >= 60 ~ colors$palette$highlight,
      TRUE ~ colors$palette$neutral_dark
    )
  )

p1 <- ggplot(bars, aes(pct_lost, category_label)) +
  # Geoms
  geom_col(aes(fill = bar_color), width = 0.6) +
  geom_text(
    aes(label = paste0(pct_lost, "% lost")),
    hjust = -0.08,
    size = 3.5,
    fontface = "bold",
    family = fonts$text,
    color = colors$palette$primary
  ) +
  geom_vline(
    xintercept = global_end$pct_lost,
    linetype = "dashed",
    linewidth = 0.4,
    color = colors$palette$primary
  ) +
  annotate(
    "text",
    x = global_end$pct_lost - 1,
    y = 0.6,
    label = glue("Global avg: {global_end$pct_lost}% lost"),
    hjust = 1,
    size = 2.8,
    family = fonts$text,
    color = colors$palette$primary
  ) +
  # Scales
  scale_fill_identity() +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 25),
    labels = ~ paste0(.x, "%")
  ) +
  # Labs
  labs(
    title = "Wildlife Loss Since 1970 Is Uneven — and Extreme in Freshwater Systems",
    subtitle = glue(
      "% of monitored vertebrate populations lost by 2020 | ",
      "Living Planet Index (1970 = 100 baseline)"
    ),
    x = "% of 1970 population lost",
    y = NULL
  )

### |- PANEL 2 — ECOSYSTEM TRENDS (% REMAINING) ----
make_panel <- function(cat, col, bold = FALSE) {
  d <- lpi_data |> filter(category == cat)
  ep <- endpoints |> filter(category == cat)

  ggplot(d, aes(year)) +
    # Geoms
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
      fill = col, alpha = 0.12
    ) +
    geom_hline(
      yintercept = 100, linetype = "dotted",
      color = colors$palette$neutral_dark, linewidth = 0.2
    ) +
    geom_line(aes(y = lpi_pct), color = col, linewidth = 1) +
    geom_point(
      data = d |> filter(year == 2020),
      aes(y = lpi_pct),
      color = col,
      size = 2
    ) +
    annotate(
      "text",
      x = 2022,
      y = ep$lpi_pct,
      label = paste0(ep$pct_remaining, "%"),
      hjust = 0,
      size = 3.2,
      fontface = "bold",
      family = fonts$text,
      color = col
    ) +
    # Scales
    scale_y_continuous(
      limits = c(0, 115),
      breaks = c(0, 50, 100),
      labels = ~ paste0(.x, "%")
    ) +
    scale_x_continuous(
      breaks = c(1970, 1995, 2020),
      expand = expansion(mult = c(0.02, 0.15))
    ) +
    # Labs
    labs(
      title = cat,
      x = NULL,
      y = NULL
    ) +
    # Theme
    theme(
      plot.title = element_text(
        face = ifelse(bold, "bold", "plain"),
        family = fonts$title
      ),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
    )
}

p2_global <- make_panel("Global", colors$palette$primary)
p2_terr <- make_panel("Terrestrial", colors$palette$terrestrial)
p2_fresh <- make_panel("Freshwater", colors$palette$freshwater, bold = TRUE)
p2_marine <- make_panel("Marine", colors$palette$marine)

p2 <- (p2_global / (p2_terr | p2_fresh | p2_marine)) +
  plot_annotation(
    subtitle = "Lines show % of populations remaining since 1970",
    theme = theme(
      plot.subtitle = element_text(
        family = fonts$text,
        size = 10,
        color = colors$palette$neutral_dark,
        margin = margin(b = 10)
      ),
      panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
    )
  )

### |- COMBINED PLOTS ----
combined_plots <- (p1 / p2) +
  plot_layout(heights = c(1.2, 1.3)) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.4),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 0, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.8),
        family = fonts$subtitle,
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
  )

snap(combined_plots)


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

# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-02-16
# rstudio  2026.01.0+392 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [1] local (on disk 4.4.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.4.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.4.0)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.4.3)
# commonmark     1.9.1    2024-01-30 [1] CRAN (R 4.4.0)
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
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.4.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.4.3)
# magick         2.9.0    2025-09-08 [1] CRAN (R 4.4.3)
# magrittr       2.0.4    2025-09-12 [1] CRAN (R 4.4.3)
# markdown       1.12     2023-12-06 [1] CRAN (R 4.4.0)
# methods      * 4.4.0    2024-04-24 [1] local
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.4.1)
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.4.3)
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
# Rcpp           1.0.13   2024-07-17 [1] CRAN (R 4.4.1)
# readr        * 2.1.6    2025-11-14 [1] CRAN (R 4.4.3)
# readxl         1.4.5    2025-03-07 [1] CRAN (R 4.4.3)
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
# utils        * 4.4.0    2024-04-24 [1] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.4.3)
# withr          3.0.1    2024-07-31 [1] CRAN (R 4.4.1)
# xfun           0.47     2024-08-17 [1] CRAN (R 4.4.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.4.3)
# 
# V ── Loaded and on-disk version mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────
# > 