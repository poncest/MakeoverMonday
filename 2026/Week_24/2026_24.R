## Challenge: #MakeoverMonday 2026 week 24
## Data:      Global Top Foreign Investments
## Author:    Steven Ponce
## Date:      2026-06-16

## Article
# https://www.visualcapitalist.com/the-worlds-largest-investor-countries/

## Data
# https://data.world/makeovermonday/2026w23-global-top-foreign-investments

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, janitor
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 8,
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
df_raw <- readxl::read_excel("data/2026/MM2026-WK24-LargestFDI.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- OFC classification (Garcia-Bernardo et al. 2017) ----
conduit_ofc <- c(
  "Netherlands", "United Kingdom", "Switzerland",
  "Singapore", "Ireland"
)
sink_ofc <- c(
  "British Virgin Islands", "Cayman Islands", "Bermuda",
  "Luxembourg", "Hong Kong", "Jersey", "Mauritius"
)

norm <- function(x) str_squish(str_to_lower(x))

df <- df_raw |>
  rename(fdi_m = fdi_outflows_in_2024_m) |>
  mutate(country = if_else(country == "enmark", "Denmark", country)) |>
  arrange(desc(fdi_m)) |>
  mutate(
    rank = row_number(),
    is_ofc = norm(country) %in% norm(c(conduit_ofc, sink_ofc)),
    category = if_else(is_ofc,
      "Offshore financial center",
      "Productive economy"
    )
  )

global_total <- sum(df$fdi_m)
top6_share <- sum(df$fdi_m[df$rank <= 6]) / global_total

### |- top 20 for the chart ----
df_plot <- df |>
  filter(rank <= 20) |>
  mutate(
    country   = fct_reorder(country, fdi_m),
    label_b   = paste0("$", comma(round(fdi_m / 1000)), "B")
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    ofc        = "#8E3B46",
    productive = "#B7B4AC",
    text_dark  = "#2B2B2B",
    text_muted = "#6E6B66"
  )
)

col_ofc <- clrs$palette$ofc
col_prod <- clrs$palette$productive
col_dark <- clrs$palette$text_dark
col_muted <- clrs$palette$text_muted

### |- titles and caption ----
title_text <- str_glue(
  "The World's Largest \u201cInvestor Countries\u201d ",
  "Are Often Financial Hubs"
)

subtitle_text <- str_glue(
  "Five of the ten largest sources of foreign direct investment in 2024 were ",
  "recognized <b style='color:{col_ofc}'>offshore financial centers</b> \u2014 ",
  "including Luxembourg, Hong Kong, and the British Virgin Islands."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 24,
  source_text = paste0(
    "Visual Capitalist \u00b7 FDI outflows 2024 \u00b7 top 20 of 50 shown ",
    "(data.world)<br>",
    "OFC classification: Garcia-Bernardo et al. (2017) \u2014 academic, ",
    "not an official designation"
  )
)

### |- annotation text ----
ann_group <- "Offshore financial\ncenters"
ann_mech <- str_glue(
  "Luxembourg and the British Virgin Islands each report\n",
  "larger outflows than Germany or India."
)

### |- annotation coordinates  ----
grp_x  <- 128000; grp_y  <- 17.3   
mech_x <- 128000; mech_y <- 14.2  

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_markdown(
      face = "bold", size = rel(1.48),
      margin = margin(b = 4), family = fonts$title_1
    ),
    plot.subtitle = element_textbox_simple(
      size = rel(0.85), color = col_muted, lineheight = 1.1,
      width = unit(1, "npc"), margin = margin(b = 16), family = fonts$subtitle
    ),
    plot.caption = element_markdown(
      size = rel(0.5), color = col_muted,
      hjust = 0, margin = margin(t = 14), family = fonts$caption
    ),
    axis.text.y = element_text(size = rel(0.95), color = col_dark),
    axis.text.x = element_text(color = col_muted),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )
)

theme_set(weekly_theme)

### |- final plot ----
p <- ggplot(df_plot, aes(x = fdi_m, y = country, fill = category)) +
  # Geoms
  geom_col(width = 0.74) +
  geom_text(aes(label = label_b),
    hjust = -0.15, size = 3.2,
    color = col_dark
  ) +
  # Annotate
  annotate("text",
    x = grp_x, y = grp_y, label = ann_group,
    hjust = 0, vjust = 1, color = col_ofc, fontface = "bold",
    size = 4.0, lineheight = 0.95
  ) +
  annotate("text",
    x = mech_x, y = mech_y, label = ann_mech,
    hjust = 0, vjust = 1, color = col_muted, size = 3.1,
    lineheight = 0.95
  ) +
  # Scales
  scale_fill_manual(values = c(
    "Offshore financial center" = col_ofc,
    "Productive economy"        = col_prod
  )) +
  scale_x_continuous(
    labels = label_number(
      scale = 1e-3, prefix = "$", suffix = "B",
      accuracy = 1
    ),
    expand = expansion(mult = c(0, 0.12))
  ) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  ) +
  coord_cartesian(clip = "off")


### |-  Preview ----
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
 
# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-16
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
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
# ────────────────────────────────────────────────────────────────────────────────