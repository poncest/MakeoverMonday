## Challenge: #MakeoverMonday 2026 week 20
## Data:      UK Fuel Prices
## Author:    Steven Ponce
## Date:      2026-05-19

## Article
# https://www.rac.co.uk/drive/advice/fuel-watch/

## Data
# https://data.world/makeovermonday/2026w20-uk-fuel-prices

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, patchwork
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
df_raw <- readxl::read_excel("data/2026/UK Fuel Prices (2013-2026).xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

## Core series: pump prices inc VAT, proper Date column
df <- df_raw |>
  mutate(date = as.Date(date)) |>
  select(date, diesel = diesel_pump_inc_vat, petrol = unleaded_pump_inc_vat) |>
  arrange(date)

## Baseline: first observation (Jan 2013 = 100)
baseline_diesel <- df$diesel[1]
baseline_petrol <- df$petrol[1]

df_indexed <- df |>
  mutate(
    diesel_idx = diesel / baseline_diesel * 100,
    petrol_idx = petrol / baseline_petrol * 100
  )

## Panel B — diesel premium over petrol (pence)
df_spread <- df |>
  mutate(spread = diesel - petrol)

## Pre-Ukraine baseline (data before Feb 2022)
pre_ukraine_mean_spread <- df_spread |>
  filter(date < as.Date("2022-02-24")) |>
  summarise(m = mean(spread)) |>
  pull(m)

## Verify spread claims
spread_summary <- df_spread |>
  mutate(era = case_when(
    date < as.Date("2022-02-24") ~ "pre-Ukraine",
    date < as.Date("2026-02-28") ~ "Ukraine era",
    TRUE ~ "Iran era"
  )) |>
  group_by(era) |>
  summarise(
    mean_spread = mean(spread),
    median_spread = median(spread),
    min_spread = min(spread),
    max_spread = max(spread),
    .groups = "drop"
  )

## Peak values for annotations
ukraine_peak <- df_spread |>
  filter(date >= as.Date("2022-02-24"), date < as.Date("2023-06-01")) |>
  slice_max(spread, n = 1, with_ties = FALSE)

iran_peak <- df_spread |>
  filter(date >= as.Date("2026-02-28")) |>
  slice_max(spread, n = 1, with_ties = FALSE)

## Latest spread value
latest <- df_spread |> slice_max(date, n = 1, with_ties = FALSE)
latest_date <- latest$date
latest_spread <- latest$spread

## Key event dates
ukraine_date <- as.Date("2022-02-24")
iran_date <- as.Date("2026-02-28")


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_diesel  = "#9C5A2C",   
    col_petrol  = "#8B98A3",  
    col_spread  = "#426C7A",  
    col_ref     = "#B8B4AE",   
    col_text    = "#2C2C2C",
    col_sub     = "#777777",
    col_bg      = "#FAFAF7",
    col_grid    = "#E8E8E2",
    col_band    = "#426C7A",  
    col_shock   = "#D9D4CD"   
  )
)

col_diesel <- colors$palette$col_diesel
col_petrol <- colors$palette$col_petrol
col_spread <- colors$palette$col_spread
col_ref <- colors$palette$col_ref
col_text <- colors$palette$col_text
col_sub <- colors$palette$col_sub
col_bg <- colors$palette$col_bg
col_grid <- colors$palette$col_grid
col_band <- colors$palette$col_band
col_shock <- colors$palette$col_shock
  

### |-  titles and caption ----
title_text <- str_glue("Diesel's Crisis Premium")

subtitle_text <- str_glue(
  "Diesel prices stayed only slightly above petrol for most of the 2010s. ",
  "During the Ukraine and Iran conflicts,<br>the premium widened sharply."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 20,
  source_text = "RAC Fuel Watch | data.world/makeovermonday/2026w20-uk-fuel-prices"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    axis.text  = element_text(color = col_sub, size = 8.5),
    axis.title = element_blank(),    
    axis.title.x = element_blank(),      
    axis.title.y = element_blank(),   
    axis.ticks = element_blank(),
    plot.margin = margin(8, 16, 8, 16)
  )
)

theme_set(weekly_theme)

### |-  Panel A Indexed prices (context / scaffolding) ----
p_a <- ggplot(df_indexed, aes(x = date)) +

  # Rect
  annotate("rect",
    xmin = ukraine_date, xmax = as.Date("2023-09-01"),
    ymin = -Inf, ymax = Inf,
    fill = col_shock, alpha = 0.10
  ) +
  annotate("rect",
    xmin = iran_date, xmax = as.Date("2026-04-28"),
    ymin = -Inf, ymax = Inf,
    fill = col_shock, alpha = 0.10
  ) +

  # Geom
  geom_line(aes(y = petrol_idx),
    color = col_petrol,
    linewidth = 0.5, alpha = 0.5
  ) +
  geom_line(aes(y = diesel_idx),
    color = col_diesel,
    linewidth = 0.9
  ) +
  geom_hline(
    yintercept = 100, color = col_ref,
    linewidth = 0.6, linetype = "dashed"
  ) +

  # Annotate
  annotate("text",
    x = as.Date("2026-06-01"), y = 107,
    label = "Diesel", color = col_diesel,
    size = 3.0, fontface = "bold", hjust = 0
  ) +
  annotate("text",
    x = as.Date("2026-06-01"), y = 99,
    label = "Petrol", color = col_petrol,
    size = 3.0, fontface = "bold", hjust = 0
  ) +

  # Scales
  scale_x_date(
    date_breaks = "2 years", date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.10))
  ) +
  scale_y_continuous(
    breaks = seq(80, 160, by = 20),
    labels = function(x) rep("", length(x))
  ) +
  coord_cartesian(ylim = c(72, 155), clip = "off") +
  labs(subtitle = "**Indexed prices** \u2014 Jan 2013 = 100") +

  # Theme
  theme(
    panel.grid.major.y = element_blank(),
  plot.subtitle = element_markdown(
      size = 11, color = col_sub, margin = margin(b = 4), family = fonts$text
    )
  )

### |-  Panel B: Diesel premium over petrol (payoff panel) ----
latest_label   <- paste0("+", round(latest_spread, 1), "p")
ukraine_label  <- paste0("2022 peak: +", round(ukraine_peak$spread, 1), "p")

p_b <- ggplot(df_spread, aes(x = date, y = spread)) +

  # Rect
  annotate("rect",
    xmin = ukraine_date, xmax = as.Date("2023-09-01"),
    ymin = -Inf, ymax = Inf,
    fill = col_shock, alpha = 0.10
  ) +
  annotate("rect",
    xmin = iran_date, xmax = as.Date("2026-04-28"),
    ymin = -Inf, ymax = Inf,
    fill = col_shock, alpha = 0.10
  ) +
  annotate("rect",
    xmin = min(df_spread$date), xmax = max(df_spread$date),
    ymin = 0, ymax = pre_ukraine_mean_spread,
    fill = col_band, alpha = 0.06
  ) +

  # Geoms
  geom_hline(
    yintercept = pre_ukraine_mean_spread,
    color = col_ref, linewidth = 0.5, linetype = "dashed"
  ) +
  geom_area(fill = col_spread, alpha = 0.07) +
  geom_line(color = col_spread, linewidth = 1.4) +

  # Annotate
  annotate("text",
    x = ukraine_peak$date - 60,
    y = ukraine_peak$spread + 1.8,
    label = ukraine_label,
    size = 3.0, color = col_sub, hjust = 1, fontface = "italic"
  ) +
  geom_point(
    data = ukraine_peak, aes(x = date, y = spread),
    color = col_sub, size = 2.0, shape = 21,
    fill = col_bg, stroke = 1.2
  ) +

  # Annotate
  annotate("text",
    x = latest_date - 50,
    y = latest_spread + 2.2,
    label = latest_label,
    size = 4.4, color = col_diesel, hjust = 1, fontface = "bold"
  ) +
  geom_point(
    data = latest, aes(x = date, y = spread),
    color = col_spread, size = 3.0
  ) +
  annotate("text",
    x = ukraine_peak$date + 20,
    y = ukraine_peak$spread + 5.0,
    label = "Ukraine\nconflict",
    size = 2.6, color = col_sub, hjust = 0.5, lineheight = 0.97
  ) +
  # Scales
  scale_x_date(
    date_breaks = "2 years", date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, "p"),
    breaks = seq(-5, 40, by = 5)
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(subtitle = "**Diesel premium over petrol**") +

  # Theme
  theme(
    plot.subtitle = element_markdown(
      size = 11, color = col_sub, margin = margin(b = 4), family = fonts$text
    )
  )

### |-  Combine plots ----
p_combined <- (p_a / p_b) +
    plot_layout(heights = c(0.85, 1.15)) +
    plot_annotation(
      title = title_text,
      subtitle = subtitle_text,
      caption  = caption_text,
      theme = theme(
        plot.background = element_rect(fill = col_bg, color = NA),
        plot.title = element_text(
          face = "bold", size = 30, color = col_text,
          margin = margin(t = 12, b = 4), family = fonts$title
        ),
        plot.subtitle = element_markdown(
          size = 12, color = col_sub, lineheight = 1.4,
          margin = margin(b = 14), family = fonts$text
        ),
        plot.caption    = element_markdown(
          size = 8, color = col_sub, hjust = 0,
          margin = margin(t = 10), family = fonts$caption
        ),
        plot.margin = margin(16, 24, 12, 24)
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
 
# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-19
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr          1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2        4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue           1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readxl         1.4.5    2025-03-07 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales         1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble         3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
