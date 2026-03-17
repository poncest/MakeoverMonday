## Challenge: #MakeoverMonday 2026 week 11
## Data:      Fastest Cars in the World (2026)
## Author:    Steven Ponce
## Date:      2026-03-17

## Article
# https://www.rankred.com/fastest-cars-in-the-world/

## Data
# https://data.world/makeovermonday/8-fastest-cars-in-the-world-as-of-2026

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, readxl, patchwork      
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 11,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_xlsx("data/2026/The fastest cars.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
df <- df_raw |>
  rename(
    top_speed_kmh = top_speed_km_h,
    hp            = horsepower_hp,
    torque        = torque_lb_ft,
    accel_sec     = x0_100_km_h_approximate_in_sec,
    units         = units_produced,
    price_per_hp  = price_per_hp_usd,
    speed_per_hp  = speed_per_hp_km_h_per_hp
  ) |>
  mutate(
    car_label = case_when(
      car == "Hennessey Venom F5"              ~ "Hennessey Venom F5",
      car == "Bugatti Chiron Super Sport 300+" ~ "Bugatti Chiron SS 300+",
      car == "SSC Tuatara"                     ~ "SSC Tuatara",
      car == "Koenigsegg Jesko"                ~ "Koenigsegg Jesko",
      car == "Koenigsegg Agera RS"             ~ "Koenigsegg Agera RS",
      car == "Hennessey Venom GT"              ~ "Hennessey Venom GT",
      car == "Bugatti Veyron 16.4 Super Sport" ~ "Bugatti Veyron SS",
      car == "Rimac C Two"                     ~ "Rimac C Two"
    ),
    car_short = case_when(
      car == "Hennessey Venom F5"              ~ "Venom F5",
      car == "Bugatti Chiron Super Sport 300+" ~ "Chiron SS 300+",
      car == "SSC Tuatara"                     ~ "Tuatara",
      car == "Koenigsegg Jesko"                ~ "Jesko",
      car == "Koenigsegg Agera RS"             ~ "Agera RS",
      car == "Hennessey Venom GT"              ~ "Venom GT",
      car == "Bugatti Veyron 16.4 Super Sport" ~ "Veyron SS",
      car == "Rimac C Two"                     ~ "C Two"
    )
  )


## 5. VISUALIZATION ----

### |- Colors ----
colors <- get_theme_colors(
  palette = list(
    burgundy      = "#6D1A36",
    steel         = "#4A6FA5",
    sweden        = "#6B9DC2",
    croatia       = "#3D6B6B",
    neutral_dark  = "#2B2B2B",
    neutral_mid   = "#6B6B6B",
    neutral_light = "#E5E5E5",
    grid_light    = "#D9D9D9",
    empty_tile    = "#F3F3F3"
  )
)

country_cols <- c(
  "USA"     = colors$palette$burgundy,
  "France"  = colors$palette$steel,
  "Sweden"  = colors$palette$sweden,
  "Croatia" = colors$palette$croatia
)

### |- Titles and caption ----
title_text <- "The World's Fastest Cars: Some Sell Engineering, Others Sell Exclusivity"

subtitle_text <- glue(
  "Top: deviation from the median in <b>cost per horsepower</b> and <b>speed delivered per horsepower</b>.<br>",
  "Bottom: the same cars by <b>units produced</b>. ",
  "<span style='color:{colors$palette$burgundy}'>Burgundy</span> signals worse value; ",
  "<span style='color:{colors$palette$steel}'>steel blue</span> signals better value."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 11,
  source_text = "RankRed.com · data.world/makeovermonday"
)


### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.x = element_line(color = colors$palette$grid_light, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.ticks         = element_blank(),
    axis.text.x        = element_text(size = 9, color = colors$palette$neutral_mid),
    axis.text.y        = element_text(size = 10, color = colors$palette$neutral_mid),
    axis.title         = element_blank(),
    plot.title = element_text(
      size = rel(1.25), family = fonts$title, face = "bold",
      color = colors$palette$neutral_dark, lineheight = 1.05,
      margin = margin(t = 3, b = 4)
    ),
    plot.subtitle = element_text(
      size = rel(0.8), family = fonts$subtitle,
      color = colors$palette$neutral_mid, lineheight = 1.1,
      margin = margin(t = 0, b = 8)
    )
  )
)

theme_set(weekly_theme)

### |- PANEL A: Diverging Bars — Luxury Premium vs. Engineering Efficiency ----

# Derived Data ----
med_price_hp <- median(df$price_per_hp)
med_speed_hp <- median(df$speed_per_hp)

plot_df <- df |>
  mutate(
    price_hp_dev = price_per_hp - med_price_hp,
    speed_hp_dev = speed_per_hp - med_speed_hp,
    cost_flag    = if_else(price_hp_dev > 0, "Above median cost", "Below median cost"),
    eff_flag     = if_else(speed_hp_dev > 0, "Above median efficiency", "Below median efficiency")
  ) |>
  arrange(desc(price_hp_dev)) |>
  mutate(
    car_order = factor(car_short, levels = rev(car_short))
  )

### |-  left panel: cost per HP ----
p_cost <- plot_df |>
  ggplot(aes(x = price_hp_dev, y = car_order, fill = cost_flag)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, linewidth = 0.6, color = colors$palette$neutral_dark) +
  geom_text(
    aes(
      label = if_else(price_hp_dev > 0,
                      paste0("+", dollar(round(price_hp_dev))),
                      dollar(round(price_hp_dev))),
      x = if_else(price_hp_dev > 0, price_hp_dev + 28, price_hp_dev - 28)
    ),
    size = 3,
    color = colors$palette$neutral_dark,
    fontface = "bold",
    hjust = if_else(plot_df$price_hp_dev > 0, 0, 1)
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0.55, yend = 1.15,
    color = colors$palette$neutral_mid,
    linewidth = 0.35,
    linetype = 2
  ) +
  annotate(
    "text",
    x = 20, y = 0.95,
    label = glue("median = {dollar(round(med_price_hp))}/HP"),
    hjust = 0, size = 2.7,
    color = colors$palette$neutral_mid,
    fontface = "italic"
  ) +
  scale_fill_manual(
    values = c(
      "Above median cost" = colors$palette$burgundy,
      "Below median cost" = colors$palette$steel
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_dollar(),
    limits = c(-520, 1325),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Cost per horsepower",
    subtitle = "Deviation from median ($/HP) · sorted most to least expensive"
  ) +
  theme(
    plot.title    = element_text(size = rel(1.0), face = "bold"),
    plot.subtitle = element_text(size = rel(0.76), margin = margin(b = 8)),
    axis.text.y   = element_text(color = colors$palette$neutral_mid)
  )

### |-  right panel: speed per HP ----
p_speed <- plot_df |>
  ggplot(aes(x = speed_hp_dev, y = car_order, fill = eff_flag)) +
  geom_col(width = 0.62) +
  geom_vline(xintercept = 0, linewidth = 0.6, color = colors$palette$neutral_dark) +
  geom_text(
    aes(
      label = if_else(speed_hp_dev > 0,
                      paste0("+", number(speed_hp_dev, accuracy = 0.001)),
                      number(speed_hp_dev, accuracy = 0.001)),
      x = if_else(speed_hp_dev > 0, speed_hp_dev + 0.004, speed_hp_dev - 0.004)
    ),
    size = 3,
    color = colors$palette$neutral_dark,
    fontface = "bold",
    hjust = if_else(plot_df$speed_hp_dev > 0, 0, 1)
  ) +
  annotate(
    "segment",
    x = 0, xend = 0,
    y = 0.55, yend = 1.15,
    color = colors$palette$neutral_mid,
    linewidth = 0.35,
    linetype = 2
  ) +
  annotate(
    "text",
    x = 0.002, y = 0.95,
    label = glue("median = {number(med_speed_hp, accuracy = 0.001)} km/h/HP"),
    hjust = 0, size = 2.7,
    color = colors$palette$neutral_mid,
    fontface = "italic"
  ) +
  scale_fill_manual(
    values = c(
      "Above median efficiency" = colors$palette$steel,
      "Below median efficiency" = colors$palette$burgundy
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.01),
    limits = c(-0.10, 0.07),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Speed per horsepower",
    subtitle = "Deviation from median (km/h per HP) · same car order as left panel"
  ) +
  theme(
    plot.title    = element_text(size = rel(1.0), face = "bold"),
    plot.subtitle = element_text(size = rel(0.76), margin = margin(b = 8)),
    axis.text.y   = element_blank()
  )

### |- PANEL B: Unit Chart — How Many Were Ever Built? ----

# Derived Data ----
rarity_base <- df |>
  transmute(
    car_short,
    units,
    country,
    facet_label = glue("{car_short}\n{units} units")
  ) |>
  arrange(desc(units)) |>
  mutate(
    facet_label = factor(facet_label, levels = facet_label)
  )

max_units <- max(rarity_base$units)
n_cols    <- 15
n_rows    <- ceiling(max_units / n_cols)

rarity_full <- rarity_base |>
  rowwise() |>
  reframe(
    car_short   = car_short,
    facet_label = facet_label,
    country     = country,
    units       = units,
    unit_id     = seq_len(n_cols * n_rows)
  ) |>
  mutate(
    filled = unit_id <= units,
    col    = (unit_id - 1) %% n_cols + 1,
    row    = (unit_id - 1) %/% n_cols + 1,
    fill_key = if_else(filled, country, "empty")
  )

rarity_cols <- c(country_cols, empty = colors$palette$empty_tile)

### |-  unit chart ----
p_rarity <- rarity_full |>
  ggplot(aes(x = col, y = -row, fill = fill_key)) +
  geom_tile(width = 0.8, height = 0.8, color = "white", linewidth = 0.45) +
  facet_wrap(~facet_label, ncol = 4) +
  scale_fill_manual(
    values = rarity_cols,
    breaks = c("USA", "France", "Sweden", "Croatia"),
    name   = "Country"
  ) +
  labs(
    title = "How many were ever built?",
    subtitle = "Each square represents one car. Common empty grid makes rarity comparable across models."
  ) +
  coord_equal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(
      size = rel(0.8), face = "bold",
      color = colors$palette$neutral_dark,
      lineheight = 1.3
    ),
    panel.spacing = unit(1.1, "lines"),
    legend.position = "bottom",
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.75)),
    legend.key.size = unit(0.4, "cm"),
  )

### |-  Combined Plots ----
p_top <- p_cost + p_speed + plot_layout(widths = c(1.15, 1))

final_plot <- p_top / p_rarity +
  plot_layout(heights = c(1.05, 1)) +
  plot_annotation(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.42), face = "bold",
        color = colors$palette$neutral_dark,
        margin = margin(b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.88),
        color = colors$palette$neutral_mid,
        lineheight = 1.15,
        margin = margin(b = 12)
      ),
      plot.caption = element_markdown(
        size = rel(0.70),
        color = colors$palette$neutral_mid,
        hjust = 0
      ),
      plot.margin = margin(t = 14, r = 16, b = 12, l = 16)
    )
  )

snap(final_plot)


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

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-16
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/RtmpM9t8Em/file79384bbc1f7f". Did you mean command "install"? @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe
# 
# ─ Packages ────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
# cli            3.6.4    2025-02-13 [1] CRAN (R 4.3.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.37   2024-08-19 [1] CRAN (R 4.3.3)
# dplyr        * 1.2.0    2026-02-03 [1] CRAN (R 4.3.1)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.3.1)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.3.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.3.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.3.1)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.3.1)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.3.1)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.3.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.3.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.3.3)
# P graphics     * 4.3.1    2023-06-16 [2] local
# P grDevices    * 4.3.1    2023-06-16 [2] local
# P grid           4.3.1    2023-06-16 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.3.1)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.3.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.3.1)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.3.1)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.3.1)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.3.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.3.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.3.1)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.3.1)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.3.1)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.3.1)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.3.1)
# magick         2.8.6    2025-03-23 [1] CRAN (R 4.3.3)
# magrittr       2.0.3    2022-03-30 [1] CRAN (R 4.3.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.3.3)
# P methods      * 4.3.1    2023-06-16 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.3.1)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.3.3)
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.3.1)
# pillar         1.10.2   2025-04-05 [1] CRAN (R 4.3.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.3.3)
# purrr        * 1.2.1    2026-01-09 [1] CRAN (R 4.3.1)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.3.3)
# ragg           1.5.0    2025-09-02 [1] CRAN (R 4.3.1)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.3.1)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.3.1)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.3.1)
# readxl       * 1.4.5    2025-03-07 [1] CRAN (R 4.3.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.3.3)
# rlang          1.1.7    2026-01-09 [1] CRAN (R 4.3.1)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.3.1)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.3.1)
# rsvg           2.6.2    2025-03-23 [1] CRAN (R 4.3.3)
# S7             0.2.0    2024-11-07 [1] CRAN (R 4.3.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.3.1)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.3.3)
# showtext     * 0.9-7    2024-03-02 [1] CRAN (R 4.3.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.3.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.3.1)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.3.3)
# P stats        * 4.3.1    2023-06-16 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.3.3)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.3.1)
# svglite        2.1.3    2023-12-08 [1] CRAN (R 4.3.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.3.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.3.1)
# textshaping    1.0.4    2025-10-10 [1] CRAN (R 4.3.1)
# tibble       * 3.2.1    2023-03-20 [1] CRAN (R 4.3.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.3.1)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.3.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.3.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.3.1)
# P tools          4.3.1    2023-06-16 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.3.3)
# utf8           1.2.4    2023-10-22 [1] CRAN (R 4.3.3)
# P utils        * 4.3.1    2023-06-16 [2] local
# vctrs          0.7.1    2026-01-23 [1] CRAN (R 4.3.1)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.3.3)
# xfun           0.56     2026-01-18 [1] CRAN (R 4.3.1)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.3.1)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.3
# [2] C:/Program Files/R/R-4.3.1/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────
