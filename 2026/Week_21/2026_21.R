## Challenge: #MakeoverMonday 2026 week 21
## Data:      GPU computational performance per dollar
## Author:    Steven Ponce
## Date:      2026-05-25

## Article
# https://ourworldindata.org/grapher/gpu-price-performance

## Data
# https://data.world/makeovermonday/gpu-computational-performance-per-dollar

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_csv("data/2026/GPU performance per dollar.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |-  parse and rename ----
df <- df_raw |>
  rename(
    gpu_name = entity,
    release_date = day,
    flops_per_dollar = gpu_performance_per_dollar
  ) |>
  arrange(release_date)

### |-  frontier: rolling maximum FLOP/s/$ over time ----
# Frontier = best available price-performance at each release date
# with_ties = FALSE prevents non-deterministic multi-row returns

df_frontier <- df |>
  arrange(release_date) |>
  mutate(
    frontier_max = cummax(flops_per_dollar),
    on_frontier = flops_per_dollar == frontier_max
  ) |>
  filter(on_frontier)

### |-  pre-extract label coordinates from df ----
gtx280_x  <- df |> filter(gpu_name == "NVIDIA GeForce GTX 280")  |> pull(release_date)
gtx280_y <- df |> filter(gpu_name == "NVIDIA GeForce GTX 280")  |> pull(flops_per_dollar)
rtx3090_x <- df |> filter(gpu_name == "NVIDIA GeForce RTX 3090") |> pull(release_date)
rtx3090_y <- df |> filter(gpu_name == "NVIDIA GeForce RTX 3090") |> pull(flops_per_dollar)
amd_x <- df |> filter(gpu_name == "AMD Radeon RX 7900 XTX")  |> pull(release_date)
amd_y <- df |> filter(gpu_name == "AMD Radeon RX 7900 XTX")  |> pull(flops_per_dollar)

### |-  AMD peak point (drawn separately with accent color) ----
df_amd_peak <- df |> filter(gpu_name == "AMD Radeon RX 7900 XTX")

### |-  AI milestone bands ----

# Four milestones
# for a societal acceleration story where ChatGPT is the public inflection)
# Band width = 120 days
milestones <- tibble(
  label = c("ALEXNET", "TRANSFORMER", "GPT-2", "CHATGPT"),
  date = as.Date(c("2012-09-30", "2017-06-12", "2019-02-14", "2022-11-30")),
  date_end = as.Date(c("2012-09-30", "2017-06-12", "2019-02-14", "2022-11-30")) + 120
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    col_cloud = "#A8B4BC",
    col_frontier = "#1B4F72",
    col_peak = "#9C2B1B",
    col_peak_label = "#7F2418",
    col_milestone = "#D6C79A",
    col_label = "#1B4F72"
  )
)

# Unpack  — avoids $palette$ repetition in plot code
col_cloud <- clrs$palette$col_cloud
col_frontier <- clrs$palette$col_frontier
col_peak <- clrs$palette$col_peak
col_peak_label <- clrs$palette$col_peak_label
col_milestone <- clrs$palette$col_milestone
col_label <- clrs$palette$col_label

### |-  titles and caption ----
title_text <- str_glue("A Dollar Buys Far More AI Computation Than a Decade Ago")

subtitle_text <- str_glue(
  "GPU performance per dollar improved gradually for a decade.\n",
  "Then the AI era sharply accelerated the trajectory."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 21,
  source_text = "Epoch AI (2026); U.S. Bureau of Labor Statistics (2026) |
  Note: FLOP/s at 32-bit precision. Constant 2024 US$."
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Title / subtitle
    plot.title.position = "plot",
    plot.title = element_text(size = 24, face = "bold", family = fonts$title_1, 
                              color = clrs$title, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12, family = fonts$subtitle, lineheight = 1.2, 
                                 color = clrs$subtitle, margin = margin(b = 30)),

    # Caption
    plot.caption = element_markdown(
      size = 7, hjust = 0, lineheight = 1.4, family = fonts$caption,
      margin = margin(t = 12), color = clrs$caption
    ),

    # Axes
    axis.title.x = element_text(size = 10, margin = margin(t = 8)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.ticks = element_blank(),

    # Grid — y-lines only
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    # Top margin 
    plot.margin = margin(t = 32, r = 30, b = 10, l = 20)
  )
)

theme_set(weekly_theme)

### |-  y-axis label helper ----
flops_labels <- function(x) {
  case_when(
    x >= 1e9 ~ paste0(round(x / 1e9), "B"),
    x >= 1e6 ~ paste0(round(x / 1e6), "M"),
    TRUE ~ scales::comma(x)
  )
}

### |-  main plot ----
p <- ggplot() +
  # Geoms
  geom_rect(
    data = milestones,
    aes(xmin = date, xmax = date_end),
    ymin = -Inf,
    ymax = Inf,
    fill = col_milestone,
    alpha = 0.55,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = milestones,
    aes(x = date + 60, y = Inf, label = label),
    vjust = -0.5,
    hjust = 0,
    size = 2.4,
    color = "gray45",
    fontface = "plain",
    inherit.aes = FALSE
  ) +
  geom_point(
    data = df,
    aes(x = release_date, y = flops_per_dollar),
    color = col_cloud,
    size = 2,
    alpha = 0.55
  ) +
  geom_line(
    data = df_frontier,
    aes(x = release_date, y = flops_per_dollar),
    color = col_frontier,
    linewidth = 0.78,
    lineend = "round"
  ) +
  geom_point(
    data = df_frontier,
    aes(x = release_date, y = flops_per_dollar),
    color = col_frontier,
    size = 2.8
  ) +
  geom_point(
    data = df_amd_peak,
    aes(x = release_date, y = flops_per_dollar),
    color = col_peak,
    size = 3.8
  ) +
  # Annotate
  annotate(
    "text",
    x = gtx280_x - 150,
    y = gtx280_y,
    label = "GTX 280\n(2008 baseline)",
    hjust = 1,
    vjust = 0.5,
    size = 2.8,
    color = col_label,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = rtx3090_x - 180,
    y = rtx3090_y * 1.95,
    label = "RTX 3090",
    hjust = 1,
    vjust = 0,
    size = 2.8,
    color = col_label,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = amd_x - 150,
    y = amd_y * 1.2,
    label = "AMD RX 7900 XTX",
    hjust = 1,
    vjust = 0,
    size = 2.8,
    color = col_peak_label,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = as.Date("2008-06-01"),
    y = 6e9,
    label = "Each horizontal band\nrepresents 10× more\ncompute per dollar",
    hjust = 0,
    vjust = 0,
    size = 2.6,
    color = "gray40",
    lineheight = 1.3,
    fontface = "italic"
  ) +
  # Scales
  scale_y_log10(
    labels = flops_labels,
    breaks = c(1e8, 1e9, 1e10, 1e11)
  ) +
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.18, 0.05))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "GPU Release Date",
    y = NULL
  )

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
 
# ─ Session info ───────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-05-25
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
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
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
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
# ──────────────────────────────────────────────────────