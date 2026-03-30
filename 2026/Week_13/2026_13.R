## Challenge: #MakeoverMonday 2026 week 13
## Data:      Why People Create AI "Workslop"
## Author:    Steven Ponce
## Date:      2026-03-30

## Article
# https://hbr.org/2026/01/why-people-create-ai-workslop-and-how-to-stop-it

## Data
# https://data.world/makeovermonday/2026w13-ai-workslop

## Original Source
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/regionalunemploymentbyagex02

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## Citation
# Niederhoffer, K., Robichaux, A., & Hancock, J.T. (2026).
# Why People Create AI "Workslop"—and How to Stop It.
# Harvard Business Review. https://hbr.org/2026/01/why-people-create-ai-workslop-and-how-to-stop-it
#
# BetterUp Labs & Stanford Social Media Lab. (2025).
# Workslop: The Hidden Cost of AI-Generated Busywork.
# Online survey of 1,150 full-time U.S. desk workers, September 2025.
# https://www.betterup.com/workslop


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
  width  = 13,
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
df_raw <- read_xlsx("data/2026/MM 2026W13.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- left panel: bin the distribution ----
df_bins <- df_raw |>
  mutate(
    workslop_num = suppressWarnings(as.numeric(workslop_pct)),
    bin = case_when(
      is.na(workslop_num) ~ NA_character_,
      workslop_num == 0 ~ "None",
      workslop_num <= 30 ~ "Occasional",
      workslop_num <= 60 ~ "Regular",
      workslop_num >= 70 ~ "Heavy"
    ),
    bin = factor(bin, levels = c("None", "Occasional", "Regular", "Heavy"))
  ) |>
  filter(!is.na(bin)) |>
  group_by(bin) |>
  summarise(pct = sum(respondent_pct), .groups = "drop") |>
  mutate(pct_rescaled = pct / sum(pct) * 100)

### |- right panel: cost cascade values (hand-encoded, BetterUp/Stanford) ----
# Source: BetterUp Labs + Stanford Social Media Lab, n=1,150, Sept 2025
# https://www.betterup.com/workslop
# Values: 2 hrs/incident; $186/employee/month; $9M/year for 10k-person org


## 5. VISUALIZATION ----

### |- Colors ----
colors <- get_theme_colors(
  palette = list(
    none       = "#B8C4C8",   
    occasional = "#B8922A",   
    regular    = "#C75B2A",   
    heavy      = "#6B1F14",  
    # Supporting
    background = "#F8F5F0",
    text_dark  = "#1A1A1A",
    text_mid   = "#4A4A4A",
    text_light = "#8A8A8A",
    divider    = "#D0C8BC",
    # Annotation accent
    anchor     = "#1A1A1A"
  )
)

# Named vector for stacked bar fill mapping
bin_colors <- c(
  "None"       = colors$palette$none,
  "Occasional" = colors$palette$occasional,
  "Regular"    = colors$palette$regular,
  "Heavy"      = colors$palette$heavy
)

### |- titles and caption ----
title_text    <- str_glue("Low-quality AI output is common —<br>and it scales into millions in hidden cost")

subtitle_text <- str_glue(
  "53% of AI-using workers admit sending at least some 'workslop': AI-generated content that looks complete but lacks substance.<br>",
  "Each incident costs ~2 hours to resolve. Individual behavior scales into organizational cost."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 13,
  source_text = "BetterUp Labs & Stanford Social Media Lab (n=1,150, Sept 2025)<br>
                 Niederhoffer, Robichaux & Hancock (2026), Harvard Business Review"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Panel
    plot.background  = element_rect(fill = colors$palette$background, color = NA),
    panel.background = element_rect(fill = colors$palette$background, color = NA),
    panel.grid       = element_blank(),
    axis.ticks       = element_blank(),
    # Text
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    # Margins
    plot.margin      = margin(8, 12, 8, 12)
  )
)

theme_set(weekly_theme)

### |- LEFT PANEL: Stacked composition bar ----

# Derived annotation values
sends_some_pct <- df_bins |>
  filter(bin != "None") |>
  summarise(total = sum(pct_rescaled)) |>
  pull(total) |>
  round(0)

none_pct <- df_bins |>
  filter(bin == "None") |>
  pull(pct_rescaled)

df_bins_pos <- df_bins |>
  arrange(desc(as.integer(bin))) |>
  mutate(
    x_end   = cumsum(pct_rescaled),
    x_start = x_end - pct_rescaled,
    x_mid   = (x_start + x_end) / 2
  )

p_left <- ggplot(df_bins, aes(x = pct_rescaled, y = "", fill = bin)) +
  geom_col(
    position  = position_stack(reverse = FALSE),
    width     = 0.42,
    color     = colors$palette$background,
    linewidth = 1.2
  ) +
  geom_text(
    data = df_bins |> filter(pct_rescaled >= 9),
    aes(label = glue("{round(pct_rescaled, 0)}%")),
    position = position_stack(vjust = 0.5, reverse = FALSE),
    color = "white",
    size = 4.5,
    fontface = "bold",
    family = fonts$text
  ) +
  geom_text(
    data = df_bins_pos,
    aes(x = x_mid, label = bin),
    y = 1.22,
    color = colors$palette$text_mid,
    size = 2.8,
    fontface = "plain",
    family = fonts$text,
    hjust = 0.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = sends_some_pct / 2,
    y = 1.58,
    label = glue("{sends_some_pct}% send at least\nsome workslop"),
    size = 3.6,
    color = colors$palette$text_dark,
    fontface = "bold",
    family = fonts$text,
    hjust = 0.5,
    lineheight = 1.2
  ) +
  annotate(
    "segment",
    x = 1, xend = sends_some_pct - 1,
    y = 1.44, yend = 1.44,
    color = colors$palette$text_mid, linewidth = 0.5
  ) +
  annotate("segment",
    x = 1, xend = 1,
    y = 1.40, yend = 1.44,
    color = colors$palette$text_mid, linewidth = 0.5
  ) +
  annotate("segment",
    x = sends_some_pct - 1, xend = sends_some_pct - 1,
    y = 1.40, yend = 1.44,
    color = colors$palette$text_mid, linewidth = 0.5
  ) +
  annotate(
    "text",
    x = 0, y = 0.58,
    label = "Excludes 17% of respondents who did not use AI at work",
    size = 2.3,
    color = colors$palette$text_light,
    hjust = 0,
    family = fonts$text,
    fontface = "italic"
  ) +
  scale_fill_manual(values = bin_colors) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.01)),
    limits = c(0, 100)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.55, 1.05))) +
  labs(
    title    = "Workforce workslop profile",
    subtitle = "Share of AI-using workers by amount of workslop sent"
  ) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(
      size   = 18,
      face   = "bold",
      color  = colors$palette$text_dark,
      family = fonts$title,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size   = 10,
      color  = colors$palette$text_mid,
      family = 'sans',
      margin = margin(b = 12)
    ),
    plot.margin = margin(12, 24, 12, 12)
  )


### |- RIGHT PANEL: Cost cascade ----

y_pos <- c(0.82, 0.50, 0.14)

cascade_sizes <- c(8.5, 13, 20)

p_right <- ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  annotate("text",
    x = 0.5, y = y_pos[1] + 0.09,
    label = "2 hours",
    size = cascade_sizes[1],
    fontface = "bold",
    color = colors$palette$text_dark,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[1] - 0.02,
    label = "PER INCIDENT",
    size = 2.5,
    color = colors$palette$text_light,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[1] - 0.08,
    label = "average time to resolve",
    size = 2.8,
    color = colors$palette$text_mid,
    family = fonts$text,
    hjust = 0.5,
    fontface = "italic"
  ) +
  annotate("segment",
    x = 0.15, xend = 0.85,
    y = (y_pos[1] + y_pos[2]) / 2,
    yend = (y_pos[1] + y_pos[2]) / 2,
    color = colors$palette$divider,
    linewidth = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[2] + 0.09,
    label = "$186",
    size = cascade_sizes[2],
    fontface = "bold",
    color = colors$palette$regular,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[2] - 0.02,
    label = "PER EMPLOYEE / MONTH",
    size = 2.5,
    color = colors$palette$text_light,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[2] - 0.08,
    label = "invisible productivity tax",
    size = 2.8,
    color = colors$palette$text_mid,
    family = fonts$text,
    hjust = 0.5,
    fontface = "italic"
  ) +
  annotate("segment",
    x = 0.15, xend = 0.85,
    y = (y_pos[2] + y_pos[3]) / 2,
    yend = (y_pos[2] + y_pos[3]) / 2,
    color = colors$palette$divider,
    linewidth = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[3] + 0.09,
    label = "$9 million",
    size = cascade_sizes[3],
    fontface = "bold",
    color = colors$palette$heavy,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[3] - 0.02,
    label = "PER YEAR (10K ORG)",
    size = 2.5,
    color = colors$palette$text_light,
    family = fonts$text,
    hjust = 0.5
  ) +
  annotate("text",
    x = 0.5, y = y_pos[3] - 0.08,
    label = "annual organizational cost",
    size = 2.8,
    color = colors$palette$text_mid,
    family = fonts$text,
    hjust = 0.5,
    fontface = "italic"
  ) +
  labs(
    title    = "The compounding cost",
    subtitle = "Individual behavior \u2192 organizational scale"
  ) +
  theme(
    plot.title = element_text(
      size   = 18,
      face   = "bold",
      color  = colors$palette$text_dark,
      family = fonts$title,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size   = 10,
      color  = colors$palette$text_mid,
      family = 'sans',
      margin = margin(b = 12)
    ),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(12, 12, 12, 24)
  )


### |- Combined Plots ----
combined_plots <- p_left + p_right +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size        = 30,
        face        = "bold",
        color       = colors$palette$text_dark,
        family      = fonts$title,
        lineheight  = 1.3,
        margin      = margin(b = 8)
      ),
      plot.subtitle = element_markdown(
        size        = 11,
        color       = colors$palette$text_mid,
        family      = fonts$text,
        lineheight  = 1.5,
        margin      = margin(b = 18)
      ),
      plot.caption = element_markdown(
        size        = 7,
        color       = colors$palette$text_light,
        family      = fonts$caption,
        linewidth   = 1.3,
        hjust       = 0,
        margin      = margin(t = 14)
      ),
      plot.background = element_rect(fill = colors$palette$background, color = NA),
      plot.margin = margin(20, 20, 14, 20)
    )
  )

### |- Preview ----
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
 
# ─ Session info ─────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-30
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.3.1    2023-06-16 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.3.1)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.3.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.3.3)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.3.1)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.3.1)
# P compiler       4.3.1    2023-06-16 [2] local
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.3.1)
# P datasets     * 4.3.1    2023-06-16 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.3.1)
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
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.3.1)
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
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.3.1)
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
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────