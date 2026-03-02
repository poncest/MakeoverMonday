## Challenge: #MakeoverMonday 2026 week 09
## Data:      Trump's Approval Ratings
## Author:    Steven Ponce
## Date:      2026-03-02

## Article
# https://edition.cnn.com/2026/02/23/politics/trump-approval-rating-independents-cnn-poll

## Data
# https://data.world/makeovermonday/2026w9-trumps-approval-ratings

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,       
  janitor, patchwork   
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
df_raw <- readxl::read_xlsx("data/2026/MM 2026 W09 Trump Approval Ratings.xlsx") |>
  clean_names() |>
  rename(
    late_feb_2025 = x45689_0,   # Excel serial 45689 = Late February 2025
    feb_2026      = x46054_0    # Excel serial 46054 = February 2026
  )


## 3. EXAMINING THE DATA ----
glimpse(df_raw)


## 4. TIDY DATA ----

# Margin of error from CNN/SSRS poll footnote
MOE <- 0.086

df <- df_raw |>
  mutate(
    category = case_when(
      group == "All Adults" ~ "Overall",
      group %in% c("Men", "Women") ~ "Gender",
      str_starts(group, "Age") ~ "Age",
      group %in% c("Latino Americans", "White Americans", "Black Americans") ~ "Race/Ethnicity",
      group %in% c("Independents", "Republicans", "Democrats") ~ "Party"
    ),
    category = factor(category,
      levels = c(
        "Overall", "Gender", "Age",
        "Race/Ethnicity", "Party"
      )
    ),
    sig_change = abs(net_percent_pt_change) >= MOE,
    is_independents = group == "Independents",
    is_all_adults = group == "All Adults"
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    primary       = "#1E3A5F",   
    accent        = "#C05C2E",  
    highlight     = "#E8732A",  
    neutral       = "#6B8CAE",  
    neutral_light = "#B8CCE0",   
    gray_dark     = "#444444",   
    gray_mid      = "#888888",
    gray_light    = "#CCCCCC"
  )
)

### |- titles and caption ----
title_text <- str_glue("Trump's Approval Ratings: Declines and Current Standing")

subtitle_text <- str_glue(
  "Left: net change with MoE (\u00b18.6 pp) \u2014 ",
  "<span style='color:{colors$palette$accent}'>**significant**</span> vs. ",
  "<span style='color:{colors$palette$gray_mid}'>**within MoE**</span> | ",
  "Right: Feb 2026 approval ",
  "(<span style='color:{colors$palette$highlight}'>**Independents**</span>, ",
  "<span style='color:{colors$palette$accent}'>**All Adults**</span> highlighted)"
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 9,
  source_text = "CNN/SSRS poll, Feb 17\u201320, 2026 (n=2,496)<br>MoE: \u00b18.6 pp"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.ticks         = element_blank(),
    # axis.text.y handled per-panel (p_left uses selective bold; p_right hides it)
    axis.text.x        = element_text(size = 9,   color = colors$palette$gray_mid),
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10), family = fonts$subtitle,
      color = "gray40"
    ),
    plot.title = element_text(
      size = rel(1.3), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 3)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 8)
    ),
  )
)

theme_set(weekly_theme)

### |- LEFT PANEL: Who Dropped Most? ----

# All Adults change value for reference line annotation
all_adults_change <- df |>
  filter(is_all_adults) |>
  pull(net_percent_pt_change)

# Selective bold for Independents and All Adults y-axis labels
group_order <- df |>
  arrange(net_percent_pt_change) |>
  pull(group)

y_label_faces <- if_else(
  group_order %in% c("Independents", "All Adults"), "bold", "plain"
)

names(y_label_faces) <- group_order

df_left <- df |>
  mutate(
    group = fct_reorder(group, net_percent_pt_change),
    moe_lo = net_percent_pt_change - MOE,
    moe_hi = net_percent_pt_change + MOE,
    pt_color = case_when(
      is_independents ~ colors$palette$highlight,
      is_all_adults ~ colors$palette$accent,
      sig_change ~ colors$palette$neutral,
      TRUE ~ colors$palette$gray_mid # within MoE
    )
  )

p_left <- ggplot(df_left, aes(x = net_percent_pt_change, y = group)) +
  # Annotate
  annotate(
    "rect",
    xmin = -MOE, xmax = MOE,
    ymin = 0.4, ymax = 13.6,
    fill = colors$palette$gray_light, alpha = 0.18
  ) +
  annotate(
    "text",
    x = 0,
    y = 13.3,
    label = "Within MoE",
    size = 2.4, color = colors$palette$gray_mid,
    hjust = 0.5
  ) +
  # Geoms
  geom_vline(
    xintercept = 0,
    color = "#666666", linewidth = 0.4
  ) +
  geom_vline(
    xintercept = all_adults_change,
    linetype   = "dotted",
    color      = colors$palette$accent,
    linewidth  = 0.5
  ) +
  annotate(
    "text",
    x = all_adults_change - 0.002,
    y = 0.7,
    label = glue("All Adults:\n{round(all_adults_change * 100)} pp"),
    size = 2.3, color = colors$palette$accent,
    hjust = 1, lineheight = 0.9
  ) +
  geom_linerange(
    aes(xmin = moe_lo, xmax = moe_hi, color = pt_color),
    linewidth = 1.1, alpha = 0.45
  ) +
  geom_point(
    aes(color = pt_color),
    size = 4
  ) +
  geom_text(
    aes(
      label = glue("{round(net_percent_pt_change * 100)} pp"),
      color = pt_color
    ),
    nudge_y = 0.38,
    size = 2.7,
    fontface = "bold"
  ) +
  # Scales
  scale_color_identity() +
  scale_x_continuous(
    breaks = c(-0.30, -0.20, -0.10, 0),
    labels = c("-30 pp", "-20 pp", "-10 pp", "0"),
    limits = c(-0.34, 0.13)
  ) +
  scale_y_discrete(
    limits = rev,
    labels = function(x) x
  ) +
  # Labs
  labs(
    title    = "Who Dropped Most?",
    subtitle = "Net pp change | bars = \u00b18.6 pp margin of error",
    x        = "Net Change (percentage points)",
    y        = NULL
  ) +
  # Theme
  theme(
    axis.text.y = element_text(
      face  = rev(y_label_faces),
      size  = 9.5,
      color = colors$palette$gray_dark
    )
  )

### |- RIGHT PANEL: Who Approves Now? ----
df_right <- df |>
  mutate(
    group = fct_reorder(group, net_percent_pt_change),
    bar_color = case_when(
      is_all_adults ~ colors$palette$accent,
      is_independents ~ colors$palette$highlight,
      TRUE ~ colors$palette$primary
    )
  )

p_right <- ggplot(df_right, aes(x = feb_2026, y = group)) +
  # Geoms
  geom_col(aes(fill = bar_color), width = 0.65) +
  geom_text(
    aes(label = percent(feb_2026, 1)),
    hjust = 1.15,
    size = 2.7,
    color = "white",
    fontface = "bold"
  ) +
  # Scales
  scale_fill_identity() +
  scale_x_continuous(
    labels = percent_format(1),
    limits = c(0, 1.0),
    breaks = c(0, 0.25, 0.50, 0.75, 1.0)
  ) +
  scale_y_discrete(limits = rev) +
  # Labs
  labs(
    title    = "Who Approves Now?",
    subtitle = "Feb 2026 approval | ordered by magnitude of decline",
    x        = "Approval Rating",
    y        = NULL
  ) +
  # Theme
  theme(axis.text.y = element_blank())

### |- COMBINE WITH PATCHWORK ----
p_left + p_right +
  plot_layout(widths = c(1.1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.6),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 0, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.85),
        family = fonts$subtitle,
        face = "italic",
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.5,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.5),
        family = fonts$subtitle,
        color = colors$caption,
        hjust = 0,
        lineheight = 1.4,
        margin = margin(t = 20, b = 5)
      ),
      plot.margin = margin(15, 15, 10, 15)
    )
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

# ─ Session info ───────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-03-02
# rstudio  2026.01.1+403 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# V base         * 4.4.1    2024-04-24 [1] local (on disk 4.4.0)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.4.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.4.0)
# cli            3.6.5    2025-04-23 [1] CRAN (R 4.4.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.4.3)
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
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.4.0)
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
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.4.3)
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
# ──────────────────────────────────────────────────────────────────────────
# > 