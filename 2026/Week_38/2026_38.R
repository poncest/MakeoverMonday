## Challenge: #MakeoverMonday 2026 week 38
## Data:      CFB Roster Spending 2026

## Author:    Steven Ponce
## Date:      2026-09-21

## Article
# https://www.linkedin.com/posts/ryan-sagers_the-athletic-just-dropped-a-super-interesting-share-7506027124345126912-54uU/?utm_source=share&utm_medium=member_ios&rcm=ACoAAATRm80BRDol2MPefSrDOJ6QtDfL0kI_zAs

## Data
# https://www.nytimes.com/athletic/interactive/college-football-nil-spending-budgets/?unlocked_article_code=1.BlE.1bJL.q6tmjvQfrAdf&source=twitterhq

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 12
fig_h <- 8

## 2. READ IN THE DATA ----
df_raw <- read_csv(
  "data/2026/cfb_roster_budgets_2026.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- reference line ----
# Median of all 68 midpoints (Notre Dame included in the benchmark)
nat_median <- median(df_raw$est_mid_musd)

### |- classify each estimated range against the line ----
# Uses the WHOLE range, not the midpoint:
#   above = low end is above the line
#   below = high end is below the line
#   straddles = the line falls inside the range
conf_levels <- c("SEC", "Big Ten", "ACC", "Big 12")

plot_data <- df_raw |>
  filter(conference != "Independent") |>
  mutate(
    state = case_when(
      est_low_musd > nat_median ~ "above",
      est_high_musd < nat_median ~ "below",
      .default = "straddles"
    ),
    state = factor(state, levels = c("above", "straddles", "below")),
    conference = factor(conference, levels = conf_levels)
  ) |>
  arrange(conference, desc(est_mid_musd), desc(est_high_musd), team) |>
  mutate(row = row_number(), .by = conference)

panel_stats <- plot_data |>
  summarise(
    n = n(),
    n_above = sum(state == "above"),
    n_below = sum(state == "below"),
    .by = conference
  ) |>
  arrange(conference) |>
  mutate(count_label = if_else(
    row_number() == 1,
    paste0(n_above, " of ", n, " entirely above"),
    paste0(n_above, " of ", n)
  ))

nd <- df_raw |> filter(conference == "Independent")


## 5. VISUALIZATION ----

### |- plot aesthetics ----
col_above <- "#722F37"
col_straddle <- "#767676"
col_below <- "#9C9C9C"
col_ink <- "#2B2B2B"

colors <- get_theme_colors(
  palette = list(
    primary      = col_above,
    neutral_dark = col_straddle,
    neutral_mid  = col_below
  )
)
clrs <- colors$palette

### |- layout constants (data units) ----
x_names <- 6.5
x_head <- -21.5
x_lim <- c(-22, 57)
y_lim <- c(19, -2.6)

### |- titles and caption ----
title_text <- "SEC Roster Budgets Run Deeper Than Any Other Power Conference's" 

subtitle_text <- str_glue(
  "Estimated 2026 roster-budget ranges for 67 programs. Nearly every SEC range sits ",
  "<span style='color:{col_above}'><b>entirely above</b></span> ",
  "the ${nat_median}M median estimate;<br>in the Big 12, almost none does."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 38,
  source_text = "The Athletic (estimated 2026 roster budgets: revenue sharing + third-party NIL)"
)

note_text <- str_glue(
  "Each line is a program's estimated low-high range (The Athletic's estimate, not a confidence interval). ",
  "Programs are sorted by midpoint within conference; where ranges overlap, the order of neighbors is not meaningful. ",
  "Median estimate = median of all 68 programs' midpoints. ",
  "{nd$team} (independent, {nd$budget_label}) is not shown."
)

caption_full <- str_glue("{note_text}<br><br>{caption_text}")


### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.title = element_textbox_simple(
      family = fonts$title_1, face = "bold", size = rel(1.7), colour = col_ink,
      width = unit(1, "npc"), margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$text, size = rel(0.85), colour = "grey30",
      width = unit(1, "npc"), lineheight = 1.15, margin = margin(b = 12)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$text, size = rel(0.65), colour = "grey40",
      width = unit(1, "npc"), lineheight = 1.2, margin = margin(t = 12)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(family = fonts$text, size = rel(0.7), colour = "grey45"),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0.6, "cm"),
    legend.position = "top",
    legend.justification = "left",
    legend.location = "plot",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.key.spacing.x = unit(0.7, "cm"),
    legend.text = element_text(family = fonts$text, size = rel(0.6), colour = "grey30"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(b = 6),
    plot.margin = margin(15, 20, 10, 20)
  )
)

theme_set(weekly_theme)

### |- reference-line label + line span ----
median_label <- tibble(
  conference = factor("SEC", levels = conf_levels),
  x = nat_median + 0.8,
  y = 17.3,
  label = paste0("Median estimate\n$", nat_median, "M")
)

# One segment per panel
median_line <- tibble(conference = factor(conf_levels, levels = conf_levels))

### |- plot ----
p <- ggplot(plot_data) +
  geom_segment(
    data = \(d) filter(d, est_low_musd - 0.7 > x_names + 0.5),
    aes(x = x_names + 0.5, xend = est_low_musd - 0.7, y = row, yend = row),
    colour = "grey90", linewidth = 0.25, linetype = "dotted"
  ) +
  geom_segment(
    data = median_line,
    aes(x = nat_median, xend = nat_median, y = 0.3, yend = 18.8),
    inherit.aes = FALSE, colour = col_ink, linewidth = 0.4
  ) +
  geom_segment(
    aes(x = est_low_musd, xend = est_high_musd, y = row, yend = row, colour = state),
    linewidth = 2.6, lineend = "butt"
  ) +
  geom_text(
    aes(x = x_names, y = row, label = team),
    hjust = 1, size = 2.7, family = fonts$text, colour = col_ink
  ) +
  geom_text(
    data = panel_stats,
    aes(x = x_head, y = -2, label = conference),
    hjust = 0, size = 3.8, fontface = "bold", family = fonts$text, colour = col_ink
  ) +
  geom_text(
    data = panel_stats,
    aes(x = x_head, y = -0.8, label = count_label),
    hjust = 0, size = 2.9, family = fonts$text, colour = col_above
  ) +
  geom_text(
    data = median_label,
    aes(x = x, y = y, label = label),
    hjust = 0, size = 2.5, lineheight = 0.95, family = fonts$text, colour = "grey35"
  ) +
  scale_colour_manual(
    values = c(above = col_above, straddles = col_straddle, below = col_below),
    labels = c(
      above     = "Entire range above the median",
      straddles = "Range crosses the median",
      below     = "Entire range below the median"
    )
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2.6))) +
  scale_x_continuous(
    limits = x_lim, breaks = c(10, 30, 50),
    labels = \(x) str_c("$", x, "M"), expand = expansion(0)
  ) +
  scale_y_reverse(limits = y_lim, expand = expansion(0)) +
  facet_wrap(~conference, nrow = 1) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_full)

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")


### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_38", "2026_38.png"),
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

# ─ Session info ──────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-09-21
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
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
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
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
# vroom          1.7.1   2026-03-31 [1] CRAN (R 4.6.0)
# withr          3.0.3   2026-06-19 [1] CRAN (R 4.6.0)
# xfun           0.60    2026-07-09 [1] CRAN (R 4.6.1)
# xml2           1.6.0   2026-06-22 [1] CRAN (R 4.6.1)
# 
# [1] /Library/Frameworks/R.framework/Versions/4.6/Resources/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────────
# > 
