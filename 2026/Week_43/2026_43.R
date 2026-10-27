## Challenge: #MakeoverMonday 2026 week 43
## Data:      Big's Backyard Ultramarathon World Team Championships

## Author:    Steven Ponce
## Date:      2026-10-27

## Article
# https://wtc2026.bigsbackyardultra.com

## Data
# https://wtc2026.bigsbackyardultra.com

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview, readxl, ggrepel, magick 
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 10
fig_h <- 15
fig_dpi <- 320

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
text_family  <- fonts$text
cap_family   <- fonts$caption


## 2. READ IN THE DATA ----
xl_path <- here::here("data/2026/bigs_backyard_wtc_2020_2026.xlsx")

read_sheet <- function(sheet_name) {
  read_excel(xl_path, sheet = sheet_name,
             col_types = "text", na = character()) |>
    clean_names()
}

df_runners <- read_sheet("runner_results")
df_nations <- read_sheet("nation_years")


## 3. EXAMINING THE DATA ----
glimpse(df_runners)
glimpse(df_nations)
df_nations |> filter(year == "2026") |> count(result_status)


## 4. TIDY DATA ----

### |- standings: places rebuilt from team yards (site rule: level = shared place) ----
# The workbook's `rank` breaks 2026 ties by race distance; the site shares the
# place with no tie-break, and the site is the record. `rank` is never used.
class_levels <- c("SUPER" = "Super Six", "AAA" = "AAA", "AA" = "AA", "A" = "A")

standings <- df_nations |>
  filter(year == "2026", result_status %in% c("Final", "Cancelled")) |>
  mutate(
    team_yards    = as.numeric(team_yards),
    overall_place = min_rank(-team_yards),
    cancelled     = result_status == "Cancelled",
    division      = factor(class_levels[division_2026], levels = class_levels)
  ) |>
  mutate(
    place = min_rank(-team_yards),
    tied = duplicated(team_yards) | duplicated(team_yards, fromLast = TRUE),
    place_lab = if_else(tied, paste0("=", place), as.character(place)),
    champ = place == 1,
    .by = division
  ) |>
  mutate(
    nation_short = recode(nation, "United States of America" = "United States"),
    nation_lab   = if_else(cancelled, paste0(nation_short, " (race cancelled)"), nation_short)
  )

### |- runner profiles: starters only, sorted farthest to shortest ----
# DNS runners are left out, so a short team shows blank positions, not zeros
profiles <- df_runners |>
  filter(
    year == "2026", nation_code %in% standings$nation_code,
    result != "DNS"
  ) |>
  mutate(yards = as.numeric(yards)) |>
  arrange(nation_code, desc(yards)) |>
  mutate(pos = row_number(), .by = nation_code)

max_y <- max(profiles$yards)

# Each profile sums to its official team total (no 2026 nation is off by one)
prof_check <- profiles |>
  summarise(sum_y = sum(yards), .by = nation_code) |>
  left_join(standings |> select(nation_code, team_yards), by = "nation_code")

### |- layout: one numeric y, gaps between classes ----
gap <- 1.6

rows <- standings |>
  arrange(division, place, nation) |>
  mutate(
    seq   = row_number(),
    div_i = as.integer(division),
    y     = -(seq + (div_i - 1) * gap)
  )

class_lab <- rows |>
  summarise(y = max(y), .by = division) |>
  mutate(label = as.character(division))

dividers <- rows |>
  summarise(y_min = min(y), .by = div_i) |>
  filter(div_i < 4) |>
  mutate(y = y_min - (1 + gap) / 2)

# column positions (data units)
# class labels (Big Shoulders, ~10 units wide for "Super Six") get their own
# column; place starts after it so rank 1 is never covered
x_class <- 0
x_place <- 12.5
x_nation <- 13.6
x_bar0 <- 28
bar_len <- 26
x_prof0 <- 59
col_w <- 1.8
x_prof1 <- x_prof0 + 15 * col_w
x_note <- x_prof1 + 0.8
x_max <- 100
y_head <- 0.2

bars <- rows |>
  mutate(xmax = x_bar0 + bar_len * team_yards / max(team_yards))

prof_plot <- profiles |>
  inner_join(rows |> select(nation_code, y), by = "nation_code") |>
  mutate(
    xmin = x_prof0 + (pos - 1) * col_w,
    xmax = xmin + col_w,
    ymin = y - 0.42,
    ymax = y - 0.42 + 0.84 * yards / max_y
  )

bel_note <- rows |>
  filter(nation_code == "BEL") |>
  transmute(y, label = "No national winner:\n2 runners tied at 124")


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    paper   = "#f4f1ea",
    ink     = "#2b2b2b",
    muted   = "#6f6a63",
    accent  = "#722F37",
    bar     = "#b9b3aa",
    profile = "#a9a297",
    rule    = "#cfc8be"
  )
)
clrs <- colors$palette
col_paper <- clrs$paper
col_ink <- clrs$ink
col_muted <- clrs$muted
col_accent <- clrs$accent
col_bar <- clrs$bar
col_profile <- clrs$profile
col_rule <- clrs$rule

### |- titles and caption ----
champs <- rows |>
  filter(champ) |>
  arrange(division)

title_text <- "Four classes, four champions"

subtitle_text <- paste0(
  "Final 2026 standings, placed within each class. Class champions in burgundy.\n",
  "A team scores every yard its runners complete; nations level on yards share a place."
)

note_text <- paste(
  "Final 2026 results, pulled 23 Oct 2026 after the last yard.",
  "The United States also won the overall team title (1,120 yards).",
  "Classes assigned before the race by Gary Cantrell, grouping teams of similar strength.",
  "Spain's race was cancelled for weather",
  "during yard 25; its yards count. Mauritius and Singapore did not start.",
  "AIN: Individual Neutral Athletes."
)

caption_text <- paste0(
  note_text, "<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 43,
    source_text = "Big's Backyard Ultra World Team Championship (wtc2026.bigsbackyardultra.com)"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_paper, colour = NA),
    panel.background = element_rect(fill = col_paper, colour = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      family = title_family, size = rel(2.6), colour = col_ink,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.95), colour = col_muted,
      lineheight = 1.3, margin = margin(b = 14)
    ),
    plot.caption = element_textbox_simple(
      family = cap_family, size = rel(0.5), colour = col_muted,
      lineheight = 1.3, margin = margin(t = 12)
    ),
    plot.margin = margin(20, 20, 15, 20)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot() +
  geom_segment(
    data = dividers,
    aes(x = x_class, xend = x_max, y = y, yend = y),
    colour = col_rule, linewidth = 0.3, linetype = "dotted"
  ) +
  annotate(
    "text",
    x = c(x_place, x_nation, x_bar0, x_prof0),
    y = y_head,
    label = c("CLASS PLACE", "NATION", "TEAM YARDS", "EACH RUNNER'S YARDS, FARTHEST FIRST"),
    hjust = c(1, 0, 0, 0),
    family = cap_family, size = 2.3, colour = col_muted
  ) +
  geom_text(
    data = class_lab,
    aes(x = x_class, y = y, label = label),
    hjust = 0, vjust = 0.5,
    family = title_family, size = 6, colour = col_ink
  ) +
  geom_text(
    data = rows,
    aes(x = x_place, y = y, label = place_lab),
    hjust = 1, family = text_family, size = 2.6, colour = col_muted
  ) +
  geom_text(
    data = rows,
    aes(
      x = x_nation, y = y, label = nation_lab,
      fontface = if_else(champ, "bold", "plain")
    ),
    hjust = 0, family = text_family, size = 2.6, colour = col_ink
  ) +
  geom_rect(
    data = bars,
    aes(
      xmin = x_bar0, xmax = xmax, ymin = y - 0.32, ymax = y + 0.32,
      fill = if_else(champ, col_accent, col_bar)
    )
  ) +
  geom_text(
    data = bars,
    aes(
      x = xmax + 0.6, y = y, label = comma(team_yards),
      fontface = if_else(champ, "bold", "plain")
    ),
    hjust = 0, family = text_family, size = 2.4, colour = col_ink
  ) +
  geom_rect(
    data = prof_plot,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = col_profile
  ) +
  geom_text(
    data = bel_note,
    aes(x = x_note, y = y, label = label),
    hjust = 0, vjust = 0.5, lineheight = 0.95,
    family = text_family, size = 2.2, colour = col_muted
  ) +
  scale_fill_identity() +
  coord_cartesian(
    xlim = c(x_class, x_max),
    ylim = c(min(rows$y) - 0.8, y_head + 0.8),
    expand = FALSE, clip = "off"
  ) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)
  
### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_43/2026_43.png")

save_ggplot(
  plot = p, file = out_file,
  width = fig_w, height = fig_h, units = "in", dpi = 320
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

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-26
# rstudio  2026.10.0+233 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────────────────
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
# ggrepel      * 0.9.8   2026-03-17 [1] CRAN (R 4.6.0)
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
# magick       * 2.9.1   2026-02-28 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
# otel           0.2.0   2025-08-29 [1] CRAN (R 4.6.0)
# pacman       * 0.5.1   2019-03-11 [1] CRAN (R 4.6.0)
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R.cache        0.17.0  2025-05-02 [1] CRAN (R 4.6.0)
# R.methodsS3    1.8.2   2022-06-13 [1] CRAN (R 4.6.0)
# R.oo           1.27.1  2025-05-02 [1] CRAN (R 4.6.0)
# R.utils        2.13.0  2025-02-24 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
# readxl       * 1.5.0   2026-05-16 [1] CRAN (R 4.6.0)
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
# styler         1.11.0  2025-10-13 [1] CRAN (R 4.6.0)
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
# ───────────────────────────────────────────────────────────────────────
