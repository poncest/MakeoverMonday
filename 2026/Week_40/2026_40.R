## Challenge: #MakeoverMonday 2026 week 40
## Data:      Songs with Cowbell

## Author:    Steven Ponce
## Date:      2026-10-06

## Article
# https://cowbellsongs.com/the-list/

## Data
# https://cowbellsongs.com/the-list/

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview, readxl
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 10
fig_h <- 6.5

## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel(
  "data/2026/MM2026_wk40.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- cleaning ----
artist_fixes <- c(
  "Jimmy Buffet"                   = "Jimmy Buffett",
  "Sleater Kinney"                 = "Sleater-Kinney",
  "B’52s"                          = "B-52s",
  "Chambers Brothers"              = "The Chambers Brothers",
  "Earth, Wind, and Fire"          = "Earth Wind and Fire",
  "Rage Against the Machine"       = "Rage Against The Machine",
  "Bachman Turner Overdrive (BTO)" = "BTO",
  "Dave Mathews Band"              = "Dave Matthews Band"
)

df_clean <- df_raw |>
  mutate(
    swap    = artist %in% c("The Tide is high", "Deliverance"),
    artist2 = if_else(swap, title, artist),
    title   = if_else(swap, artist, title),
    artist  = artist2
  ) |>
  select(-swap, -artist2) |>
  mutate(
    artist = recode(artist, !!!artist_fixes),
    title = case_when(
      title == "Ain’t Seen Nothing Yet" ~ "You Ain’t Seen Nothing Yet!",
      title == "Time Has Come" ~ "Time Has Come Today",
      .default = title
    )
  ) |>
  distinct(artist, title)

### |- tiers: songs contributed by artists with N songs on the list ----
# display-only name fixes for the named head tiers
display_names <- c(
  "Beatles"      = "The Beatles",
  "Guns n Roses" = "Guns N’ Roses",
  "Donnas"       = "The Donnas"
)

tiers <- df_clean |>
  count(artist, name = "per_artist") |>
  mutate(artist = recode(artist, !!!display_names)) |>
  summarise(
    n_artists = n(),
    songs = sum(per_artist),
    names = str_flatten_comma(artist[order(str_remove(artist, "^The "))]),
    .by = per_artist
  ) |>
  arrange(per_artist)

### |- copy numbers, built from the data ----
n_songs <- sum(tiers$songs)
n_artists <- sum(tiers$n_artists)
t1 <- tiers |> filter(per_artist == 1)
share_t1 <- t1$songs / n_songs


## 5. VISUALIZATION ----

#### |- plot aesthetics ----
# encoding colors as plain constants 
col_paper <- "#f4f1ea"
col_accent <- "#722F37"
col_bar <- "#c9c4ba"
col_ink <- "#2b2926"
col_muted <- "#6b665e"

clrs <- get_theme_colors(
  palette = list(accent = col_accent, paper = col_paper)
)

### |-  plot data ----
allman_note <- glue(
  "<br><span style='font-size:8.5pt;color:{col_muted};'><i>",
  "All seven Allman Brothers songs were added in one July 2026 update</i></span>"
)

plot_df <- tiers |>
  mutate(
    row_lab = if_else(per_artist == 1, "1 song", glue("{per_artist} songs")),
    row_lab = fct_reorder(row_lab, per_artist), # 9 at top, 1 at bottom
    is_focus = per_artist == 1,
    who = case_when(
      is_focus ~ "one per artist",
      n_artists <= 4 ~ names,
      .default = glue("{n_artists} artists")
    ),
    note = if_else(per_artist == 7, allman_note, ""),
    bar_lab = if_else(
      is_focus,
      glue("<b>{songs}</b> songs, {who}"),
      glue(
        "<b>{songs}</b>{if_else(per_artist == max(per_artist), ' songs', '')}",
        "<span style='color:{col_muted};'> from {who}</span>{note}"
      )
    )
  )

### |-  titles and caption ----
title_text <- "Most cowbell comes one song at a time"

subtitle_text <- glue(
  "Each bar counts the songs from artists with that many songs on ",
  "cowbellsongs.com's reader-submitted list.<br>Artists listed only once supply ",
  "<span style='color:{col_accent};'><b>{t1$songs} of {n_songs}</b></span>."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 40,
  source_text = glue(
    "cowbellsongs.com, The List (reader-submitted since 2001)<br>",
    "Note: Cleaned for exact duplicates, spelling variants and swapped ",
    "artist/title rows ({nrow(df_raw)} rows → {n_songs} songs, {n_artists} artists). ",
    "Inclusion reflects reader submissions, not a census of cowbell use."
  )
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme <- create_base_theme(clrs)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_paper, color = NA),
    panel.background = element_rect(fill = col_paper, color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      family = fonts$text, size = 10, color = col_ink,
      hjust = 1, margin = margin(r = 8)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(
      family = fonts$title_1, size = 28, face = "bold",
      color = col_ink, margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = 11, color = col_muted,
      lineheight = 1.25, margin = margin(b = 18)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = 6.5, color = col_muted,
      lineheight = 1.3, margin = margin(t = 16)
    ),
    plot.margin = margin(20, 30, 14, 20)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(plot_df, aes(x = songs, y = row_lab)) +
  # Geoms
  geom_col(
    data = filter(plot_df, !is_focus),
    fill = col_bar, color = col_muted, linewidth = 0.3, width = 0.68
  ) +
  geom_col(
    data = filter(plot_df, is_focus),
    fill = col_accent, width = 0.68
  ) +
  geom_richtext(
    data = filter(plot_df, !is_focus),
    aes(label = bar_lab),
    hjust = 0, nudge_x = 4,
    family = fonts$text, size = 3.4, color = col_ink,
    fill = NA, label.color = NA, label.padding = unit(0, "pt")
  ) +
  geom_richtext(
    data = filter(plot_df, is_focus),
    aes(label = bar_lab),
    hjust = 1, nudge_x = -6,
    family = fonts$text, size = 3.6, color = "white",
    fill = NA, label.color = NA, label.padding = unit(0, "pt")
  ) +
  # Scales
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(limits = levels(plot_df$row_lab)) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text
  )

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_40/2026_40.png")

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
# date     2026-10-06
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
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
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
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
# > 
