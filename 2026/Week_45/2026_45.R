## Challenge: #MakeoverMonday 2026 week 45
## Data:      Hot Air Balloons

## Author:    Steven Ponce
## Date:      2026-11-09

## Article
# https://thehabdatabase.com

## Data
# https://www.kaggle.com/datasets/matthuff4/hot-air-balloon

## Citation
# The Hot Air Balloon Database (thehabdatabase.com), "Database-Entries - Balloons",
# via Kaggle (matthuff4). Size classes: FAI Sporting Code Section 1 - Aerostats,
# August 2026, para 2.1.2.

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  ggview, readxl, magick, janitor
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 10
fig_dpi <- 320
fig_h <- 6.22

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
text_family <- fonts$text
cap_family <- fonts$caption


## 2. READ IN THE DATA ----
xl_path <- here::here("data/2026/hot_air_balloon.xlsx")

df_raw <- read_excel(
  xl_path,
  skip = 1,
  col_types = "text",
  na = character(),
  .name_repair = "unique"
) |>
  clean_names()


## 3. EXAMINING THE DATA ----
# split records
df_raw <- df_raw |>
  mutate(
    is_record = str_detect(balloon_id, "^HAB-\\d{6}$"),
    record_no = cumsum(is_record)
  )

glimpse(df_raw)


## 4. TIDY DATA ----

### |- reattach the split records ----
fragments <- df_raw |>
  filter(!is_record) |>
  summarise(
    owners_cont = paste(balloon_id, collapse = " "),
    across(c(name, registration, manufacturer, model, serial_number, year_built, size), last),
    .by = record_no
  ) |>
  mutate(
    record_no, owners_cont,
    image_f = name, seen_f = registration, crewed_f = manufacturer,
    flown_f = model, burner_time_f = serial_number,
    additional_images_f = year_built, notes_f = size,
    .keep = "none"
  )

balloons <- df_raw |>
  filter(is_record) |>
  left_join(fragments, by = "record_no") |>
  mutate(
    previous_owners = if_else(is.na(owners_cont), previous_owners,
      str_squish(paste(previous_owners, owners_cont))
    ),
    image = coalesce(image_f, image),
    seen = coalesce(seen_f, seen),
    crewed = coalesce(crewed_f, crewed),
    flown = coalesce(flown_f, flown),
    burner_time = coalesce(burner_time_f, burner_time),
    additional_images = coalesce(additional_images_f, additional_images),
    notes = coalesce(notes_f, notes)
  ) |>
  select(balloon_id:notes)

### |- parse fields ----
balloons <- balloons |>
  mutate(
    # first four-digit year = original build; 4 impossible values (2969, 2997 x2,
    # 6077) and 3 malformed ones ("200", "2-14", "2-22") become NA
    build_year = suppressWarnings(as.integer(str_extract(year_built, "^\\d{4}"))),
    build_year = if_else(build_year > 2026, NA_integer_, build_year),
    volume_cuft = suppressWarnings(as.numeric(
      str_remove_all(str_extract(size, "^[\\d,]+(?=\\s*cu)"), ",")
    )),
    in_us_state = location %in% c(state.name, "District of Columbia")
  )

### |- giant threshold: FAI size class AX-11 and above ----
# FAI Sporting Code Section 1 (Aug 2026), 2.1.2: category 11 = 6,000-9,000 m3
cuft_per_m3 <- 35.3147
giant_m3 <- 6000
giant_cuft <- giant_m3 * cuft_per_m3

### |- the three build periods compared ----
periods <- tibble(
  period = c("2000–2009", "2010–2019", "2020–2026"),
  from   = c(2000L, 2010L, 2020L),
  to     = c(2009L, 2019L, 2026L)
)

sized <- balloons |>
  filter(!is.na(volume_cuft), !is.na(build_year)) |>
  inner_join(periods, by = join_by(between(build_year, from, to))) |>
  mutate(giant = volume_cuft > giant_cuft)

us_summary <- sized |>
  filter(in_us_state) |>
  summarise(
    n = n(), k = sum(giant), median_cuft = median(volume_cuft),
    .by = period
  ) |>
  arrange(period) |>
  mutate(
    pct   = 100 * k / n,
    units = round(pct)
  )

outside_summary <- sized |>
  filter(!in_us_state) |>
  summarise(n = n(), k = sum(giant), .by = period) |>
  arrange(period) |>
  mutate(pct = 100 * k / n)

typical_cuft <- unique(us_summary$median_cuft)

### |- unit grids: 10 x 10 per period ----
grid_df <- us_summary |>
  select(period, units) |>
  reframe(i = 1:100, .by = c(period, units)) |>
  mutate(
    col  = (i - 1) %% 10,
    row  = 9 - (i - 1) %/% 10,
    kind = factor(if_else(i <= units, "giant", "other"), levels = c("giant", "other"))
  )

grid_labels <- us_summary |>
  mutate(
    label = glue("{sprintf('%.1f', pct)}%  ·  {k} of {comma(n)} recorded builds"),
    x = 4.5, y = -1.25
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    paper  = "#f4f1ea",
    ink    = "#2b2b2b",
    muted  = "#6f6a63",
    accent = "#722F37",
    square = "#c9c4ba"
  )
)
clrs <- colors$palette
col_paper <- clrs$paper
col_ink <- clrs$ink
col_muted <- clrs$muted
col_accent <- clrs$accent
col_square <- clrs$square

### |- titles and caption ----
title_text <- "The typical balloon stayed the same size.\nGiants became more common."

subtitle_text <- paste0(
  "Hot-air balloons recorded in US states, by the year they were built.\n",
  glue("The median rated envelope volume was {comma(typical_cuft)} cu ft in every period.")
)

key_labels <- c(
  giant = glue("Giant: rated over {comma(round(giant_cuft, -2))} cu ft, more than twice the typical balloon"),
  other = "All other recorded balloons"
)

note_text <- paste(
  glue("Giant = FAI size class AX-11 or larger (over {comma(giant_m3)} m³)."),
  "Squares rounded to whole percents; balloons without a rated size or build year excluded.",
  glue(
    "Outside US states: ",
    paste(glue("{sprintf('%.1f', outside_summary$pct)}% ({outside_summary$k} of {outside_summary$n})"), collapse = ", "),
    "."
  ),
  "A balloonist's archive, not a census of production; 2026 is partial."
)

caption_text <- paste0(
  note_text, "<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 45,
    source_text = "The Hot Air Balloon Database, via Kaggle"
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
    strip.text = element_text(
      family = text_family, face = "bold", size = rel(1.05),
      colour = col_ink, margin = margin(b = 8)
    ),
    panel.spacing = unit(40, "pt"),
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "vertical",
    legend.title = element_text(
      family = text_family, size = rel(0.72), colour = col_muted,
      margin = margin(b = 4)
    ),
    legend.text = element_text(family = text_family, size = rel(0.72), colour = col_ink),
    legend.key.size = unit(11, "pt"),
    legend.key.spacing.y = unit(3, "pt"),
    legend.margin = margin(t = 2, b = 6),
    plot.title = element_text(
      family = title_family, size = rel(1.72), colour = col_ink,
      lineheight = 1.05, margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.80), colour = col_muted,
      lineheight = 1.3, margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = cap_family, size = rel(0.45), colour = col_muted,
      lineheight = 1.3, margin = margin(t = 12)
    ),
    plot.margin = margin(16, 60, 12, 60)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(grid_df, aes(x = col, y = row)) +
  geom_tile(
    aes(fill = kind, colour = kind),
    width = 0.84, height = 0.84, linewidth = 0.3
  ) +
  geom_text(
    data = grid_labels,
    aes(x = x, y = y, label = label),
    family = text_family, size = 3.2, colour = col_ink
  ) +
  scale_fill_manual(
    values = c(giant = col_accent, other = col_square),
    labels = key_labels,
    name = "Each square: 1 in every 100 recorded builds"
  ) +
  scale_colour_manual(
    values = c(giant = col_paper, other = col_muted),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(-0.6, 9.6), expand = expansion(0)) +
  scale_y_continuous(limits = c(-1.9, 9.6), expand = expansion(0)) +
  facet_wrap(~period) +
  coord_fixed(clip = "off") +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_45/2026_45.png")

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

# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.7.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-09
# rstudio  2026.10.1+239 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
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
# janitor      * 2.2.1   2024-12-22 [1] CRAN (R 4.6.0)
# jsonlite       2.0.0   2025-03-27 [1] CRAN (R 4.6.0)
# labeling       0.4.3   2023-08-29 [1] CRAN (R 4.6.0)
# lifecycle      1.0.5   2026-01-08 [1] CRAN (R 4.6.0)
# litedown       0.10    2026-07-11 [1] CRAN (R 4.6.1)
# lubridate    * 1.9.5   2026-02-04 [1] CRAN (R 4.6.0)
# magick       * 2.9.1   2026-02-28 [1] CRAN (R 4.6.0)
# magrittr       2.0.5   2026-04-04 [1] CRAN (R 4.6.0)
# markdown       2.0     2025-03-23 [1] CRAN (R 4.6.0)
# P methods      * 4.6.1   2026-06-25 [1] local
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
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rprojroot      2.1.1   2025-08-26 [1] CRAN (R 4.6.0)
# rstudioapi     0.19.0  2026-06-11 [1] CRAN (R 4.6.0)
# S7             0.2.2   2026-04-22 [1] CRAN (R 4.6.0)
# scales       * 1.4.0   2025-04-24 [1] CRAN (R 4.6.0)
# sessioninfo    1.2.4   2026-06-04 [1] CRAN (R 4.6.0)
# showtext     * 0.9-8   2026-03-21 [1] CRAN (R 4.6.0)
# showtextdb   * 3.0     2020-06-04 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────────────────────────
# > 