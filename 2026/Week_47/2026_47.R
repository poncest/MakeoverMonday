## Challenge: #MakeoverMonday 2026 week 47
## Data:      What Does 'Likely' Actually Mean? (CAPphrase pairwise comparisons)

## Author:    Steven Ponce
## Date:      2026-11-23

## Article
# https://adamkucharski.github.io/CAPphrase/

## Data
# https://zenodo.org/records/18750055
# https://github.com/adamkucharski/CAPphrase

## Citation
# Kucharski AJ (2026). CAPphrase: Comparative and Absolute Probability phrase
#   dataset. DOI: 10.5281/zenodo.18750055

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  ggview, magick, janitor
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

### |- figure size ----
fig_w <- 10
fig_h <- 7
fig_dpi <- 320

### |- fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
text_family <- fonts$text
cap_family <- fonts$caption

## 2. READ IN THE DATA ----
csv_path <- here::here("data/2026/pairwise_comparisons.csv")

df_raw <- read_csv(
  csv_path,
  col_types = cols(.default = col_character()), na = character()
) |>
  clean_names()

## 3. EXAMINING THE DATA ----
glimpse(df_raw)
df_raw |> count(pair_id)

## 4. TIDY DATA ----

### |- unordered pair key ----
df <- df_raw |>
  mutate(
    response_id = as.integer(response_id),
    pair_id     = as.integer(pair_id),
    a           = pmin(term1, term2),
    b           = pmax(term1, term2)
  )

### |- crowd split from comparisons 2-9 (no repeat answers inside) ----
split_ind <- df |>
  filter(pair_id %in% 2:9) |>
  summarise(p_b = mean(selected == b), .by = c(a, b))

### |- the re-ask: comparison 1 shown again as comparison 10 ----
reask <- df |>
  filter(pair_id == 1) |>
  select(response_id, a, b, side_1 = term1, sel_1 = selected) |>
  inner_join(
    df |> filter(pair_id == 10) |> select(response_id, a, b, side_10 = term1, sel_10 = selected),
    by = c("response_id", "a", "b")
  ) |>
  left_join(split_ind, by = c("a", "b")) |>
  mutate(
    changed  = sel_1 != sel_10,
    majority = pmax(p_b, 1 - p_b),
    between  = 2 * p_b * (1 - p_b),    # two different people disagree
    split    = majority < 0.70         # majority got 50-70% of first answers
  )

focal <- filter(reask, split)
focal_pairs <- focal |>
  summarise(n = n(), between = first(between), .by = c(a, b))

### |- pooled rates (respondent-weighted) ----
n_pairs <- nrow(focal_pairs)
n_resp <- nrow(focal)
rate_within <- mean(focal$changed)
rate_between <- mean(focal$between)
rate_repeat <- 1 - rate_within

### |- 95% intervals: bootstrap resampling the 18 pairs ----
set.seed(47)
boot <- map(1:4000, \(i) {
  pick <- focal_pairs |> slice_sample(n = n_pairs, replace = TRUE) |> select(a, b)
  d <- inner_join(pick, focal, by = c("a", "b"), relationship = "many-to-many")
  tibble(within = mean(d$changed), between = mean(d$between))
}) |> list_rbind()

ci <- boot |> summarise(across(everything(), \(x) list(quantile(x, c(0.025, 0.975)))))
ci_within <- ci$within[[1]]
ci_between <- ci$between[[1]]

pct1 <- label_percent(accuracy = 0.1)

### |- answers behind the between-person rate ----
n_between_answers <- df |>
  filter(pair_id %in% 2:9) |>
  semi_join(focal_pairs, by = c("a", "b")) |>
  nrow()

### |- plot data ----
comma0 <- label_comma(accuracy = 1)
rng <- \(ci) glue("95% bootstrap interval: {pct1(ci[1])}–{pct1(ci[2])}")

plot_df <- tibble(
  focus = c("between", "within"),
  y     = c(2, 1),
  rate  = c(rate_between, rate_within),
  lo    = c(ci_between[1], ci_within[1]),
  hi    = c(ci_between[2], ci_within[2]),
  head  = c(
    "Two different people, each answering once",
    "The same person, asked again a few questions later"
  ),
  foot = c(
    glue("{rng(ci_between)} · from {comma0(n_between_answers)} first answers"),
    glue("{rng(ci_within)} · {n_resp} people re-asked")
  )
) |>
  mutate(label = pct1(rate))

bar_half <- 0.13
track_max <- 1.00 

## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    within  = "#722F37", 
    between = "#7A7068", 
    ink     = "#2C2825",
    muted   = "#72685F",
    track   = "#E4DFD6"
  )
)
clrs <- colors$palette
col_within <- clrs$within
col_between <- clrs$between
col_ink <- clrs$ink
col_muted <- clrs$muted
col_track <- clrs$track

### |- titles and caption ----
title_text <- "A divided crowd doesn't mean\nmost people change their answers"

subtitle_text <- glue(
  "{n_resp} people saw one of {n_pairs} phrase pairs that divided the crowd ",
  "(the majority got 50 to 70% of first answers)\ntwice, a few questions apart. ",
  "How often did two answers to the same pair differ?"
)

note_text <- glue(
  "Two different people: the chance that two respondents, each answering once, chose differently ",
  "(first answers in comparisons 2 to 9). The same person: each respondent's first comparison ",
  "was shown again as their tenth, sides swapped.<br>",
  "95% bootstrap intervals come from resampling the {n_pairs} phrase pairs. Calculation details in the write-up."
)

caption_text <- paste0(
  note_text, "<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 47,
    source_text = "CAPphrase pairwise comparisons, Kucharski (2026), doi:10.5281/zenodo.18750055"
  )
)

sel_note <- "Near 50% by design: these pairs\nwere chosen because the crowd split"

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      family = title_family, size = rel(1.72), color = col_ink, margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.80), color = col_muted,
      lineheight = 1.3, margin = margin(b = 10)
    ),
    plot.caption = element_textbox_simple(
      family = cap_family, size = rel(0.45), color = col_muted,
      lineheight = 1.3, margin = margin(t = 16), width = unit(1, "npc")
    ),
    plot.margin = margin(16, 24, 12, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(plot_df, aes(y = y)) +
  geom_rect(aes(xmin = 0, xmax = track_max, ymin = y - bar_half, ymax = y + bar_half), fill = col_track) +
  geom_rect(aes(xmin = 0, xmax = rate, ymin = y - bar_half, ymax = y + bar_half, fill = focus)) +
  geom_segment(aes(x = lo, xend = hi, y = y - bar_half - 0.05, yend = y - bar_half - 0.05, color = focus), linewidth = 0.9) +
  geom_segment(aes(x = lo, xend = lo, y = y - bar_half - 0.08, yend = y - bar_half - 0.02, color = focus), linewidth = 0.9) +
  geom_segment(aes(x = hi, xend = hi, y = y - bar_half - 0.08, yend = y - bar_half - 0.02, color = focus), linewidth = 0.9) +
  geom_text(aes(x = 0, y = y + bar_half + 0.10, label = head),
    hjust = 0, vjust = 0, family = text_family, fontface = "bold", size = 4.7, color = col_ink
  ) +
  geom_text(aes(x = 0.006, y = y, label = label),
    hjust = 0, vjust = 0.5, family = text_family, fontface = "bold", size = 6, color = "white"
  ) +
  geom_text(aes(x = 0, y = y - bar_half - 0.14, label = foot),
    hjust = 0, vjust = 1, family = text_family, size = 3.4, color = col_muted
  ) +
  annotate("text",
    x = track_max, y = 2 + bar_half + 0.10, label = sel_note,
    hjust = 1, vjust = 0, lineheight = 1.1, family = text_family, size = 3.3, color = col_muted
  ) +
  scale_fill_manual(values = c(between = col_between, within = col_within)) +
  scale_color_manual(values = c(between = col_between, within = col_within)) +
  scale_x_continuous(limits = c(0, track_max), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.62, 2.50), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_47/2026_47.png")

save_ggplot(
  plot = p, file = out_file,
  width = fig_w, height = fig_h, units = "in", dpi = fig_dpi
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
# Also used (packages, not custom helpers):
#    • ggview::canvas(), ggview::save_ggplot() - preview at final size; save
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

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.7.3
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-23
# rstudio  2026.11.0+203 Montauk Daisy (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# crayon         1.5.3   2024-06-20 [1] CRAN (R 4.6.0)
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# dplyr        * 1.2.1   2026-04-03 [1] CRAN (R 4.6.0)
# farver         2.1.2   2024-05-13 [1] CRAN (R 4.6.0)
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
# P parallel       4.6.1   2026-06-25 [1] local
# pillar         1.11.1  2025-09-17 [1] CRAN (R 4.6.0)
# pkgconfig      2.0.3   2019-09-22 [1] CRAN (R 4.6.0)
# purrr        * 1.2.2   2026-04-10 [1] CRAN (R 4.6.0)
# R6             2.6.1   2025-02-15 [1] CRAN (R 4.6.0)
# ragg           1.5.2   2026-03-23 [1] CRAN (R 4.6.0)
# RColorBrewer   1.1-3   2022-04-03 [1] CRAN (R 4.6.0)
# Rcpp           1.1.2   2026-07-05 [1] CRAN (R 4.6.1)
# readr        * 2.2.0   2026-02-19 [1] CRAN (R 4.6.0)
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
# ───────────────────────────────────────────────────────────────────────────
# > 