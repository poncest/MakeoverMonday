## Challenge: #MakeoverMonday 2026 week 41
## Data:      Tourism Nights Booked Through Platforms

## Author:    Steven Ponce
## Date:      2026-10-11

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
  janitor, ggview, readxl, ggrepel
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel(
  "data/2026/Tourism_20Nights_20Booked_20Through_20Platforms_202025.xlsx"
) |>
  clean_names()

months <- c(
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december"
)


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- data rows: total is digits or ":" (drops Eurostat legend footer) ----
df <- df_raw |>
  filter(!is.na(geo_labels), str_detect(total, "^[0-9]+$|^:$")) |>
  select(geo = geo_labels, total, all_of(months)) |>
  mutate(across(c(total, all_of(months)), ~ as.numeric(na_if(str_trim(.x), ":"))))

### |- infer NUTS level (extract has labels only, no geo codes) ----
# Eurostat order: country -> NUTS1 -> its NUTS2 children. Integer values, so
# every NUTS1 must equal its children's sum exactly (checked below).
countries <- c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia",
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "Iceland", "Liechtenstein", "Norway", "Switzerland"
)

infer_nuts_levels <- function(d) {
  n <- nrow(d)
  level <- rep(NA_character_, n)
  country <- rep(NA_character_, n)
  parent1 <- rep(NA_integer_, n)
  level[str_detect(d$geo, "^European Union")] <- "EU"
  level[d$geo %in% countries & d$geo != lag(d$geo, default = "")] <- "country"

  pair_start <- function(i) {
    i < n && d$geo[i + 1] == d$geo[i] &&
      identical(d$total[i + 1], d$total[i])
  }
  closes_ahead <- function(i, max_k = 20) {
    if (is.na(d$total[i])) {
      return(FALSE)
    }
    acc <- 0
    for (j in seq(i + 1, min(n, i + max_k))) {
      if (identical(level[j], "country") || is.na(d$total[j])) {
        return(FALSE)
      }
      acc <- acc + d$total[j]
      if (acc == d$total[i]) {
        return(TRUE)
      }
      if (acc > d$total[i]) {
        return(FALSE)
      }
    }
    FALSE
  }

  cur_country <- NA_character_
  cur_l1 <- NA_integer_
  remaining <- 0
  has_na <- FALSE
  for (i in seq_len(n)) {
    if (identical(level[i], "EU")) next
    if (identical(level[i], "country")) {
      cur_country <- d$geo[i]
      country[i] <- cur_country
      cur_l1 <- NA_integer_
      remaining <- 0
      has_na <- FALSE
      next
    }
    country[i] <- cur_country
    tot <- d$total[i]
    if (i > 1 && identical(level[i - 1], "nuts1") && d$geo[i - 1] == d$geo[i] &&
      identical(d$total[i - 1], tot)) {
      level[i] <- "nuts2"
      parent1[i] <- i - 1
      remaining <- 0
      has_na <- FALSE
      next
    }
    if (is.na(cur_l1) || remaining == 0 || pair_start(i) || (has_na && closes_ahead(i))) {
      level[i] <- "nuts1"
      cur_l1 <- i
      remaining <- coalesce(tot, 0)
      has_na <- FALSE
    } else {
      level[i] <- "nuts2"
      parent1[i] <- cur_l1
      if (is.na(tot)) has_na <- TRUE else remaining <- remaining - tot
    }
  }
  mutate(d, level, country, parent1)
}

geo_tbl <- infer_nuts_levels(df)

### |- hierarchy guards ----
l1_check <- geo_tbl |>
  filter(level == "nuts2") |>
  group_by(parent1) |>
  summarise(
    child_sum = sum(total, na.rm = TRUE), n_na = sum(is.na(total)),
    .groups = "drop"
  ) |>
  mutate(gap = geo_tbl$total[parent1] - child_sum)

eu_row <- geo_tbl |> filter(level == "EU")

### |- NUTS2 regions with data ----
nuts2 <- geo_tbl |>
  filter(level == "nuts2", !is.na(total)) |>
  mutate(
    julaug_share = (july + august) / total,
    q4 = october + november + december,
    q4_share = q4 / total,
    peak_month = months[max.col(across(all_of(months)), ties.method = "first")],
    rank_year = min_rank(desc(total)),
    rank_q4 = min_rank(desc(q4))
  )

cells <- nuts2 |>
  select(geo, julaug_share, total, all_of(months)) |>
  pivot_longer(all_of(months), names_to = "month", values_to = "nights") |>
  mutate(
    share = nights / total,
    month_i = match(month, months),
    row_y = as.numeric(fct_reorder(geo, julaug_share))
  ) # top = most concentrated

### |- computed facts ----
n_regions <- nrow(nuts2)
n_summer_peak <- sum(nuts2$peak_month %in% c("july", "august"))
eu_julaug <- (eu_row$july + eu_row$august) / eu_row$total
min_ja <- min(nuts2$julaug_share)
max_ja <- max(nuts2$julaug_share)
jad <- nuts2 |> filter(geo == "Jadranska Hrvatska")
eu_q4_share <- (eu_row$october + eu_row$november + eu_row$december) / eu_row$total

share_q <- quantile(cells$share, c(.99, 1))
cap <- ceiling(share_q[[1]] * 100) / 100
n_above <- sum(cells$share > cap)
pct <- \(x) paste0(format(round(100 * x, 1), nsmall = 1), "%")

### |- anchors (primary) ----
anchors <- tribble(
  ~geo, ~label, ~dy, ~vjust,
  "Yugoiztochen", "Yugoiztochen (BG)\nmost summer-heavy region", 0, 1,
  "Jadranska Hrvatska", "Adriatic Croatia\n#2 for the year;\noutside top 25 in Oct-Dec", -22, 1,
  "Alsace", "Alsace (FR)\nbusiest month: December", 0, 0.5,
  "Pohjois- ja Itä-Suomi", "Northern & Eastern Finland\nleast summer-heavy;\nbusiest month: December", 0, 0
) |>
  left_join(cells |> distinct(geo, row_y), by = "geo") |>
  mutate(label_y = row_y + dy)

### |- secondary label: darkest December row (misattribution guard) ----
show_secondary <- TRUE
dec_top <- cells |>
  filter(month == "december") |>
  slice_max(share, n = 1, with_ties = FALSE)

secondary <- tibble(
  geo = dec_top$geo, row_y = dec_top$row_y, label_y = dec_top$row_y,
  label = glue("Sud-Vest Oltenia (RO)\n{pct(dec_top$share)} of its year in December")
)


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    background = "#f4f1ea",
    text       = "#2b2b2b",
    muted      = "#6b6b6b"
  )
)
clrs <- colors$palette

# encoding colours as plain constants
col_high <- "#722F37"
col_low <- "#4A6F8A"
col_mid <- "#f4f1ea"

### |-  titles and caption ----
title_text <- "Summer peaks almost everywhere.<br>How much of the year it takes doesn't."

subtitle_text <- glue(
  "{n_summer_peak} of the {n_regions} European regions with data peaked in July or August. ",
  "But those two months hold {pct(min_ja)} to {pct(max_ja)} of a region's ",
  "platform-booked nights."
)

legend_title <- paste0(
  "Each row = one region (not weighted by size), most summer-heavy at top.\n",
  "Colour = that month's share of the region's 2025 nights; neutral = an even 1/12."
)

note_text <- glue(
  "Note: Guest nights booked via Airbnb, Booking and Expedia Group, 2025, NUTS 2 regions with data. ",
  "Eurostat's Q4 map covers {pct(eu_q4_share)} of the EU year. ",
  "No regional data for parts of Portugal (incl. Lisbon), the Netherlands and Iceland. ",
  "Colour capped at {cap * 100}% ({n_above} of {comma(nrow(cells))} cells above)."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 41,
  source_text = "Eurostat, tour_ce_omn12 (experimental statistics)"
)
caption_text <- glue("{note_text}<br>{caption_text}")

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1

### |-  plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = clrs$background, colour = NA),
    panel.background = element_rect(fill = clrs$background, colour = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = rel(0.9), colour = clrs$muted),
    plot.title = element_textbox_simple(
      family = title_family, face = "bold", size = rel(1.6),
      lineheight = 1.05, colour = clrs$text, margin = margin(b = 14)
    ),
    plot.subtitle = element_textbox_simple(
      family = fonts$subtitle, size = rel(0.85), lineheight = 1.25,
      colour = clrs$text, margin = margin(b = 14)
    ),
    plot.caption = element_textbox_simple(
      family = fonts$caption, size = rel(0.6), lineheight = 1.3,
      colour = clrs$muted, margin = margin(t = 12)
    ),
    legend.position = "bottom",
    legend.justification = "left",
    legend.title.position = "top",
    legend.title = element_text(size = rel(0.75), colour = clrs$muted),
    legend.text = element_text(size = rel(0.7), colour = clrs$muted),
    plot.margin = margin(20, 190, 15, 20)
  )
)
theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(cells, aes(month_i, row_y, fill = share)) +
  geom_tile() +
  geom_segment(
    data = anchors, aes(x = 12.55, xend = 12.95, y = row_y, yend = label_y),
    inherit.aes = FALSE, colour = clrs$muted, linewidth = 0.25
  ) +
  geom_text(
    data = anchors, aes(x = 13.05, y = label_y, label = label, vjust = vjust),
    inherit.aes = FALSE, family = fonts$text, size = 3.1, lineheight = 0.95,
    colour = clrs$text, hjust = 0
  ) +
  {
    if (show_secondary) {
      list(
        geom_segment(
          data = secondary, aes(x = 12.55, xend = 12.95, y = row_y, yend = label_y),
          inherit.aes = FALSE, colour = clrs$muted, linewidth = 0.2
        ),
        geom_text(
          data = secondary, aes(x = 13.05, y = label_y, label = label),
          inherit.aes = FALSE, family = fonts$text, size = 2.6, lineheight = 0.95,
          colour = clrs$muted, hjust = 0, vjust = 0.5
        )
      )
    }
  } +
  scale_fill_gradient2(
    low = col_low, mid = col_mid, high = col_high,
    midpoint = 1 / 12, limits = c(0, cap), oob = squish,
    breaks = c(0, 1 / 12, 0.15, cap),
    labels = c("0%", "1/12", "15%", glue("{cap * 100}%+")),
    name = legend_title
  ) +
  scale_x_continuous(
    breaks = 1:12, labels = str_sub(str_to_title(months), 1, 3),
    expand = c(0, 0), position = "top"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.5, 12.5), clip = "off") +
  guides(fill = guide_colorbar(barwidth = unit(6, "cm"), barheight = unit(0.3, "cm"))) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)
  
### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
fig_w <- 10; fig_h <- 10
out_file <- here::here("2026/Week_41/2026_41.png")

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

# ─ Session info ──────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-11
# rstudio  2026.08.1+195 Yellow Yarrow (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ──────────────────────────────────────
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
# ─────────────────────────────────────────────────

