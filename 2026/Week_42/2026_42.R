## Challenge: #MakeoverMonday 2026 week 42
## Data:      The First Century of the U.S. Census

## Author:    Steven Ponce
## Date:      2026-10-18

## Article
# https://ec.europa.eu/eurostat/web/products-eurostat-news/w/ddn-20260702-1

## Data
# https://ec.europa.eu/eurostat/databrowser/view/tour_ce_omn12__custom_15997748/bookmark/table?lang=en&bookmarkId=3cebcd02-17c5-4393-be50-dcd8d670f4ac&c=1742994015070

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

### |- figure size ----
fig_w <- 10
fig_h <- 6.5
fig_dpi <- 320

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()
title_family <- fonts$title_1
text_family  <- fonts$text


## 2. READ IN THE DATA ----
df_raw <- read_csv(
  "data/2026/us_state_population_1790_1990.csv",
  col_types = cols(.default = col_character()),
  na = character()
) |>
  clean_names() |>
  mutate(year = as.integer(year), population = as.numeric(population))


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

two_states <- c("New York", "Pennsylvania")
original_13 <- c(
  "Connecticut", "Delaware", "Georgia", "Maryland", "Massachusetts",
  "New Hampshire", "New Jersey", "New York", "North Carolina",
  "Pennsylvania", "Rhode Island", "South Carolina", "Virginia"
)
grp_two <- "New York + Pennsylvania"
grp_eleven <- "Other 11 original states"

shares <- df_raw |>
  filter(year <= 1900) |>
  mutate(group = case_when(
    state %in% two_states ~ grp_two,
    state %in% original_13 ~ grp_eleven,
    .default = "Rest of U.S."
  )) |>
  summarise(pop = sum(population), .by = c(year, group)) |>
  mutate(share = pop / sum(pop), .by = year)

plot_data <- shares |> filter(group != "Rest of U.S.")

### |- values used in the copy ----
get_share <- function(g, y) {
  shares |>
    filter(group == g, year == y) |>
    pull(share)
}

two_1790 <- get_share(grp_two, 1790)
two_1900 <- get_share(grp_two, 1900)
eleven_1790 <- get_share(grp_eleven, 1790)
eleven_1900 <- get_share(grp_eleven, 1900)

peak <- plot_data |>
  filter(group == grp_two) |>
  slice_max(share, with_ties = FALSE)

orig13_by_year <- shares |>
  filter(group != "Rest of U.S.") |>
  summarise(share = sum(share), .by = year)

year_below_half <- min(orig13_by_year$year[orig13_by_year$share < 0.5])

state_delta <- df_raw |>
  filter(year %in% c(1790, 1900)) |>
  mutate(share = population / sum(population), .by = year) |>
  filter(state %in% original_13) |>
  select(state, year, share) |>
  pivot_wider(names_from = year, values_from = share, names_prefix = "y") |>
  mutate(delta_pp = 100 * (y1900 - y1790))

### |- end labels at fixed positions ----
end_labels <- plot_data |>
  filter(year == 1900) |>
  mutate(
    label_x = 1905,
    label_y = if_else(group == grp_eleven, share + 0.025, share - 0.025),
    label_vjust = if_else(group == grp_eleven, 0, 1),
    label_name = if_else(group == grp_eleven, "Other 11<br>original states", "New York +<br>Pennsylvania"),
    label = glue("**{label_name}**<br>{percent(share, 0.1)}")
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    paper       = "#f4f1ea",
    accent      = "#722F37",
    eleven_line = "#8A8780",
    eleven_text = "#6f6a63",
    ink         = "#2b2b2b",
    grid        = "#dedad1"
  )
)
clrs <- colors$palette
col_paper <- clrs$paper
col_accent <- clrs$accent
col_line11 <- clrs$eleven_line
col_text11 <- clrs$eleven_text
col_ink <- clrs$ink
col_grid <- clrs$grid

### |- titles and caption ----
title_text <- "The original thirteen didn't fade equally"

subtitle_text <- glue(
  "Share of U.S. population at each census, 1790–1900. ",
  "<b><span style='color:{col_accent}'>New York and Pennsylvania </span></b>",
  "held about a fifth of the nation; ",
  "<b><span style='color:{col_text11}'>the other eleven original states </span></b>",
  "fell from {percent(eleven_1790, 0.1)} to nearly the same level."
)

note_text <- glue(
  "Shares, not population counts: every group grew in population at every census. ",
  "The other eleven lost share collectively, though Georgia's share rose."
)
method_text <- "Modern state boundaries; U.S. totals include territories and D.C."

caption_text <- paste0(
  note_text, "<br><br>", method_text, "<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 42,
    source_text = "U.S. Census Bureau, Working Paper No. 56 (Gibson & Jung, 2002)"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_paper, color = NA),
    panel.background = element_rect(fill = col_paper, color = NA),
    plot.title = element_text(
      family = title_family, size = rel(1.9), color = col_ink,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      family = text_family, size = rel(0.95), color = col_ink,
      lineheight = 1.25, margin = margin(b = 18)
    ),
    plot.caption = element_textbox_simple(
      family = text_family, size = rel(0.62), color = col_text11,
      lineheight = 1.2, margin = margin(t = 14)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_text(
      family = text_family, size = rel(0.8),
      color = col_text11, margin = margin(r = 8)
    ),
    axis.text = element_text(family = text_family, size = rel(0.8), color = col_text11),
    legend.position = "none",
    plot.margin = margin(20, 45, 14, 20)
  )
)

theme_set(weekly_theme)

### |-  main plot ----
p <- ggplot(plot_data, aes(year, share, color = group)) +
  geom_line(linewidth = 1.3, lineend = "round") +
  geom_point(data = peak, size = 3, color = col_accent) +
  annotate(
    "text",
    x = peak$year, y = peak$share + 0.04,
    label = glue("{peak$year} peak · {percent(peak$share, 0.1)}"),
    family = text_family, color = col_accent, size = 3.8
  ) +
  geom_segment(
    data = end_labels,
    aes(x = 1900.8, xend = label_x - 0.6, y = share, yend = label_y),
    linewidth = 0.35
  ) +
  geom_richtext(
    data = end_labels,
    aes(x = label_x, y = label_y, label = label, vjust = label_vjust),
    hjust = 0, size = 3.8, lineheight = 1.05,
    family = text_family, fill = NA, label.color = NA,
    label.padding = unit(0, "pt")
  ) +
  scale_color_manual(
    values = c(col_accent, col_line11) |> set_names(c(grp_two, grp_eleven)),
    limits = c(grp_two, grp_eleven)
  ) +
  scale_x_continuous(
    breaks = seq(1790, 1900, 10),
    expand = expansion(mult = c(0.02, 0.20))
  ) +
  scale_y_continuous(
    labels = label_percent(), limits = c(0, 0.8),
    breaks = seq(0, 0.8, 0.2), expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL, y = "Share of U.S. population",
    title = title_text, subtitle = subtitle_text, caption = caption_text
  )
  
### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_42/2026_42.png")

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

# ─ Session info ───────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.6.2
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-10-18
# rstudio  2026.10.0+233 Blue Mistflower (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# base64enc      0.1-6   2026-02-02 [1] CRAN (R 4.6.0)
# bit            4.6.0   2025-03-06 [1] CRAN (R 4.6.0)
# bit64          4.8.2   2026-05-19 [1] CRAN (R 4.6.0)
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────────
# > 