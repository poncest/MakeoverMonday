## Challenge: #MakeoverMonday 2026 week 46
## Data:      Minnesota Turkey Fact Sheet (MDA) + USDA NASS 2023 turkeys raised

## Author:    Steven Ponce
## Date:      2026-11-17

## Article
# https://www.mda.state.mn.us/sites/default/files/inline-files/MN%20turkey%20fact%20sheet%20%282023%29.pdf

## Data
# https://www.mda.state.mn.us/sites/default/files/inline-files/MN%20turkey%20fact%20sheet%20%282023%29.pdf
# USDA NASS Quick Stats (accessed 2026-11-16): https://quickstats.nass.usda.gov/

## Citation
# USDA National Agricultural Statistics Service. Quick Stats: Turkeys, Production,
#   2023 (head; live-weight lb). Accessed 2026-11-16.
# Minnesota Department of Agriculture (Su Ye). Minnesota Turkey Fact Sheet (2023).

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  ggview, readxl, magick, janitor, rnassqs
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

### |- MDA fact sheet workbook (the original's numbers) ----
xl_path <- here::here("data/2026/minnesota_turkey_fact_sheet.xlsx")

states_raw <- read_excel(
  xl_path, sheet = "US Top Producing States", range = "A4:D15",
  col_types = c("text", "text", "text", "numeric")
) |> clean_names()

### |- USDA NASS 2023 (cached) ----
nass_path <- here::here("data/2026/nass_turkeys_2023.rds")

if (!file.exists(nass_path)) {
  nass_pull <- rnassqs::nassqs(list(
    source_desc    = "SURVEY",
    commodity_desc = "TURKEYS",
    year           = 2023,
    agg_level_desc = c("STATE", "NATIONAL"),
    freq_desc      = "ANNUAL"
  ))
  saveRDS(nass_pull, nass_path)
}
nass_raw <- readRDS(nass_path) |> as_tibble()


## 3. EXAMINING THE DATA ----
glimpse(states_raw)
nass_raw |> count(short_desc, unit_desc, reference_period_desc)


## 4. TIDY DATA ----

### |- NASS: turkeys raised (head) and live-weight production (lb) ----
nass_items <- c(
  HEAD = "TURKEYS - PRODUCTION, MEASURED IN HEAD",
  LB   = "TURKEYS - PRODUCTION, MEASURED IN LB"
)

nass_wide <- nass_raw |>
  filter(short_desc %in% nass_items, reference_period_desc == "YEAR") |>
  mutate(unit = names(nass_items)[match(short_desc, nass_items)]) |>
  select(agg_level_desc, state_name, state_alpha, unit, value = Value) |>
  pivot_wider(names_from = unit, values_from = value) |>
  rename(head = HEAD, lb = LB)

us_tot <- filter(nass_wide, agg_level_desc == "NATIONAL")

states_nass <- nass_wide |>
  filter(agg_level_desc == "STATE", state_alpha != "OT") |> # OT = "Other States"
  mutate(
    lb_per_bird = lb / head,
    share_head  = head / us_tot$head,
    share_lb    = lb / us_tot$lb
  )

### |- residual: "Other States" (includes states NASS withholds) ----
nass_residual <- tibble(
  share_head = 1 - sum(states_nass$share_head),
  share_lb   = 1 - sum(states_nass$share_lb)
)

### |- plot data: one row per state x measure, one shared state order ----
measure_labels <- c(
  share_head = "Share of U.S. turkeys raised",
  share_lb   = "Share of U.S. live weight"
)

state_levels <- states_nass |>
  arrange(head) |>
  pull(state_name) |>
  str_to_title()

plot_df <- states_nass |>
  mutate(
    focus = case_when(state_alpha == "MN" ~ "MN", state_alpha == "NC" ~ "NC", .default = "other"),
    state = factor(str_to_title(state_name), levels = state_levels)
  ) |>
  select(state, state_alpha, focus, lb_per_bird, share_head, share_lb) |>
  pivot_longer(c(share_head, share_lb), names_to = "measure", values_to = "share") |>
  mutate(measure = factor(measure_labels[measure], levels = measure_labels))

pct1 <- label_percent(accuracy = 0.1)
pct0 <- label_percent(accuracy = 1)

label_df <- plot_df |>
  filter(focus != "other") |>
  mutate(label = pct1(share))

weight_df <- plot_df |>
  filter(focus != "other", measure == measure_labels[["share_lb"]]) |>
  mutate(label = glue("avg. {number(lb_per_bird, accuracy = 0.1)} lb per bird"))


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    mn      = "#722F37",
    nc      = "#7A7068",
    nc_text = "#655C55",
    context = "#C9C4BA",
    ink     = "#2C2825",
    muted   = "#7A7068",
    grid    = "#E4DFD6"
  )
)
clrs <- colors$palette
col_mn <- clrs$mn
col_nc <- clrs$nc
col_nc_text <- clrs$nc_text
col_context <- clrs$context
col_ink <- clrs$ink
col_muted <- clrs$muted
col_grid <- clrs$grid

### |- titles and caption ----
# plain text: gridtext (textbox and markdown alike) widens word gaps with these fonts
title_text <- "Minnesota raises a third more turkeys than North Carolina,\nbut nearly the same live weight"

subtitle_text <- "2023 U.S. turkey production."

note_text <- glue(
  "Shares of the U.S. total for the 13 states USDA reports separately, ordered by turkeys raised. ",
  "The rest of the U.S. ({pct0(nass_residual$share_head)} of turkeys, ",
  "{pct0(nass_residual$share_lb)} of live weight) includes states USDA withholds.<br>",
  "Live weight is the weight of the birds, not meat. Average weight per bird = live weight ÷ turkeys raised."
)

caption_text <- paste0(
  note_text, "<br>",
  create_social_caption(
    mm_year = 2026,
    mm_week = 46,
    source_text = "USDA NASS Quick Stats, 2023 (accessed 2026-11-16)"
  )
)

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.3),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(family = text_family, size = rel(0.82), colour = col_ink, hjust = 1),
    axis.text.x = element_text(family = text_family, size = rel(0.70), colour = col_muted),
    strip.text = element_text(
      family = text_family, face = "bold", size = rel(0.95),
      colour = col_ink, hjust = 0, margin = margin(b = 8)
    ),
    panel.spacing.x = unit(28, "pt"),
    legend.position = "none",
    plot.title = element_text(
      family = title_family, size = rel(1.72), colour = col_ink,
      lineheight = 1.05, margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      family = text_family, size = rel(0.80), colour = col_muted,
      lineheight = 1.3, margin = margin(b = 16)
    ),
    plot.caption = element_textbox_simple(
      family = cap_family, size = rel(0.45), colour = col_muted,
      lineheight = 1.3, margin = margin(t = 14), width = unit(1, "npc")
    ),
    plot.margin = margin(16, 24, 12, 20)
  )
)

theme_set(weekly_theme)

### |- main plot ----
p <- ggplot(plot_df, aes(x = share, y = state)) +
  geom_col(aes(fill = focus), width = 0.72) +
  geom_text(
    data = label_df,
    aes(label = label, colour = focus),
    hjust = 0, nudge_x = 0.004,
    family = text_family, fontface = "bold", size = 3.3
  ) +
  geom_text(
    data = weight_df,
    aes(label = label),
    x = 0.004, hjust = 0, colour = "white",
    family = text_family, size = 3.1
  ) +
  scale_fill_manual(values = c(MN = col_mn, NC = col_nc, other = col_context)) +
  scale_colour_manual(values = c(MN = col_mn, NC = col_nc_text)) +
  scale_x_continuous(
    limits = c(0, 0.20), breaks = seq(0, 0.20, 0.05),
    labels = label_percent(), expand = expansion(mult = c(0, 0.20))
  ) +
  scale_y_discrete(limits = state_levels) +
  coord_cartesian(clip = "off") +
  facet_wrap(~measure, nrow = 1) +
  labs(title = title_text, subtitle = subtitle_text, caption = caption_text)

### |- Preview ----
p + ggview::canvas(width = fig_w, height = fig_h, units = "in")

### |- save ----
out_file <- here::here("2026/Week_46/2026_46.png")

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
#    • rnassqs::nassqs()                       - USDA NASS Quick Stats API
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
#   3. Run the script as-is (the NASS pull needs a free Quick Stats key)
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
# version  R version 4.6.1 (2026-06-24)
# os       macOS Tahoe 26.7.3
# system   aarch64, darwin23
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       America/New_York
# date     2026-11-17
# rstudio  2026.11.0+203 Montauk Daisy (desktop)
# pandoc   NA
# quarto   1.9.38 @ /usr/local/bin/quarto
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────────────────────────────
# ! package      * version date (UTC) lib source
# base         * 4.6.1   2026-06-25 [?] local
# cellranger     1.1.0   2016-07-27 [1] CRAN (R 4.6.0)
# cli            3.6.6   2026-04-09 [1] CRAN (R 4.6.0)
# commonmark     2.0.0   2025-07-07 [1] CRAN (R 4.6.0)
# P compiler       4.6.1   2026-06-25 [1] local
# curl           7.1.0   2026-04-22 [1] CRAN (R 4.6.0)
# P datasets     * 4.6.1   2026-06-25 [1] local
# digest         0.6.39  2025-11-19 [1] CRAN (R 4.6.0)
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
# rematch        2.0.0   2023-08-30 [1] CRAN (R 4.6.0)
# rlang          1.3.0   2026-07-05 [1] CRAN (R 4.6.1)
# rnassqs      * 0.6.3   2024-08-30 [1] CRAN (R 4.6.0)
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
# ────────────────────────────────────────────────────────────────────────────────────────────────────────────
# > 