## Challenge: #MakeoverMonday 2026 week 26
## Data:      The World's Space Launch Sites
## Author:    Steven Ponce
## Date:      2026-06-30

## Article
# https://www.voronoiapp.com/geopolitics/Mapped-The-Worlds-Space-Launch-Sites-2016-2026-8434

## App
# https://spacedata.aei.org/space/launches/?metric=launches&groupBy=countryGroup&starlink=include&startYear=2016&endYear=2026&countryGroup=United+States&countryGroup=Russia%2FSoviet+Union&countryGroup=China&countryGroup=NATO+Ally&countryGroup=Major+Non-NATO+Ally&countryGroup=Other&categories=Orbital&categories=Deep+Space&chartMode=stackedBar&mapMode=flat

## Data
# https://data.world/makeovermonday/2026wk26-the-worlds-space-launch-sites

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


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
  width  = 12,
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
df_raw <- read_csv("data/2026/Launches (launches-launches-table-2026-06-29)_Launches.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----

### |- filter to US + China, 2016–2026 ----
df <- df_raw |>
  filter(
    year(launch_date) >= 2016,
    country_group %in% c("United States", "China")
  ) |>
  mutate(
    year = year(launch_date),
    group = case_when(
      country_group == "United States" & !is.na(starlink_mission) ~ "Starlink launches",
      country_group == "United States" & is.na(starlink_mission) ~ "U.S. (without Starlink)",
      country_group == "China" ~ "China"
    )
  )

### |- Panel 1: annual bars (China + US non-SL) and US total dots ----
annual_counts <- df |>
  count(year, group) |>
  complete(year, group, fill = list(n = 0L))

annual_us_total <- df |>
  filter(country_group == "United States") |>
  count(year, name = "us_total")

annual_bars <- annual_counts |>
  filter(group != "Starlink launches") |>
  mutate(group = factor(group, levels = c("China", "U.S. (without Starlink)")))

### |- Panel 2: cumulative — China, US non-Starlink, US Total (dashed reference only) ----
cumulative_lines <- df |>
  filter(group %in% c("China", "U.S. (without Starlink)")) |>
  count(year, group) |>
  complete(year, group, fill = list(n = 0L)) |>
  arrange(group, year) |>
  group_by(group) |>
  mutate(cumulative = cumsum(n)) |>
  ungroup()

us_total_line <- df |>
  filter(country_group == "United States") |>
  count(year) |>
  arrange(year) |>
  mutate(
    group      = "Total U.S. (with Starlink)",
    cumulative = cumsum(n)
  )

cumulative_all <- bind_rows(cumulative_lines, us_total_line)

### |- annotation coordinates (pre-extracted named scalars) ----
p2_china_end <- cumulative_all |> filter(year == 2026, group == "China") |> pull(cumulative)
p2_us_ns_end <- cumulative_all |> filter(year == 2026, group == "U.S. (without Starlink)") |> pull(cumulative)
p2_us_tot_end <- cumulative_all |> filter(year == 2026, group == "Total U.S. (with Starlink)") |> pull(cumulative)
p2_gap <- p2_china_end - p2_us_ns_end          
p2_us_lead <- p2_us_tot_end - p2_china_end         
p2_starlink_n  <- cumulative_all |>                    
  filter(year == 2026) |>
  summarise(sl = p2_us_tot_end - p2_us_ns_end) |>
  pull(sl)
p2_bracket_mid <- (p2_china_end + p2_us_ns_end) / 2


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
col_china <- "#722F37"
col_us_ns <- "#2A6496"
col_starlink <- "#AAAAAA"
col_us_total <- "#BBBBBB"
col_annotation <- "#333333"
col_subtitle <- "#555555"
col_bg <- "#FAF8F4"
col_grid <- "#E8E4DE"

clrs <- get_theme_colors(palette = list(
  china      = col_china,
  us_ns      = col_us_ns,
  starlink   = col_starlink,
  annotation = col_annotation,
  background = col_bg
))

### |- titles and captions ----
title_text    <- "The U.S. Launch Lead Is a Starlink Story"

subtitle_text <- str_glue(
  "Starlink alone exceeds the U.S. lead over China. Without those launches, ",
  "<span style='color:{col_china}'>**China**</span> ",
  "has led <span style='color:{col_us_ns}'>**the U.S.**</span> ",
  "every year since 2018."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 26,
  source_text = "AEI Space Data Center · Data: 2016–2026* through June 26, 2026<br>
  Note: Orbital and deep space launches only. Starlink identified via AEI mission flag."
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- theme ----
base_theme   <- create_base_theme(clrs)
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 8, color = col_annotation,
      margin = margin(r = 6)
    ),
    axis.text = element_text(size = 8, color = col_annotation),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.background = element_rect(fill = col_bg, color = NA),
    legend.text = element_text(size = 8.5, color = col_annotation),
    legend.key.width = unit(1.2, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.spacing.x = unit(0.5, "lines"),
    plot.margin = margin(t = 6, r = 10, b = 4, l = 6)
  )
)

theme_set(weekly_theme)

### |- Panel 1: grouped bars + gray context dots ----

# First dot only gets a label — 2016
p1_dot_label <- annual_us_total |> filter(year == 2016)

p1 <- ggplot() +

  # Geoms
  geom_col(
    data = annual_bars,
    aes(x = year, y = n, fill = group),
    position = position_dodge(width = 0.72),
    width = 0.65, alpha = 0.92
  ) +
  geom_point(
    data = tibble(year = NA_real_, us_total = NA_real_, dot_label = "U.S. total (incl. Starlink)"),
    aes(x = year, y = us_total, color = dot_label),
    size = 2.2, shape = 16, alpha = 0.65
  ) +
  geom_point(
    data = annual_us_total,
    aes(x = year, y = us_total),
    color = col_starlink, size = 2.2, shape = 16, alpha = 0.65,
    show.legend = FALSE
  ) +
  geom_text(
    data = p1_dot_label,
    aes(
      x = year, y = us_total,
      label = "U.S. total\n(incl. Starlink)"
    ),
    nudge_y = 9, size = 2.2, color = col_starlink,
    lineheight = 1.15, hjust = 0.5, vjust = 0
  ) +
  # Scales
  scale_fill_manual(
    values = c(
      "China"                   = col_china,
      "U.S. (without Starlink)" = col_us_ns
    ),
    guide = guide_legend(
      nrow = 1, order = 1,
      override.aes = list(alpha = 1, shape = NA)
    )
  ) +
  scale_color_manual(
    values = c("U.S. total (incl. Starlink)" = col_starlink),
    guide = guide_legend(
      nrow = 1, order = 2,
      override.aes = list(
        shape = 16, size = 2.8, alpha = 0.85,
        linetype = 0, fill = NA
      )
    )
  ) +
  scale_x_continuous(
    breaks = 2016:2026,
    labels = c(as.character(2016:2025), "2026*"),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_continuous(
    limits = c(0, 280),
    breaks = seq(0, 250, 50),
    expand = expansion(mult = c(0, 0.14))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "**1** &nbsp; Annual Launches",
    subtitle = "China has led U.S. non-Starlink launches every year since 2018.",
    y = "Number of launches",
    fill = NULL,
    color = NULL
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = 10, color = col_annotation,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 8, color = col_subtitle,
      lineheight = 1.3, margin = margin(b = 6)
    )
  )

### |- Panel 2: cumulative lines ----

p2 <- ggplot(
  cumulative_all |>
    mutate(group = factor(group, levels = c(
      "China", "U.S. (without Starlink)", "Total U.S. (with Starlink)"
    ))),
  aes(
    x = year, y = cumulative,
    color = group, linetype = group, linewidth = group
  )
) +
  # Geoms
  geom_line() +
  geom_point(
    data = cumulative_all |>
      filter(group %in% c("China", "U.S. (without Starlink)")),
    size = 2.4, show.legend = FALSE
  ) +

  # Annotate
  annotate("segment",
    x = 2026.35, xend = 2026.35,
    y = p2_us_ns_end, yend = p2_china_end,
    color = col_annotation, linewidth = 0.5
  ) +
  annotate("segment",
    x = 2026.22, xend = 2026.35,
    y = p2_china_end, yend = p2_china_end,
    color = col_annotation, linewidth = 0.5
  ) +
  annotate("segment",
    x = 2026.22, xend = 2026.35,
    y = p2_us_ns_end, yend = p2_us_ns_end,
    color = col_annotation, linewidth = 0.5
  ) +
  annotate("text",
    x = 2026.5, y = p2_bracket_mid,
    label = glue("+{p2_gap}"),
    hjust = 0, vjust = 0.5, size = 3.2,
    color = col_annotation, fontface = "bold"
  ) +
  annotate("text",
    x = 2020.2, y = 845,
    label = glue("Total U.S. (with Starlink): {p2_us_tot_end}"),
    hjust = 0, vjust = 0.5, size = 2.5,
    color = col_us_total, fontface = "italic"
  ) +
  annotate("text",
    x = 2020.2, y = 775,
    label = glue(
      "U.S. leads China by +{p2_us_lead} total  \u2022  Starlink alone: {p2_starlink_n}"
    ),
    hjust = 0, vjust = 0.5, size = 2.4,
    color = col_us_total, fontface = "italic"
  ) +
  annotate("text",
    x = 2026.1, y = p2_china_end + 14,
    label = as.character(p2_china_end),
    hjust = 0, vjust = 0, size = 4.4,
    color = col_china, fontface = "bold"
  ) +
  annotate("text",
    x = 2026.1, y = p2_us_ns_end - 14,
    label = as.character(p2_us_ns_end),
    hjust = 0, vjust = 1, size = 4.4,
    color = col_us_ns, fontface = "bold"
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "China" = col_china,
      "U.S. (without Starlink)" = col_us_ns,
      "Total U.S. (with Starlink)" = col_us_total
    ),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c(
      "China" = "solid",
      "U.S. (without Starlink)" = "solid",
      "Total U.S. (with Starlink)" = "dashed"
    ),
    guide = "none"
  ) +
  scale_linewidth_manual(
    values = c(
      "China" = 1.3,
      "U.S. (without Starlink)" = 1.3,
      "Total U.S. (with Starlink)" = 0.8
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 2016:2026,
    labels = c(as.character(2016:2025), "2026*"),
    expand = expansion(mult = c(0.02, 0.22))
  ) +
  scale_y_continuous(
    limits = c(0, 920),
    breaks = seq(0, 900, 100),
    expand = expansion(mult = c(0, 0.04))
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = "**2** &nbsp; Cumulative Launches",
    subtitle = glue("By mid-2026, China leads U.S. non-Starlink launches by {p2_gap} missions."),
    y = "Cumulative launches"
  ) +
  # Theme
  theme(
    plot.title = element_markdown(
      size = 10, color = col_annotation,
      margin = margin(b = 3)
    ),
    plot.subtitle = element_text(
      size = 8.5, color = col_subtitle,
      lineheight = 1.3, margin = margin(b = 6)
    )
  )

### |- combine plots ----
p_combined <- (p1 | p2) +
  plot_layout(widths = c(1.15, 0.85)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.85), face = "bold", family = fonts$title_1,
        color = col_annotation,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.8),
        color = col_subtitle, family = fonts$subtitle,
        lineheight = 1.35,
        margin = margin(b = 16)
      ),
      plot.caption = element_markdown(
        size = rel(0.5),
        color = alpha(col_annotation, 0.60),
        hjust = 0,
        lineheight = 1.3,
        margin = margin(t = 8), family = fonts$caption
      ),
      plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
      plot.background = element_rect(fill = col_bg, color = NA)
    )
  )

### |-  Preview ----
snap(p_combined)


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
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-29
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# bit            4.6.0    2025-03-06 [1] CRAN (R 4.5.3)
# bit64          4.6.0-1  2025-01-16 [1] CRAN (R 4.5.3)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# crayon         1.5.3    2024-06-20 [1] CRAN (R 4.5.3)
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.3    2026-04-22 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# P parallel       4.5.3    2026-03-11 [2] local
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R.cache        0.17.0   2025-05-02 [1] CRAN (R 4.5.3)
# R.methodsS3    1.8.2    2022-06-13 [1] CRAN (R 4.5.2)
# R.oo           1.27.1   2025-05-02 [1] CRAN (R 4.5.2)
# R.utils        2.13.0   2025-02-24 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# styler         1.11.0   2025-10-13 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# vroom          1.7.1    2026-03-31 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────