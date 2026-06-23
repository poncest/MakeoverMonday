## Challenge: #MakeoverMonday 2026 week 25
## Data:      UFO Sightings (NUFORC)
## Author:    Steven Ponce
## Date:      2026-06-23

## Article
# https://www.kaggle.com/datasets/joebeachcapital/ufo-sightings

## Data
# https://data.world/makeovermonday/2026w25-ufo-sightings

## NOTE: Vocabulary spectrum (orb / circle / sphere) — the round-word family as a
##       controlled comparison: one geometry, three names, sorted by daylight.
##       Uses custom helper functions; see HELPER FUNCTIONS DOCUMENTATION below.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  scales,        # Scale Functions for Visualization
  glue,          # Interpreted String Literals
  binom          # Binomial Confidence Intervals (Wilson)
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
df_raw <- read_csv("data/2026/MM2026 W25 UFO Sightings.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
# Snapshot, not a time series. Signal = daylight share by reported form.
# The round-word family (orb/circle/sphere) is the only clean minimal-pair set:
# same geometry, three names, differing in luminosity -> the controlled comparison.


## 4. TIDY DATA ----
day_start <- 7
day_end   <- 18

sightings <- df_raw |>
  filter(!is.na(time)) |>
  mutate(
    hour     = as.numeric(time) / 3600,
    daylight = hour >= day_start & hour < day_end
  )

overall_day <- mean(sightings$daylight)        # ~0.282 orientation baseline

round_data <- sightings |>
  filter(shape %in% c("Orb", "Circle", "Sphere")) |>
  summarise(n = n(), day = sum(daylight), .by = shape)

ci <- binom::binom.wilson(round_data$day, round_data$n)

round_data <- round_data |>
  mutate(
    day_share = ci$mean,
    lower     = ci$lower,
    upper     = ci$upper,
    pole      = case_when(
      shape == "Orb"    ~ "after dark",
      shape == "Sphere" ~ "by daylight",
      TRUE              ~ ""
    )
  ) |>
  arrange(day_share)

# annotation values DERIVED from the frame (never hardcode)
get_r       <- function(s) round_data$day_share[round_data$shape == s]
orb_pct     <- percent(get_r("Orb"),    accuracy = 1)
sphere_pct  <- percent(get_r("Sphere"), accuracy = 1)
ratio_round <- round(get_r("Sphere") / get_r("Orb"), 1)         # ~2.2
n_total     <- sum(round_data$n)                                 # ~305
n_orb    <- round_data$n[round_data$shape == "Orb"]
n_circle <- round_data$n[round_data$shape == "Circle"]
n_sphere <- round_data$n[round_data$shape == "Sphere"]

# print(round_data)   # relock annotation coords after first snap()


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
clrs <- get_theme_colors(
  palette = list(
    luminous = "#722F37",   # burgundy — the glowing / after-dark word
    neutral  = "#908981",   # warm gray — the neutral middle word
    solid    = "#3F3A36",   # near-black — the solid / by-daylight word
    ink      = "#2C2825",
    ref_grey = "#6F6A63",
    ci_grey  = "#D7D2CA"
  ) 
)
ink      <- clrs$palette$ink
ref_grey <- clrs$palette$ref_grey
ci_grey  <- clrs$palette$ci_grey
bg_col   <- clrs$background

round_pal <- c(
  "Orb"    = clrs$palette$luminous,
  "Circle" = clrs$palette$neutral,
  "Sphere" = clrs$palette$solid
)

### |-  titles and caption ----
title_text <- "Darkness changes how UFOs are described"

subtitle_text <- str_glue(
  "<b>Orb</b>, <b>circle</b>, and <b>sphere</b> are three of the words witnesses use for a ",
  "round light \u2014 and which one they reach for tracks<br>how much daylight there was to see it."
)

annotation_text <- str_glue(
  "<b>After dark, witnesses report an <i>orb</i> \u2014 a glow with no visible surface.</b><br>",
  "By day, a <i>sphere</i> \u2014 a lit, solid form.<br>",
  "<span style='color:{ref_grey}'>The same glowing-to-solid split runs across every reported shape.</span>"
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 25,
  source_text = str_glue(
    "National UFO Reporting Center (NUFORC) \u00b7 reports with images \u00b7 data.world<br>",
    "Round-shape reports only: orb ({n_orb}), circle ({n_circle}), sphere ({n_sphere}). ",
    "Daylight = 07:00\u201318:00, fixed window. Points: daylight share with Wilson 95% CIs."
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
    plot.title    = element_markdown(face = "bold", family = fonts$title_1,
                                     size = rel(1.8), color = ink, margin = margin(b = 4)),
    plot.subtitle = element_textbox_simple(family = fonts$subtitle, size = rel(0.8),
                                           color = ink, lineheight = 1.1,
                                           width = unit(1, "npc"), margin = margin(b = 16)),
    plot.caption  = element_textbox_simple(family = fonts$caption, size = rel(0.5),
                                           color = ref_grey, lineheight = 1.3,
                                           width = unit(1, "npc"), margin = margin(t = 14)),
    axis.text.y        = element_blank(),
    axis.title.y       = element_blank(),
    axis.text.x        = element_text(family = fonts$text, size = rel(0.8), color = ref_grey),
    axis.title.x       = element_text(family = fonts$text, size = rel(0.8), color = ref_grey),
    panel.grid         = element_blank(),
    axis.ticks         = element_blank()
  )
)

theme_set(weekly_theme)

### |-  final plot ----
# y geometry on a single track (nudge after first snap())
y_word   <-  0.55
y_pct    <- -0.48
y_anno   <- -1.00   # hero sentence, centered below the track (nudge after snap)
y_reflab <-  1.30

p <- ggplot(round_data, aes(x = day_share, y = 0)) +
  
  # orientation: overall daylight share
  geom_vline(xintercept = overall_day, linetype = "dashed",
             linewidth = 0.4, color = ref_grey) +
  
  # spine: one geometry, a spectrum of names
  geom_line(aes(group = 1), linewidth = 0.7, color = "gray75") +
  
  # faint Wilson CIs (inform without dominating)
  geom_linerange(aes(xmin = lower, xmax = upper), linewidth = 3.2,
                 color = ci_grey, alpha = 0.7) +
  
  geom_point(aes(color = shape), size = 6) +
  
  # word above, share below, pole-note under the two ends only
  geom_text(aes(label = shape), y = y_word, family = fonts$title,
            fontface = "bold", size = 4.4, color = ink) +
  geom_text(aes(label = percent(day_share, accuracy = 1)), y = y_pct,
            family = fonts$text, size = 3.6, color = ink) +
  
  # # overall-line label
  # annotate("text", x = overall_day + 0.006, y = y_reflab,
  #          label = glue("All UFO reports: {percent(overall_day, accuracy = 1)} by day"),
  #          hjust = 0, vjust = 1, family = fonts$text, size = 3, color = ref_grey) +
  # 
  # # the memorable proof, unboxed + muted, in the upper-left whitespace
  # annotate("richtext", x = 0.30, y = y_anno, hjust = 0.5, vjust = 1,
  #          label = annotation_text, family = fonts$text, size = 3.05,
  #          color = ink, lineheight = 1.3, fill = NA, label.color = NA) +
  
  # overall-line label (knockout fill so the dashed line reads behind it)
  annotate("richtext", x = overall_day + 0.006, y = y_reflab,
           label = glue("All UFO reports: {percent(overall_day, accuracy = 1)} by day"),
           hjust = 0, vjust = 1, family = fonts$text, size = 3, color = ref_grey,
           fill = bg_col, label.color = NA,
           label.padding = unit(c(0.12, 0.3, 0.12, 0.2), "lines")) +
  
  # the memorable proof, centered below the track; knockout fill masks the dashed line
  annotate("richtext", x = 0.30, y = y_anno, hjust = 0.5, vjust = 1,
           label = annotation_text, family = fonts$text, size = 3.05,
           color = ink, lineheight = 1.3,
           fill = bg_col, label.color = NA,
           label.padding = unit(c(0.2, 0.3, 0.2, 0.3), "lines")) +
  
  
  
  
  
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 0.6, 0.1), limits = c(0, 0.6),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(-1.75, 1.45), expand = expansion(0)) +
  scale_color_manual(values = round_pal, guide = "none") +
  coord_cartesian(clip = "off") +
  labs(
    x = "Share of that word's reports occurring in daylight",
    y = NULL,
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )

### |-  save ----
snap(p)


## 6. HELPER FUNCTIONS DOCUMENTATION ----
# setup_fonts(), get_font_families(), get_theme_colors(), create_base_theme(),
# extend_weekly_theme(), create_social_caption(), snap()


## 7. SESSION INFO ----
# sessioninfo::session_info()

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-06-23
# rstudio  2026.04.0+526 Globemaster Allium (desktop)
# pandoc   NA
# quarto   ERROR: Unknown command "TMPDIR=C:/Users/poncest/AppData/Local/Temp/Rtmpu0rqk6/file7d90730e7ff6". Did you mean command "install"? @ C:\\Users\\poncest\\AppData\\Local\\Programs\\Quarto\\bin\\quarto.exe
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# binom        * 1.1-1.1  2022-05-02 [1] CRAN (R 4.5.3)
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
# haven          2.5.5    2025-05-30 [1] CRAN (R 4.5.3)
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
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
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
# ──────────────────────────────────────────────────────────────────
