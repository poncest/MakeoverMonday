
## Challenge: #MakeoverMondnay 2025 week 12
## Data:      Precipitation Anomalies
## Author:    Steven Ponce
## Date:      2025-03-18

## Original Chart
# Precipitation Anomalies
# https://ourworldindata.org/grapher/global-precipitation-anomaly

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    janitor,        # Simple Tools for Examining and Cleaning Dirty Data
    skimr,          # Compact and Flexible Summaries of Data
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    lubridate,      # Make Dealing with Dates a Little Easier
    ggridges,       # Ridgeline Plots in 'ggplot2' # Ridgeline Plots in 'ggplot2'
    patchwork,      # The Composer of Plots
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  12,
    height =  10,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

#' The raw data for the week MakeoverMonday challenge can be downloaded 
#' https://data.world/makeovermonday/2025w12-precipitation-anomalies
#' 
#' Article
#' https://ourworldindata.org/grapher/global-precipitation-anomaly

precipitation_raw <- read_csv('data/2025/global-precipitation-anomaly.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(precipitation_raw)
skim(precipitation_raw)


## 4. TIDYDATA ----

### |-  tidy data ----

# Define El Niño years based on moderate to strong events
el_nino_years <- c(1982, 1983, 1987, 1988, 1991, 1992, 1997, 1998, 
                   2002, 2003, 2009, 2010, 2015, 2016)

# ENSO (El Niño Southern Oscillation) data
enso_data <- precipitation_raw |>
  filter(entity %in% c('World', 'Brazil', 'Australia', 'India', 'United States')) |>
  mutate(
    el_nino = year %in% el_nino_years,
    event_type = if_else(el_nino, 'El Niño Year', 'Normal Year')
  )

# Comparative statistics for El Niño vs Normal
comparative_stats <- enso_data |>
  group_by(entity, event_type) |>
  summarize(
    mean_anomaly = mean(annual_precipitation_anomaly, na.rm = TRUE),
    median_anomaly = median(annual_precipitation_anomaly, na.rm = TRUE),
    sd_anomaly = sd(annual_precipitation_anomaly, na.rm = TRUE),
    n_obs = n(),
    se_anomaly = sd_anomaly / sqrt(n_obs),
    ci_lower = mean_anomaly - 1.96 * se_anomaly,
    ci_upper = mean_anomaly + 1.96 * se_anomaly,
    .groups = 'drop'
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  'El Niño Year' = '#FF9800',
  'Normal Year' = '#2196F3'
))
  
### |-  titles and caption ----
title_text <- str_glue("El Niño Impact on Regional Precipitation")
subtitle_text <- str_glue("Distribution patterns (left) and mean values with confidence intervals (right)")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 12,
    source_text = "Our World in Data"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----

# Start with base theme
base_theme <- create_base_theme(colors)

# Add weekly-specific theme elements
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Weekly-specific modifications
    legend.position = "plot",

    axis.title = element_text(size = rel(1.14)),  
    axis.text = element_text(size = rel(1)),  

    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5), 
    axis.ticks.length = unit(0.2, "cm"), 

    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),  
    panel.spacing.y = unit(0, "lines"),
    
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----

# P1. Ridges Chart ----
p1 <- ggplot(enso_data, aes(x = annual_precipitation_anomaly, y = entity, fill = event_type)) +
  # Geoms
  geom_density_ridges(
    scale = 0.9, 
    alpha = 0.7, 
    quantile_lines = TRUE,
    quantiles = c(0.5),  
    jittered_points = FALSE
  ) +
  geom_rug(
    aes(color = event_type), 
    alpha = 0.3, 
    size = 0.1,
    sides = "b",
    position = position_nudge(y = -0.2)  
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Scales
  scale_x_continuous() +
  scale_y_discrete() +
  scale_fill_manual(values = colors$palette) +
  scale_color_manual(values = colors$palette) +
  coord_cartesian(clip = 'off') +
  # Labs
  labs(
    x = 'Anomaly (mm)',
    y = NULL,
    fill = 'Year Type',
  ) 
  

# P2. Dot Plot ---- 
p2 <-ggplot(comparative_stats, aes(x = mean_anomaly, y = entity, color = event_type)) +
  # Geoms
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.7) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper),
    height = 0.3,
    size = 0.8,
    alpha = 0.7
  ) +
  geom_point(size = 3) +
  # Scales
  scale_x_continuous() +
  scale_y_discrete() +
  scale_color_manual(
    values = colors$palette
  ) +
  coord_cartesian(clip = 'off') +
  # Labs
  labs(
    x = "Mean Precipitation Anomaly (mm)",
    y = NULL,
  ) 


# Combined Plots ----
combined_plot <- (p1 | plot_spacer() | p2) +
  plot_layout(widths = c(1, 0.05, 1), nrow = 1) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size   = rel(2.6),
        family = fonts$title,
        face   = "bold",
        color  = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size   = rel(1.1),
        family = fonts$subtitle,
        color  = colors$subtitle,
        lineheight = 1.2,
        margin = margin(t = 5, b = 15)
      ),
      plot.caption = element_markdown(
        size   = rel(0.75),
        family = fonts$caption,
        color  = colors$caption,
        hjust  = 0.5,
        margin = margin(t = 10)
      ),
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
    )
  )


combined_plot



# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-18
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder   * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark    1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
# curl          6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] RSPM (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# evaluate      1.0.3    2025-01-10 [1] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
# fastmap       1.2.0    2024-05-15 [1] RSPM (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] RSPM (R 4.4.0)
# generics      0.1.3    2022-07-05 [1] RSPM (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
# P ggridges    * 0.5.6    2024-01-23 [?] RSPM (R 4.4.0)
# ggtext      * 0.1.2    2022-09-16 [1] RSPM (R 4.4.0)
# gifski        1.32.0-1 2024-10-13 [1] RSPM (R 4.4.1)
# glue        * 1.8.0    2024-09-30 [1] RSPM (R 4.4.0)
# P graphics    * 4.4.0    2024-04-24 [?] local
# P grDevices   * 4.4.0    2024-04-24 [?] local
# P grid          4.4.0    2024-04-24 [?] local
# gridtext      0.1.5    2022-09-16 [1] RSPM (R 4.4.0)
# gtable        0.3.6    2024-10-25 [1] RSPM (R 4.4.0)
# here        * 1.0.1    2020-12-13 [1] RSPM (R 4.4.0)
# hms           1.1.3    2023-03-21 [1] RSPM (R 4.4.0)
# htmltools     0.5.8.1  2024-04-04 [1] RSPM (R 4.4.0)
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] RSPM (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# renv          1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# repr          1.1.7    2024-03-22 [1] RSPM (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot     2.0.4    2023-11-05 [1] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg          2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# showtext    * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb  * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
# skimr       * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
# snakecase     0.11.1   2023-08-27 [1] RSPM (R 4.4.0)
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
# stringr     * 1.5.1    2023-11-14 [1] RSPM (R 4.4.0)
# svglite       2.1.3    2023-12-08 [1] RSPM (R 4.4.0)
# sysfonts    * 0.8.9    2024-03-02 [1] RSPM (R 4.4.0)
# systemfonts   1.2.1    2025-01-20 [1] RSPM (R 4.4.0)
# textshaping   1.0.0    2025-01-20 [1] RSPM (R 4.4.0)
# tibble      * 3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# timechange    0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# tzdb          0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# utf8          1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────
# > 