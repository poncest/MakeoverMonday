
## Challenge: #MakeoverMondnay 2025 week 09
## Data:      World Population Estimates & Projections
## Author:    Steven Ponce
## Date:      2025-02-23

## Original Chart 
# World Population Estimates & Projections
# World Bank Group
# https://data.worldbank.org/indicator/SP.POP.GROW?name_desc=false


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
    readxl,         # Read Excel Files
    ggHoriPlot,     # Horizon Plots for 'ggplot2'
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

#' The raw data for the week MakeoverMonday challenge can bw downloaded 
#' here: https://data.world/makeovermonday/2025w9-world-population-estimates-projections
#' 
#' Due to GitHub file size limitation, filter the inital dataset for
#' the `"Population growth (annual %)"` indicator only.
#' The working csv file was saved as `population_growth_raw.csv`

# Initial raw data
# population_estimates_raw <- read_excel(
#   'data/2025/Population Estimates.xlsx') |> 
#   clean_names()

# Subset for population_growth only
# population_estimates_raw$indicator_name |> unique() |> sort()

# populatio_growth_raw <- population_estimates_raw |> 
#   filter(indicator_name == "Population growth (annual %)") 

# write_csv(x = population_growth_raw, file = 'data/2025/population_growth_raw.csv')

population_growth_raw <- read_csv('data/2025/population_growth_raw.csv') |> 
  clean_names()

## 3. EXAMINING THE DATA ----
glimpse(population_growth_raw)
skim(population_growth_raw)


## 4. TIDYDATA ----

### |-  tidy data ----

population_growth_tidy <- population_growth_raw |> 
  pivot_longer(
    cols = starts_with("x"),     # Select all columns starting with "x"
    names_to = "year",           # Create a new column called "year"
    names_prefix = "x",          # Remove the "x" from the column names
    values_to = "growth_rate"
    ) |>  
  filter(str_detect(country_name, "^(Africa|Asia|Europe|Latin America|North America)")) |> 
  mutate(
    year = as.numeric(year),
    country_name = case_when(
      country_name == "Africa Eastern and Southern" ~ "Eastern & Southern Africa",
      country_name == "Africa Western and Central" ~ "Western & Central Africa",
      country_name == "Latin America & Caribbean" ~ "Latin America & Caribbean",
      TRUE ~ country_name
    ),
    country_name = str_wrap(country_name, width = 30),
    # Order regions for better visual flow
    country_name = factor(country_name, levels = rev(unique(country_name)))
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "#31a354",  # High Growth 
  "#78c679",  # Medium Growth 
  "#d9f0a3",  # Low Growth 
  "#fdae61",  # Low Decline 
  "#f16913",  # Medium Decline 
  "#d73027"   # High Decline 
  )
)
  
### |-  titles and caption ----
title_text <- str_glue("Population Growth Patterns Across Regions")
subtitle_text <- str_wrap("Visualizing growth rate trends from 1960 to 2050. Darker shades represent stronger growth (green) or decline (red), with the horizon line indicating zero growth.", width = 90)

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 09,
    source_text = "World Population Estimates & Projections"
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
    legend.position = "top",
    legend.title = element_text(size = rel(0.79)),
    legend.text = element_text(size = rel(0.71)),
    
    axis.title = element_text(size = rel(1.14)),  
    axis.text = element_text(size = rel(0.86)),  
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", size = 0.5), 
    axis.ticks.length = unit(0.2, "cm"), 
    
    strip.text.y = element_text(size = rel(0.7), angle = 0), 
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "lines"),  
    panel.spacing.y = unit(0, "lines"),
    
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(data = population_growth_tidy) + 
  
  # Geoms
  geom_horizon(
    aes(x = year, y = growth_rate),
    origin = "midpoint", 
    horizonscale = 6, 
    show.legend = TRUE
  ) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(1960, 2050, by = 20),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous() +
  scale_fill_manual(
    values = colors$palette,
    name = "Growth Rate",
    labels = c("High Growth", "Medium Growth", "Low Growth",
               "Low Decline", "Medium Decline", "High Decline")
  ) +
  
  # Labs
  labs(
    x = "Year",
    y = NULL,
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  
  # Facet
  facet_grid(country_name ~ ., scales = "free_y", space = "free_y") +
  
  # Theme
  theme(
    plot.title = element_text(
      size   = rel(2.5),
      family = fonts$title,
      face   = "bold",
      color  = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
      size   = rel(1.0),
      family = fonts$subtitle,
      color  = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_markdown(
      size   = rel(0.6),
      family = fonts$caption,
      color  = colors$caption,
      hjust  = 0.5,
      margin = margin(t = 10)
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-23
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder   * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cellranger    1.1.0    2016-07-27 [1] RSPM (R 4.4.0)
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
# P ggHoriPlot  * 1.0.1    2022-10-11 [?] RSPM (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
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
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# readxl      * 1.4.3    2023-07-06 [1] RSPM (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────
# > 
