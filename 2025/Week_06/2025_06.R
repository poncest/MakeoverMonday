
## Challenge: #MakeoverMondnay 2025 week 06
## Data:      National Grid: Live
## Author:    Steven Ponce
## Date:      2025-02-02

## Original Chart 
# National Grid: Live
# The National Grid is the electric power transmission
# network for Great Britain
# link https://grid.iamkate.com/


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
    camcorder,      # Record Your Plot History 
    patchwork,      # The Composer of Plots
    gghighlight     # Highlight Lines and Points in 'ggplot2'
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  8,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
energy_raw <- read_csv(
  file = 'data/2025/National_Grid_Energy_Breakdown.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(energy_raw)
skim(energy_raw)


## 4. TIDYDATA ----

### |-  tidy data ----
energy_clean <- energy_raw |> 
  rename_with(~str_remove(., pattern = "_gw"), everything()) |> 
  mutate(date = dmy(date)) |> 
  # Pivot longer
  pivot_longer(
    cols = !date,
    names_to = "metric",
    values_to = "demand_gw"
  ) |> 
  mutate(
    metric = case_when(
      metric == "fossil_fuels" ~ "fossil fuels",
      metric == "other_sources" ~ "other sources",
      TRUE ~ metric
    ),
    metric = str_to_title(metric),
    metric = factor(metric,
                    levels = c("Demand", 
                               "Transfers", 
                               "Fossil Fuels", 
                               "Renewables", 
                               "Other Sources")
    )
  ) 


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c("#4B79B7", "#F8F9FA", "#2C3E50", "#34495E", "#7F8C8D"))

### |-  titles and caption ----
title_text <- str_glue("Britain's Power Mix: A Daily Energy Snapshot")
subtitle_text <- str_glue("Tracking Great Britain's electricity demand and generation sources across<br>
                          the National Grid | Data from National Grid: Live")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 06,
    source_text = "Great Britain National Grid: Live"
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
    axis.line.x           = element_line(color = "#252525", linewidth = .2),
    
    panel.spacing.x       = unit(2, 'lines'),
    panel.spacing.y       = unit(1, 'lines'),
    panel.grid.major.x    = element_blank(),
    panel.grid.major.y    = element_line(color = alpha(colors[5], 0.2), linewidth = 0.2),
    panel.grid.minor      = element_blank(),
    
    strip.text          = element_textbox(
      size              = rel(0.9),
      face              = 'bold',
      color             = colors$palette[3],
      fill              = alpha(colors$palette[1], 0.1),
      box.color         = alpha(colors[1], 0.5),
      halign            = 0.5,
      linetype          = 1,
      r                 = unit(3, "pt"),
      width             = unit(1, "npc"),
      padding           = margin(5, 10, 5, 10),
      margin            = margin(b = 10)
    ),
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(energy_clean,
       aes(x = date, y = demand_gw, group = metric)) +
  # Geoms
  geom_line(linewidth = 0.3, alpha = 0.2) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(
      linewidth = 0.3,
      alpha = 0.45,
      color = 'gray20'
    )
  ) +
  geom_line(color = colors$palette[1], linewidth = 1.2) +
  
  # Scales
  scale_x_date(
    breaks = "3 month",               
    labels = label_date_short(),
    limits = c(min(energy_clean$date), max(energy_clean$date))  
  ) +
  scale_y_continuous(breaks = pretty_breaks()) +
  coord_cartesian(clip = 'off') +
  
  # Labs
  labs(
    x = NULL,
    y = "Energy (GW)",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) +
  
  # Facet
  facet_wrap(~metric, ncol = 2) +
  
  # Theme
  theme(
    plot.title = element_text(
      size   = rel(2),
      family = fonts$title,
      face   = "bold",
      color  = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size   = rel(0.95),
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

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-02
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────
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
# gghighlight * 0.4.1    2023-12-16 [1] RSPM (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────────
# 
# >