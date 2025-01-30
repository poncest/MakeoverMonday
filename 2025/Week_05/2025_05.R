
## Challenge: #MakeoverMondnay 2025 week 05
## Data:      World Happiness Report 2024
## Author:    Steven Ponce
## Date:      2025-01-29

## Original Chart 
# Figure 2.1: Country Rankings by Life Evaluations in 2021-2023
# tab 2 Explained by six factors
# link https://worldhappiness.report/ed/2024/happiness-of-the-younger-the-older-and-those-in-between/#ranking-of-happiness-2021-2023
  

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
    camcorder,      # Record Your Plot History 
    readxl,         # Read Excel Files
    ggbeeswarm,     # Categorical Scatter (Violin Point) Plots 
    scico,          # Colour Palettes Based on the Scientific Colour-Maps 
    ggrepel         # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
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
world_happiness_2024 <- read_excel(path = 'data/DataForFigure2.1 with sub bars 2024.xlsx') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(world_happiness_2024)
skim(world_happiness_2024)


## 4. TIDYDATA ----

### |-  tidy data ----
happiness_long <- world_happiness_2024 |>
  select(
    # Rename columns
    country_name,
    "Happiness Score" = ladder_score,
    "GDP" = explained_by_log_gdp_per_capita,
    "Social Support" = explained_by_social_support,
    "Health" = explained_by_healthy_life_expectancy,
    "Freedom" = explained_by_freedom_to_make_life_choices,
    "Generosity" = explained_by_generosity,
    "Low Corruption" = explained_by_perceptions_of_corruption
  ) |>
  # Pivot longer
  pivot_longer(
    -country_name,
    names_to = "metric",
    values_to = "value"
  ) |>
  # add metric mea
  group_by(metric) |>
  mutate(
    metric_mean = mean(value, na.rm = TRUE)
  ) |>
  ungroup() |>
  # Factor reorder by mean
  mutate(
    metric = fct_reorder(metric, metric_mean, .desc = TRUE)
  )

# data labels df
label_data <- bind_rows(
  # Get minimum values
  happiness_long |>
    group_by(metric) |>
    slice_min(order_by = value, n = 1, with_ties = FALSE),
  
  # Get maximum values
  happiness_long |>
    group_by(metric) |>
    slice_max(order_by = value, n = 1, with_ties = FALSE)
  ) |>
  ungroup() |>
  distinct(metric, country_name, value)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors()

### |-  titles and caption ----
title_text <- str_glue("World Happiness Report 2024")

subtitle_text <- str_glue("Figure 2.1: Distribution of Contributing Factors to Life Evaluations")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 05,
    source_text = "World Happiness Report Data Dashboard | The World Happiness Report"
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
      # Legend
      legend.position = c(0.8, 1.1),    
      legend.justification = c(0.5, 1),
      legend.margin = margin(b = 5),
      legend.title = element_text(size = rel(0.7)),
      legend.text = element_text(size = rel(0.6)),
      legend.direction = "horizontal",
      
      # Axis formatting
      axis.title = element_text(color = colors$text, size = rel(1), face = "bold"),
      axis.text.y = element_text(color = colors$text, size = rel(0.95), face = "bold"),
      
      # Grid customization
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
    
      # Plot margins 
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(happiness_long, aes(x = metric, y = value)) +
  # Geom
  geom_hline(
    yintercept = seq(0, 8, by = 2),
    color = "gray90",
    linewidth = 0.5
  ) +
  geom_quasirandom(
    aes(color = value),
    width = 0.4,
    size = 2.5,
    alpha = 0.7,
    shape = 21,
    stroke = 0.5
  ) +
  geom_text_repel(
    data = label_data,
    aes(label = country_name),
    size = 3,
    fontface = "plain",
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray40",
    segment.size = 0.3,
    min.segment.length = 0,
    seed = 123,           
    direction = "both",   
    force = 1,           
    force_pull = 0.5     
  ) +
  geom_point(                     # Mean indicators
    data = distinct(happiness_long, metric, metric_mean),
    aes(y = metric_mean),
    color = "red",
    fill = "white",
    size = 3,
    shape = 23
  ) +
  # Scales
  scale_x_discrete() +
  scale_y_continuous(
    expand = expansion(mult = c(0.35, 0)),
    breaks = seq(0, 8, by = 2),
    limits = c(0, 9)) +
  scale_color_scico(
    palette = "roma",
    direction = -1,
    guide = guide_colorbar(
      title.position = "top",
      barwidth = 10,
      barheight = 1,
      ticks.linewidth = 1,
      title.hjust = 0.5
    )
  ) +
  coord_flip(clip = 'off') +
  # Labs
  labs(
    x = NULL,
    y = "Value",
    color = "Value",
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
  ) +
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
    plot.subtitle = element_text(
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

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-01-29
# rstudio  2024.12.0+467 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# P beeswarm      0.4.0    2021-06-01 [?] CRAN (R 4.4.0)
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
# P ggbeeswarm  * 0.7.2    2023-04-29 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
# P ggrepel     * 0.9.6    2024-09-07 [?] RSPM (R 4.4.0)
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
# P scico       * 1.5.0    2023-08-14 [?] RSPM (R 4.4.0)
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
# P vipor         0.4.7    2023-12-18 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────
# > 