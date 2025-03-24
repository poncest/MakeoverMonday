
## Challenge: #MakeoverMondnay 2025 week 13
## Data:      Work Productivity
## Author:    Steven Ponce
## Date:      2025-03-24

## Original Chart
# Work Productivity
# https://www.activtrak.com/resources/benchmarks/productivity-benchmarks-cross-industry/

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
    camcorder       # Record Your Plot History 
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
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
#' https://data.world/makeovermonday/2025-week-13-work-productivity
#' 
#' Article
#' https://www.activtrak.com/news/press-release-productivity-benchmarks-report-1h2024/

productivity_raw <- read_csv('data/2025/Work productivy Activtrak Report.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(productivity_raw)
skim(productivity_raw)


## 4. TIDYDATA ----

### |-  tidy data ----

productivity_data <- productivity_raw |>         
  # Convert total time hours string to numeric for basic metrics
  mutate(
    total_time_numeric = case_when(
      industry == "Cross-Industry" ~ 8 + 54/60,
      industry == "Fin Serv" ~ 9,
      industry == "Healthcare" ~ 8 + 42/60,
      industry == "Insurance" ~ 8 + 41/60,
      industry == "Prof. Servs." ~ 8 + 52/60
    ),
    # basic metrics
    non_productive_time = total_time_numeric - productive_time_hrs,
    work_activities_time = focus_time_hrs + collaboration_time_hrs,
    focus_collaboration_ratio = focus_time_hrs / collaboration_time_hrs,
    productive_time_percent = (productive_time_hrs / total_time_numeric) * 100,
    
    # derived metrics
    ratio_deviation = focus_collaboration_ratio - 1.5,
    industry_label = paste0(industry, " (", format(round(focus_collaboration_ratio, 2), nsmall=2), ":1)"),
    dominance = case_when(
      abs(ratio_deviation) < 0.001 ~ "Balanced",
      ratio_deviation > 0 ~ "More Focused",
      TRUE ~ "More Collaborative"
    ),
    hours_diff = focus_time_hrs - collaboration_time_hrs,
    hours_diff_label = paste0("Difference: ", format(round(abs(hours_diff), 1), nsmall=1), " hrs"),
    annotation = paste0(
      focus_time_hrs, " hrs focus / ", 
      collaboration_time_hrs, " hrs collaboration"
    ),
    industry_order = rank(-focus_collaboration_ratio)
  )

# Annotation for Insurance
insurance_annotation <- tibble(
  x = 0.15,
  y = which(productivity_data$industry == "Insurance"),
  label = "No bar shown because ratio is exactly 1.5:1\n(6.9 hrs focus ÷ 4.6 hrs collaboration),\nplacing it precisely at the baseline"
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "More Collaborative" = "#fc8d62",  
  "Balanced" = "#8da0cb",            
  "More Focused" = "#66c2a5"  
))
  
### |-  titles and caption ----
title_text <- str_wrap("Focus to Collaboration Ratio Across Industries", width = 80)
subtitle_text <- str_glue("How industries balance individual work vs. teamwork (centered at 1.5:1 ratio)<br><br>",
                          "A ratio > 1.5:1 indicates more focus-oriented work culture<br>",
                          "A ratio < 1.5:1 indicates more collaboration-oriented work culture")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 13,
    source_text = "Activtrak"
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

    # Legend formatting
    legend.position = "top",
    legend.title = element_text(face = "bold"),

    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5), 
    axis.ticks.length = unit(0.2, "cm"), 

    # Axis formatting
    axis.title.x = element_text(face = "bold", size = rel(1.14)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = rel(1)),
    
    # Grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 30, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot  ----
ggplot(
  productivity_data, 
  aes(x = reorder(industry_label, -industry_order), 
      y = ratio_deviation)
) +
  # Geoms      
  geom_rect(
    aes(
      xmin = as.numeric(factor(reorder(industry_label, -industry_order))) - 0.5,
      xmax = as.numeric(factor(reorder(industry_label, -industry_order))) + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray95",
    color = NA,
    alpha = 0.3
  ) +
  geom_rect(
    data = productivity_data |> filter(dominance == "Balanced"),
    aes(
      xmin = as.numeric(factor(reorder(industry_label, -industry_order))) - 0.5,
      xmax = as.numeric(factor(reorder(industry_label, -industry_order))) + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray90",
    color = NA,
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0, 
    color = alpha("black", 0.5),
    linewidth = 0.2,
    linetype = "solid"
  ) +
  geom_col(
    aes(fill = dominance), 
    width = 0.7
    ) +
  geom_point(
    data = productivity_data |> filter(dominance == "Balanced"),
    aes(x = reorder(industry_label, -industry_order), y = 0),
    color = colors$palette["Balanced"],
    size = 4,
    shape = 18  # Diamond shape
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f:1", focus_collaboration_ratio),
      y = ifelse(ratio_deviation >= 0, 
                 pmax(ratio_deviation/2, 0.02), 
                 pmin(ratio_deviation/2, -0.02))
    ),
    color = "black",  
    fontface = "bold",
    size = 4
  ) +
  geom_text(
    aes(
      label = hours_diff_label,
      y = 0
    ),
    hjust = -0.1, 
    vjust = 2.7,   
    size = 3,
    color = "black",
    alpha = 0.7    
  ) +
  geom_text(
    data = insurance_annotation,
    aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 0.5,
    size = 2.8,
    fontface = "italic",
    color = "black",
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = 0, 
    color = "black", 
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 0,
    y = 5.7,
    label = "Balanced (1.5:1)",
    hjust = 0.5,
    vjust = 1,
    size = 3,
    fontface = "italic",
    color = "black",
    alpha = 0.7
  ) +
  # Scales
  scale_x_discrete(position = "top") + 
  scale_y_continuous(
    limits = c(-0.1, 0.3),
    breaks = seq(-0.1, 0.3, by = 0.05),
    labels = function(x) sprintf("%+.2f", x)
  ) +
  scale_fill_manual(
    values = colors$palette,
    name = "Work Style"
  ) +
  coord_flip() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Deviation from balanced ratio (1.5:1)"
  ) +
  # Theme
  theme(
       plot.title = element_text(
         size   = rel(2.4),
         family = fonts$title,
         face   = "bold",
         color  = colors$title,
         lineheight = 1.1,
         margin = margin(t = 5, b = 5)
       ),
       plot.subtitle = element_markdown(
         size   = rel(1),
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
       )
     )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-24
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────
# > 