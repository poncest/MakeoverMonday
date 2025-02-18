
## Challenge: #MakeoverMondnay 2025 week 08
## Data:      Per Second
## Author:    Steven Ponce
## Date:      2025-02-18

## Original Chart 
#  Information is Beautiful
#  Per Second – Vibrations / Cycles / Waves / Rate / Frequency
#  https://informationisbeautiful.net/2024/per-second-vibrations-cycles-waves-rate-frequency/


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
    highcharter,    # A Wrapper for the 'Highcharts' Library
    htmlwidgets,    # HTML Widgets for R
    webshot2        # Take Screenshots of Web Pages
)

### |- figure size ----
# gg_record(
#     dir    = here::here("temp_plots"),
#     device = "png",
#     width  =  10,
#     height =  8,
#     units  = "in",
#     dpi    = 320
# )

# Source utility functions
# source(here::here("R/utils/fonts.R"))
# source(here::here("R/utils/social_icons.R"))
# source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
per_second_raw <- read_csv(
  file = 'data/2025/per_second.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(per_second_raw)
skim(per_second_raw )


## 4. TIDYDATA ----

### |-  tidy data ----
per_second_clean <- per_second_raw  |>
  select(unit:source) |> 
  filter(description != '"Frame rate" of conscious perception in human brain') |> 
  mutate(
    category = case_when(
      str_detect(tolower(description), "brain|heart|purr") ~ "Biological",
      str_detect(tolower(description), "sound|audio|speaker|speech") ~ "Sound",
      str_detect(tolower(description), "radio|wifi|bluetooth|power") ~ "Technology",
      str_detect(tolower(description), "light|ray|radiation") ~ "Radiation",
      TRUE ~ "Other"
    ),
    # Add formatted frequency for tooltip
    freq_formatted = case_when(
      vibrations_cycles_waves_rate_per_second_not_formatted >= 1e15 ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted/1e15, 1), " PHz"),
      vibrations_cycles_waves_rate_per_second_not_formatted >= 1e12 ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted/1e12, 1), " THz"),
      vibrations_cycles_waves_rate_per_second_not_formatted >= 1e9 ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted/1e9, 1), " GHz"),
      vibrations_cycles_waves_rate_per_second_not_formatted >= 1e6 ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted/1e6, 1), " MHz"),
      vibrations_cycles_waves_rate_per_second_not_formatted >= 1e3 ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted/1e3, 1), " kHz"),
      TRUE ~ paste0(round(vibrations_cycles_waves_rate_per_second_not_formatted, 1), " Hz")
    )
  )
  

# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors_list <- list(
  "Biological" = "#36A9E1",  
   "Sound"      = "#333333",      
   "Technology" = "#FF7F00",  
   "Radiation"  = "#7B68EE",   
   "Other"      = "#2ECC71")

### |-  titles and caption ----
title_text <- str_glue("The Spectrum of Frequencies in Nature and Technology")
subtitle_text <- str_glue("From heartbeats to radiation, visualizing the vast range of frequencies")
caption_text <-  str_glue("#MakeoverMonday 2025 Week 8 &#8226; Source: Information is Beautiful")

# Create caption
# caption_text <- create_social_caption(
#     mm_year = 2025,
#     mm_week = 08,
#     source_text = "Information is Beautiful"
# )

### |-  fonts ----
# setup_fonts()
# fonts <- get_font_families()

### |-  plot theme ----

# Create custom theme
custom_theme <- hc_theme(
  chart = list(
    backgroundColor = "#f5f5f2",
    style = list(
      fontFamily = "Arial, Helvetica, sans-serif"
    )
  ),
  title = list(
    style = list(
      fontSize = "20px",
      fontWeight = "bold",
      fontFamily = "Arial, Helvetica, sans-serif"
    )
  ),
  subtitle = list(
    style = list(
      fontSize = "14px",
      fontFamily = "Arial, Helvetica, sans-serif",
      color = "#666666"
    )
  ),
  caption = list(
    style = list(
      fontSize = "10px",
      fontFamily = "Arial, Helvetica, sans-serif",
      color = "#666666"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Arial, Helvetica, sans-serif",
      fontSize = "12px"
    )
  ),
  tooltip = list(
    style = list(
      fontFamily = "Arial, Helvetica, sans-serif",
      fontSize = "12px"
    )
  ),
  xAxis = list(
    labels = list(
      style = list(
        fontFamily = "Arial, Helvetica, sans-serif"
      )
    )
  ),
  yAxis = list(
    labels = list(
      style = list(
        fontFamily = "Arial, Helvetica, sans-serif"
      )
    )
  )   
)

### |-  Plot  ----

# Create the highchart visualization
hc_viz <- highchart() |>
  hc_add_theme(custom_theme) |>
  hc_chart(type = "line") |>
  hc_title(
    text = title_text,
    align = "left"
  ) |>
  hc_subtitle(
    text = subtitle_text,
    align = "left"
  ) |>
  hc_caption(
    text = caption_text,  
    align = "right"
  ) |> 
  hc_xAxis(
    title = list(text = "Phenomena Index"),
    gridLineWidth = 1,
    gridLineColor = "#E8E8E8",
    tickInterval = 5
  ) |>
  hc_yAxis(
    type = "logarithmic",
    title = list(text = "Frequency (Hz)"),
    gridLineWidth = 1,
    gridLineColor = "#E8E8E8",
    minorGridLineWidth = 0,
    labels = list(
      formatter = JS("function() {
        var value = this.value;
        if (value >= 1e15) return (value/1e15).toFixed(1) + ' PHz';
        if (value >= 1e12) return (value/1e12).toFixed(1) + ' THz';
        if (value >= 1e9) return (value/1e9).toFixed(1) + ' GHz';
        if (value >= 1e6) return (value/1e6).toFixed(1) + ' MHz';
        if (value >= 1e3) return (value/1e3).toFixed(1) + ' kHz';
        return value.toFixed(1) + ' Hz';
      }")
    )
  ) |>
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal",
    backgroundColor = "transparent",
    borderWidth = 0
  ) |>
  hc_tooltip(
    shared = FALSE,
    headerFormat = "",
    pointFormat = paste0(
      "<b>Category: {point.category}</b><br/>",
      "<b>{point.name}</b><br/>",
      "Frequency: {point.freq}<br/>",
      "Phenomena Index: {point.x}"
    ),
    backgroundColor = "white",
    borderWidth = 1,
    shadow = TRUE
  )

# Add line charts (series) by category
for(cat in unique(per_second_clean$category)) {
  data_subset <- per_second_clean |>
    filter(category == cat)
  
  hc_viz <- hc_viz |>
    hc_add_series(
      name = cat,
      color = colors_list[[cat]],
      data = list_parse(
        data_subset |>
          mutate(
            x = seq_along(description),
            y = vibrations_cycles_waves_rate_per_second_not_formatted,
            name = description,
            freq = freq_formatted,
            category = category
          ) |>
          select(x, y, name, freq, category)
      ),
      marker = list(
        enabled = TRUE,
        symbol = "circle",
        radius = 4
      ),
      lineWidth = 1.5,
      states = list(
        hover = list(
          lineWidth = 2,
          lineWidthPlus = 0
        )
      )
    )
}

# Add additional chart options
hc_viz <- hc_viz |>
  hc_plotOptions(
    series = list(
      animation = list(duration = 1000),
      marker = list(
        states = list(
          hover = list(
            enabled = TRUE,
            radius = 6
          )
        )
      )
    )
  ) |>
  hc_credits(
    enabled = TRUE,
    text = "&#x1F465; stevenponce &#8226; &#x1F4BB; poncest",  
    style = list(
      fontSize = "10px",
      color = "#666666"
    )
  )

# Display the visualization
hc_viz


# 6. SAVE ----
width_px <- 10 * 96  # 960 pixels
height_px <- 8 * 96  # 768 pixels

# Define the paths
output_dir <- "2025/Week_08"
html_file <- file.path(output_dir, "2025_08.html")
png_file <- file.path(output_dir, "2025_08.png")

# Create directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save as HTML first
saveWidget(hc_viz, html_file)

# Convert to PNG with specific dimensions
webshot(
  url = html_file,
  file = png_file,
  delay = 2,
  vwidth = width_px,
  vheight = height_px
)


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-02-17
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   3.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# P assertthat    0.2.1    2019-03-21 [?] RSPM (R 4.4.0)
# backports     1.5.0    2024-05-23 [1] RSPM (R 4.4.0)
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# broom         1.0.7    2024-09-26 [1] RSPM (R 4.4.0)
# camcorder   * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# P chromote      0.4.0    2025-01-25 [?] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
# curl          6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
# data.table    1.16.4   2024-12-06 [1] RSPM (R 4.4.0)
# P datasets    * 4.4.0    2024-04-24 [?] local
# digest        0.6.37   2024-08-19 [1] RSPM (R 4.4.0)
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# evaluate      1.0.3    2025-01-10 [1] RSPM (R 4.4.0)
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
# P highcharter * 0.9.4    2022-01-03 [?] RSPM (R 4.4.0)
# hms           1.1.3    2023-03-21 [1] RSPM (R 4.4.0)
# htmltools     0.5.8.1  2024-04-04 [1] RSPM (R 4.4.0)
# P htmlwidgets * 1.6.4    2023-12-06 [?] CRAN (R 4.4.0)
# P igraph        2.1.4    2025-01-23 [?] RSPM (R 4.4.0)
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# P later         1.4.1    2024-11-27 [?] RSPM (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# processx      3.8.5    2025-01-08 [1] RSPM (R 4.4.0)
# P promises      1.3.2    2024-11-28 [?] RSPM (R 4.4.0)
# ps            1.8.1    2024-10-28 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# P quantmod      0.4.26   2024-02-14 [?] CRAN (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# renv          1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
# repr          1.1.7    2024-03-22 [1] RSPM (R 4.4.0)
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# P rlist         0.4.6.2  2021-09-03 [?] RSPM (R 4.4.0)
# rmarkdown     2.29     2024-11-04 [1] RSPM (R 4.4.0)
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
# tibble      * 3.2.1    2023-03-20 [1] RSPM (R 4.4.0)
# tidyr       * 1.3.1    2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect    1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# timechange    0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# P TTR           0.24.4   2023-11-28 [?] CRAN (R 4.4.0)
# tzdb          0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# utf8          1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# P webshot2    * 0.1.1    2023-08-11 [?] RSPM (R 4.4.0)
# P websocket     1.4.2    2024-07-22 [?] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# P xts           0.14.1   2024-10-15 [?] RSPM (R 4.4.0)
# yaml          2.3.10   2024-07-26 [1] RSPM (R 4.4.0)
# P zoo           1.8-12   2023-04-13 [?] CRAN (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────────────────────────────────────
# > 
