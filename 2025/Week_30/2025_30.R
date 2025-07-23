
## Challenge: #MakeoverMondnay 2025 week 30
## Data:      London Underground Average Monthly Temperatures
## Author:    Steven Ponce
## Date:      2025-07-23

## Article
# https://www.dailymail.co.uk/sciencetech/article-13739705/london-underground-hottest-line.html

## Data
# https://data.world/makeovermonday/2025w30-london-underground-average-monthly-temperatures/settings?tab=access

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  scales,         # Scale Functions for Visualization
  glue,           # Interpreted String Literals
  patchwork,      # The Composer of Plots
  ggridges        # Ridgeline Plots in 'ggplot2'
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  =  12,
  height =  8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
avg_temp_raw <- read_csv("data/2025/lu-average-monthly-temperatures 2013-2024.csv") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(avg_temp_raw)
skimr::skim(avg_temp_raw)


## 4. TIDYDATA ----
temp_long <- avg_temp_raw |>
  pivot_longer(cols = bakerloo:sub_surface_lines, 
               names_to = "line", 
               values_to = "temperature") |>  
  mutate(
    date = ymd(paste(year, month, "01")),
    month_num = match(month, month.name),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5) ~ "Spring", 
      month_num %in% c(6, 7, 8) ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Autumn"
    ),
    line_clean = case_when(
      line == "waterloo_and_city" ~ "Waterloo & City",
      line == "sub_surface_lines" ~ "Sub-Surface Lines", 
      TRUE ~ str_to_title(line)
    )
  )

line_order <- temp_long |>
  group_by(line_clean) |>
  summarise(median_temp = median(temperature, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(median_temp)) |>
  pull(line_clean)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(NULL))

### |-  titles and caption ----
title_text <- str_glue("London Underground Temperature Analysis (2013-2023)")
subtitle_text <- str_glue("Central and Bakerloo lines run consistently hottest • Sub-Surface lines stay coolest year-round")

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 30,
  source_text = "Transport for London (TfL)"
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
    plot.title = element_text(
      size = rel(1.2), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0.5,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.9), hjust = 0.5, family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9), lineheight = 0.9,
      margin = margin(t = 5, b = 20)
    ),

    # Legend formatting
    legend.position = "right",
    # legend.direction = "horizontal",
    legend.box.margin = margin(b = 10),
    legend.margin = margin(b = 5),
    legend.title = element_text(face = "bold"),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title.x = element_text(face = "bold", size = rel(0.85)),
    axis.title.y = element_text(face = "bold", size = rel(0.85)),
    axis.text = element_text(size = rel(0.85), family = fonts$subtitle, color = colors$text),

    # Grid lines
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

# Scale function
create_warm_color_scale <- function(legend_position = "right") {
  scale_fill_gradient2(
    name = "Temperature\n(°C)",
    low = "#2c1810",      
    mid = "#cc4125",       
    high = "#f9c74f",     
    midpoint = 22,
    breaks = seq(10, 30, 5),
    labels = paste0(seq(10, 30, 5), "°C"),
    guide = if(legend_position == "none") "none" else guide_colorbar()
  )
}

### |-  Plot 1  heatmap ----
p1 <- temp_long |>
  mutate(line_clean = factor(line_clean, levels = line_order)) |>
  ggplot(aes(x = month_num, y = line_clean, fill = temperature)) +
  # Geoms
  geom_tile(color = "white", linewidth = 0.8) +
  # Scales
  create_warm_color_scale() +
  scale_x_continuous(
    breaks = 1:12, 
    labels = month.abb,
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  # Labs
  labs(
    title = "Monthly Temperature Patterns",
    subtitle = "Heatmap reveals seasonal trends across London Underground lines",
    x = "Month",
    y = NULL
  ) +
  # Theme
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(0.4, "cm"),
    axis.text.y = element_text(size = 10, hjust = 1)
  )

### |-  Plot 2  ridges plot ----
# Calculate average temperature for each line 
line_avg_temps <- temp_long |>
  group_by(line_clean) |>
  summarise(avg_temp = mean(temperature, na.rm = TRUE), .groups = "drop")

p2 <- temp_long |>
  mutate(line_clean = factor(line_clean, levels = line_order)) |>
  left_join(line_avg_temps, by = "line_clean") |>
  ggplot(aes(x = temperature, y = line_clean, fill = avg_temp)) +
  # Geoms
  geom_density_ridges(
    alpha = 0.8, 
    scale = 0.95,
    linewidth = 0.3,
    color = "white"
  ) +
  # Scales
  create_warm_color_scale(legend_position = "none") +
  scale_x_continuous(
    breaks = seq(10, 35, 5),
    labels = paste0(seq(10, 35, 5), "°C"),
    expand = c(0.01, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  # Labs
  labs(
    title = "Temperature Distribution by Line",
    subtitle = "Density curves colored by each line's average temperature",
    x = "Temperature (°C)",
    y = NULL
  ) +
  # Theme
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

### |-  combined plot ----
# Create an invisible spacer plot
spacer <- ggplot() +
  theme_void()

combined_plots <- (p1 + spacer + p2) +
  plot_layout(
    widths = c(1.2, 0.01, 1)
    )

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(2),
        family = fonts$title,
        face = "bold",
        hjust = 0.5,
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1.05),
        hjust = 0.5,
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 0.9,
        margin = margin(t = 5, b = 0)
      ),
      plot.caption = element_markdown(
        size = rel(0.65),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2025-07-22
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
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
# janitor       2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
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
# skimr         2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
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
# ───────────────────────────────────────────────────────
# > 
