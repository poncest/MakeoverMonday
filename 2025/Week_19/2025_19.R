
## Challenge: #MakeoverMondnay 2025 week 19
## Data:      Asian Restaurant in the US
## Author:    Steven Ponce
## Date:      2025-05-05

## Original Chart
# Asian Restaurant in the US
# https://data.world/makeovermonday/2025-w19-asian-restaurants-in-the-us

##  Article
# Pew Research Center "71% of Asian restaurants in the U.S. serve Chinese, Japanese or Thai food"
# https://www.pewresearch.org/short-reads/2023/05/23/71-of-asian-restaurants-in-the-u-s-serve-chinese-japanese-or-thai-food/

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
    patchwork,      # The Composer of Plots
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
sheet1_raw <- read_excel('data/2025/MakeoverMonday 2025 W19_ Asian Restaurants in the US.xlsx',
                                   sheet = 1) |> 
  clean_names()

sheet2_raw <- read_excel('data/2025/MakeoverMonday 2025 W19_ Asian Restaurants in the US.xlsx',
                         sheet = 2) |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(sheet1_raw)
glimpse(sheet2_raw)


## 4. TIDYDATA ----

### |-  tidy data ----
sheet2_clean <- sheet2_raw |>
  mutate(percent_of_asian_restaurants = case_when(
    percent_of_asian_restaurants == "<1" ~ "0.5",
    TRUE ~ percent_of_asian_restaurants
  )) |>
  mutate(percent_of_asian_restaurants = as.numeric(percent_of_asian_restaurants))

# Add Asian population percentages based on Pew Research Center data
# Source: "71% of Asian restaurants in the U.S. serve Chinese, Japanese or Thai food"
# Published by Pew Research Center on May 23, 2023
population_data <- tibble(
  category = c("Chinese", "Japanese", "Thai", "Indian", "Vietnamese", "Korean", 
               "Filipino", "Pakistani", "Mongolian", "Burmese", "Other Asian/Unspecified"),
  percent_of_asian_population = c(24, 7, 2, 21, 11, 9, 20, 3, 0.1, 0.9, 2)
)

# Combine datasets
sheet2_combined <- sheet2_clean |>
  left_join(population_data, by = "category") |> 
  mutate(
    percent_of_all_restaurants = percent_of_asian_restaurants * sheet1_raw$percent_of_all_us_restaurants[1] / 100,
    representation_ratio = percent_of_asian_restaurants / percent_of_asian_population,
    representation_diff = percent_of_asian_restaurants - percent_of_asian_population,
    representation_status = ifelse(representation_diff > 0, "over represented", "under represented")
  )

# plot 1 data: composition 
composition_data <- sheet2_combined |>
  filter(category != "Other Asian/Unspecified") |>
  arrange(desc(percent_of_asian_restaurants)) |>
  mutate(label_text = paste0(percent_of_asian_restaurants, "%"))

# plot 2 data: diverging bars
diverging_data <- sheet2_combined |>
  filter(
    category != "Other Asian/Unspecified",
    percent_of_asian_restaurants >= 1
    ) |>
  # Create a temporary data frame with the needed calculations
  mutate(
    hjust_value = ifelse(representation_diff > 0, -0.2, 1.2)
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "over represented" =  "#4CAF50",   
  "under represented" = "#FF9800", 
  "neutral" = "#BDBDBD" 
))
  
### |-  titles and caption ----
title_text <- str_wrap("The Uneven Landscape of Asian Restaurants in the U.S.", width = 80)
subtitle_text <- str_wrap("Comparing restaurant presence with Asian American population distribution",
                          width = 85)

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 19,
    source_text = "Pew Research Center"
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
    axis.title.x = element_text(face = "bold", size = rel(0.85)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = rel(0.85)),
    
    # Grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 30, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot 1  ----
# Composition bar chart     
composition_plt <- composition_data |>
  ggplot(aes(x = reorder(category, percent_of_asian_restaurants), 
             y = percent_of_asian_restaurants,
             fill = representation_status)
         ) +
  # Geoms
  geom_col() +
  geom_text(aes(label = label_text), 
            hjust = -0.2, size = 3.5) +
  # Scales
  scale_fill_manual(
    values = colors$palette,
    name = "Representation"
  ) +
  scale_y_continuous(limits = c(0, 45), labels = function(x) paste0(x, "%")) +
  coord_flip() +
  # Labs
  labs(
    title = "Composition of Asian Restaurants in the U.S.",
    subtitle = "Percentage of Asian restaurants by cuisine type",
    x = NULL,
    y = "Percentage of Asian Restaurants"
  ) +
  # Theme
  # theme_minimal() +
  theme(
    legend.position = "plot",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray50")
  )

### |-  Plot 2  ----
# Diverging bar chart
diverging_plt <- diverging_data |>
  ggplot(aes(x = reorder(category, representation_diff), 
             y = representation_diff,
             fill = representation_status)
         ) +
  # Geoms
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.5, color = 'gray20') +
  geom_text(
    aes(label = paste0(ifelse(representation_diff > 0, "+", ""), 
                       round(representation_diff, 1), "%"),
        hjust = hjust_value),
    size = 3
  ) +
  # Scales
  scale_fill_manual(
    values = colors$palette,
    name = "Representation"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(-23, 25)
  ) +
  coord_flip() +
  # Labs
  labs(
    title = "Over and Under-Representation of Asian Cuisines",
    subtitle = "Difference between restaurant percentage and population percentage",
    x = NULL,
    y = "Representation Gap (%)"
  ) +
  # Theme
  # theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray50")
  )

### |-  Plot 3  ----
# title panel
title_panel <- ggplot() + 
  # Scales
  xlim(0, 1) + ylim(0, 1) +
  # Annotate
  annotate(
    "text", x = 0, y = 0.6, 
    label = title_text,
    hjust = 0, vjust = 0,
    color = colors$title, size = 9, 
    fontface = "bold", family = fonts$title
  ) +
  annotate(
    "text", x = 0, y = 0.2, 
    label = subtitle_text,
    hjust = 0, vjust = 0,
    size = 5, color = "gray50", family = fonts$subtitle
  ) +
  # Theme
  theme_void() +
  theme(
    plot.margin = margin(20, 10, 10, 10),
    plot.background  = element_rect(fill = colors$background, color = colors$background),
    panel.background = element_rect(fill = colors$background, color = colors$background),
  )

### |-  Plot 4  ----
# empty panel
empty_panel <- ggplot() + 
  # Theme
  theme_void() 

### |-  Plot 5  ----
# insights panel
insights_panel <- ggplot() + 
  # Scales
  xlim(0, 1) + ylim(0, 1) +  
  # Annotate
  annotate(
    "text", x = 0.05, y = 0.95, 
    label = "Key Insights:",
    hjust = 0, vjust = 1,
    size = 4.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 0.05, y = 0.82, 
    label = "• Only 12% of all U.S. restaurants serve Asian food",
    hjust = 0, vjust = 1,
    size = 3.2
  ) +
  annotate(
    "text", x = 0.05, y = 0.67, 
    label = "• Chinese, Japanese, and Thai cuisines dominate the Asian restaurant\n  scene (78%) despite representing only one-third of Asian Americans",
    hjust = 0, vjust = 1,
    size = 3.2
  ) +
  annotate(
    "text", x = 0.05, y = 0.47, 
    label = "• Japanese (+21%) and Thai (+9%) cuisines have the highest\n  over-representation compared to their population shares",
    hjust = 0, vjust = 1,
    size = 3.2
  ) +
  annotate(
    "text", x = 0.05, y = 0.32, 
    label = "• Filipino cuisine shows the largest representation gap: only 1% of Asian\n  restaurants despite Filipinos comprising 20% of Asian Americans",
    hjust = 0, vjust = 1,
    size = 3.2
  ) +
  annotate(
    "text", x = 0.05, y = 0.17, 
    label = "• Indian cuisine shows the second-largest representation gap (-14%),\n  with far fewer restaurants than their population would suggest",
    hjust = 0, vjust = 1,
    size = 3.2
  ) +
  # Theme
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10),  
    plot.background  = element_rect(fill = colors$background, color = colors$background),
    panel.background = element_rect(fill = colors$background, color = colors$background),
  )

### |-  Final Plot  ----
# combined plot
combined_plot <- title_panel +   # P1
  insights_panel +               # P2
  composition_plt +              # P3
  diverging_plt +                # P4
  empty_panel +                  # P5
  plot_layout(
    design = "
    AAAAA
    BCCCC
    BDDDD
    EEEEE
    ",
    widths = c(4.5, 1, 1, 1, 1),
    heights = c(0.8, 2, 2, 0.4)
  ) 

combined_plot +
  plot_annotation(
    # title = title_text,
    # subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
          panel.spacing = unit(15, "pt"),
          plot.title = element_text(
            size = rel(1.5),
            family = fonts$title,
            face = "bold",
            color = colors$title,
            lineheight = 1.1,
            margin = margin(t = 5, b = 5)
          ),
          plot.subtitle = element_text(
            size = rel(0.85),
            family = fonts$subtitle,
            color = colors$subtitle,
            lineheight = 1.2,
            margin = margin(t = 5, b = 15)
          ),
          plot.caption = element_markdown(
            size   = rel(0.65),
            family = fonts$caption,
            color  = colors$caption,
            hjust  = 0.5,
            margin = margin(t = 10)
          )
        )
    )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-05
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# camcorder   * 0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cellranger    1.1.0    2016-07-27 [1] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# commonmark    1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
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
# labeling      0.4.3    2023-08-29 [1] RSPM (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
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
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────
# > 
