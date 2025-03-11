
## Challenge: #MakeoverMondnay 2025 week 11
## Data:      Electric Car Sales
## Author:    Steven Ponce
## Date:      2025-03-11

## Original Chart
# Electric Car Sales
# https://data.world/makeovermonday/2025-week-11-electric-car-sales

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
    ggpubr,         # 'ggplot2' Based Publication Ready Plots
    patchwork,      # The Composer of Plots
    camcorder,      # Record Your Plot History 
    ggrepel,        # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
    directlabels,   # Direct Labels for Multicolor Plots
    gghighlight     # Highlight Lines and Points in 'ggplot2'
)

### |- figure size ----
gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  12,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----

#' The raw data for the week MakeoverMonday challenge can be downloaded 
#' https://data.world/makeovermonday/2025-week-11-electric-car-sales
#' 
#' Article
#' https://www.iea.org/reports/global-ev-outlook-2024/trends-in-electric-cars

electric_car_raw <- read_csv('data/2025/IEA-EV-dataEV salesHistoricalCars.csv') |> 
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(electric_car_raw)
skim(electric_car_raw)


## 4. TIDYDATA ----

# Battery Electric Vehicles (BEVs)
# Plug-in Hybrid Electric Vehicles (PHEVs)
# Hybrid Electric Vehicles (HEVs)

### |-  tidy data ----
ev_stock <- electric_car_raw |>
  filter(parameter == "EV stock", 
         mode == "Cars",
         category == "Historical") |>
  # Create a simplified region 
  mutate(
    region_group = case_when(
      region == "China" ~ "China",
      region %in% c("EU27", "UK", "EFTA", "Europe") ~ "Europe", 
      region == "USA" ~ "USA",
      TRUE ~ "ROW"   # Rest of the world
  ))

# P1. Area Chart Data ----

# A. market data
ev_market_data <- ev_stock |>
  filter(year >= 2013, year <= 2023) |>
  filter(powertrain %in% c("BEV", "PHEV")) |>       # Include only BEV and PHEV
  summarize(
    total = sum(value, na.rm = TRUE), 
    .by = c(year, region_group, powertrain)
    ) |>
  # Calculate important metrics for annotations
  mutate(
    annual_powertrain_total = sum(total),
    share_of_powertrain = total / annual_powertrain_total * 100,
    .by = c(year, powertrain)
  ) |>
  # Calculate year-over-year growth rates
  arrange(year) |>
  mutate(
    yoy_growth = (total / lag(total) - 1) * 100,
    growth_category = case_when(
      is.na(yoy_growth) ~ "First year",
      yoy_growth > 100 ~ "High growth (>100%)",
      yoy_growth > 50 ~ "Strong growth (50-100%)",
      yoy_growth > 25 ~ "Moderate growth (25-50%)",
      yoy_growth > 0 ~ "Low growth (0-25%)",
      TRUE ~ "Decline"
    ),
    .by = c(region_group, powertrain)
  ) 

# B. Annotation data for key insights
annotations <- tibble(
  year = c(2018, 2021, 2019.5, 2022),
  powertrain = c("BEV", "BEV", "PHEV", "PHEV"),
  y_pos = c(15000000, 30000000, 8500000, 23000000),
  label = c(
    "China takes BEV leadership\naccelerating electrification",
    "Post-COVID surge:\n2021-2023 added 40M EVs\n(3x the entire 2013-2020 period)",
    "Europe grows PHEV share\ndriven by policy incentives",
    "China begins rapid PHEV\nexpansion from 2021"
  ),
  hjust = c(0.5, 0.5, 0.5, 0.5),
  vjust = c(1, 0, 1, 0)
)

# C. Growth highlights data
growth_highlights <- ev_market_data |>
  filter(!is.na(yoy_growth), yoy_growth > 100, year >= 2017) |>
  group_by(region_group, powertrain) |>
  filter(yoy_growth == max(yoy_growth)) |>
  ungroup() |>
  arrange(desc(yoy_growth)) |>
  mutate(
    label = paste0("+", round(yoy_growth), "%"),
    year = year - 0.2,
    total = total + 4000000 # move up the label
  )

# D. Data for direct labeling of regions at the end (2023)
direct_labels <- ev_market_data |>
  filter(year == 2023) |>
  # Calculate cumulative positions for stacked areas
  arrange(desc(region_group)) |>  # Reverse order to match stacking
  mutate(
    y_pos = cumsum(total) - 0.5 * total,  # Center of each segment
    label = region_group,
    .by = powertrain
  ) 

# E. Milestone data
milestones <- ev_market_data |>
  group_by(powertrain) |>
  summarize(
    total_2023 = sum(total[year == 2023]),
    total_2013 = sum(total[year == 2013]),
    growth_factor = total_2023 / total_2013,
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(powertrain, " growth:\n", round(growth_factor), "x in 10 years")
  )


# P2. Growth Heatmap ----

# A. Reshape growth data for heatmap
growth_heatmap_data <- ev_stock |>
  filter(year >= 2014, year <= 2023) |>    # Start from 2014 to calculate YoY growth
  filter(powertrain %in% c("BEV", "PHEV")) |>
  summarize(
    total = sum(value, na.rm = TRUE), 
    .by = c(region_group, powertrain, year)
    ) |>
  arrange(year) |>
  mutate(
    yoy_growth = (total / lag(total) - 1) * 100,
    growth_category = case_when(
      is.na(yoy_growth) ~ NA_character_,
      yoy_growth > 100 ~ "High growth (>100%)",
      yoy_growth > 50 ~ "Strong growth (50-100%)",
      yoy_growth > 25 ~ "Moderate growth (25-50%)",
      yoy_growth > 0 ~ "Low growth (0-25%)",
      TRUE ~ "Decline"
    ),
    .by = c(region_group, powertrain)
  ) |>
  filter(!is.na(yoy_growth)) 

# change "ROW" to "Rest of the world" 
growth_heatmap_data <- growth_heatmap_data |>
  mutate(region_label = case_when(
    region_group == "ROW" ~ "Rest of world",
    TRUE ~ as.character(region_group)
  ),
  # Create an ordered factor for growth categories
  growth_category_ordered = factor(
    growth_category,
    levels = c(
      "High growth (>100%)", 
      "Strong growth (50-100%)", 
      "Moderate growth (25-50%)", 
      "Low growth (0-25%)",
      "Decline"  
    )
  )
  )



# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "China" = "#E41A1C",
  "Europe" = "#377EB8", 
  "USA" = "#4DAF4A",
  "ROW" = "#984EA3",
  "High growth (>100%)" = "#084081",    
  "Strong growth (50-100%)" = "#4292C6", 
  "Moderate growth (25-50%)" = "#9ECAE1",
  "Low growth (0-25%)" = "#DEEBF7"     
))
  
### |-  titles and caption ----
title_text <- str_glue("The Transformation of the Global Electric Vehicle Market (2013-2023)")
subtitle_text <- str_glue("Growth patterns reveal China's dominance and the accelerating global adoption of electric vehicles")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 11,
    source_text = "International Energy Agency (IEA)"
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
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5), 
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
# P1. Area Chart ----
area_chart <- ggplot(
  ev_market_data,
  aes(x = year, y = total, fill = region_group)
  ) +
  # Geom
  geom_area(
    alpha = 0.9, position = "stack"
  ) +
  geom_line(
    aes(color = region_group, group = region_group), 
    position = "stack", 
    color = "white", 
    linewidth = 0.3
  ) +
  geom_textbox(                 # Annotations
    data = annotations,
    aes(x = year, y = y_pos, label = label),
    box.color = "white",
    fill = "white",
    alpha = 0.7,
    width = unit(0.15, "npc"),
    hjust = 0.5,
    vjust = 0.5,
    size = 3,
    box.padding = unit(c(5, 5, 5, 5), "pt"),
    color = "black"
  ) +
  geom_label(                   # growth label
    data = growth_highlights,
    aes(label = label, color = region_group),
    fill = "white",
    alpha = 0.9,
    fontface = "bold",
    label.size = 0.1,
    nudge_x = -0.2,
  ) +
  geom_text(                       # direct label (right)
    data = direct_labels,
    aes(x = 2023.2, y = y_pos, label = label, color = region_group),
    hjust = 0,
    fontface = "bold",
    size = 3.5
  ) +
  geom_segment(                    # connecting lines to direct labels
    data = direct_labels,
    aes(
      x = 2023, 
      xend = 2023.15, 
      y = y_pos, 
      yend = y_pos,
      color = region_group
    ),
    linewidth = 0.5
  ) +
  # Scales
  scale_y_continuous(
    labels = label_number(scale = 1/1000000, suffix = "M"),
    expand = expansion(mult = c(0.05, 0.2))  
  ) +
  scale_x_continuous(
    breaks = seq(2013, 2023, by = 2),
    expand = expansion(mult = c(0.01, 0.06)),  
    sec.axis = sec_axis(
      ~ ., 
      breaks = c(2013, 2023),
      labels = c("2013", "2023")
    )
  ) +
  scale_fill_manual(values = colors$palette) +
  scale_color_manual(values = colors$palette) +
  # Labs
  labs(
    subtitle = paste(
      "BEVs have dominated global adoption with <span style='color:", colors$palette["China"], "'> China </span> leading growth,",
      "while PHEVs show different regional adoption patterns"
    ),
    x = NULL,
    y = "Vehicle Stock (millions)"
  ) +
  # Facets
  facet_wrap(~ powertrain, ncol = 1, scales = "free_y") +
  # Theme
  theme(
    legend.position = "none",
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),  
    strip.text = element_text(face = "bold", size = rel(0.86), margin = margin(b = 5)),
    panel.spacing.y = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.subtitle = element_markdown(
      size = rel(0.78),
      family = fonts$subtitle,
      color = colors$subtitle,
      margin = margin(b = 10)
    )
  )
  
# P2. Growth Heatmap ---- 
growth_heatmap <- ggplot(
  growth_heatmap_data,
  aes(x = year, y = fct_rev(region_label), fill = growth_category_ordered)
  ) +
  # Geoms
  geom_tile(
    color = "white", linewidth = 0.7
    ) +
  geom_text(
    aes(label = paste0(round(yoy_growth), "%")),
    color = ifelse(growth_heatmap_data$growth_category %in% 
                     c("High growth (>100%)", "Strong growth (50-100%)"), "white", "black"),
    size = 3,
    fontface = "bold"
  ) +
  # Scales
  scale_fill_manual(
    values = colors$palette,
    name = "Year-over-Year Growth",
    guide = guide_legend(
      title.position = "top",
      nrow = 1,
      label.theme = element_text(size = 9),
      reverse = FALSE
    ),
    drop = FALSE      # Drop any unused levels
  ) +
  scale_x_continuous(
    breaks = 2015:2023, 
    expand = expansion(mult = c(0, 0))
  ) +
  # Legend
  guides(
    fill = guide_legend(
      title = "Year-over-Year Growth",
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        size = 3
      ),
      title.theme = element_text(face = "bold", size = 10),
      label.theme = element_text(size = 9)
    )
  ) +
  # Labels 
  labs(
    subtitle = "Year-over-year growth percentages reveal intensity of adoption across regions",
    x = NULL,
    y = NULL
  ) +
  # Facet 
  facet_wrap(~ powertrain, ncol = 1) +
  # Theme
  theme(
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
    panel.grid = element_blank(),
    panel.spacing.y = unit(1.5, "cm"),
    strip.text = element_text(face = "bold", size = rel(0.86), margin = margin(b = 5)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.size = unit(0.8, "lines"),
    
    plot.subtitle = element_text(
      size = rel(0.78),
      family = fonts$subtitle,
      color = colors$subtitle,
      margin = margin(b = 10)
  ))

# Combined Plots ----
combined_plot <- (area_chart/ growth_heatmap ) +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size   = rel(1.6),
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
        margin = margin(t = 5, b = 5)
      ),
      plot.caption = element_markdown(
        size   = rel(0.65),
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

# ─ Session info ────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-03-11
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────
# ! package      * version   date (UTC) lib source
# P abind          1.4-8     2024-09-12 [?] RSPM (R 4.4.0)
# backports      1.5.0     2024-05-23 [1] RSPM (R 4.4.0)
# V base         * 4.4.1     2024-04-24 [2] local (on disk 4.4.0)
# base64enc      0.1-3     2015-07-28 [1] RSPM (R 4.4.0)
# bit            4.5.0.1   2024-12-03 [1] RSPM (R 4.4.0)
# bit64          4.6.0-1   2025-01-16 [1] RSPM (R 4.4.0)
# broom          1.0.7     2024-09-26 [1] RSPM (R 4.4.0)
# camcorder    * 0.1.0     2022-10-03 [1] RSPM (R 4.4.0)
# P car            3.1-3     2024-09-27 [?] RSPM (R 4.4.0)
# P carData        3.0-5     2022-01-06 [?] RSPM (R 4.4.0)
# cli            3.6.3     2024-06-21 [1] RSPM (R 4.4.0)
# colorspace     2.1-1     2024-07-26 [1] RSPM (R 4.4.0)
# commonmark     1.9.2     2024-10-04 [1] RSPM (R 4.4.0)
# P compiler       4.4.0     2024-04-24 [?] local
# crayon         1.5.3     2024-06-20 [1] RSPM (R 4.4.0)
# curl           6.2.0     2025-01-23 [1] RSPM (R 4.4.0)
# P datasets     * 4.4.0     2024-04-24 [?] local
# digest         0.6.37    2024-08-19 [1] RSPM (R 4.4.0)
# P directlabels * 2024.1.21 2024-01-24 [?] RSPM (R 4.4.0)
# dplyr        * 1.1.4     2023-11-17 [1] RSPM (R 4.4.0)
# evaluate       1.0.3     2025-01-10 [1] RSPM (R 4.4.0)
# farver         2.1.2     2024-05-13 [1] RSPM (R 4.4.0)
# fastmap        1.2.0     2024-05-15 [1] RSPM (R 4.4.0)
# forcats      * 1.0.0     2023-01-29 [1] RSPM (R 4.4.0)
# P Formula        1.2-5     2023-02-24 [?] RSPM (R 4.4.0)
# generics       0.1.3     2022-07-05 [1] RSPM (R 4.4.0)
# gghighlight  * 0.4.1     2023-12-16 [1] RSPM (R 4.4.0)
# ggplot2      * 3.5.1     2024-04-23 [1] RSPM (R 4.4.0)
# P ggpubr       * 0.6.0     2023-02-10 [?] RSPM (R 4.4.0)
# P ggrepel      * 0.9.6     2024-09-07 [?] RSPM (R 4.4.0)
# P ggsignif       0.6.4     2022-10-13 [?] RSPM (R 4.4.0)
# ggtext       * 0.1.2     2022-09-16 [1] RSPM (R 4.4.0)
# gifski         1.32.0-1  2024-10-13 [1] RSPM (R 4.4.1)
# glue         * 1.8.0     2024-09-30 [1] RSPM (R 4.4.0)
# P graphics     * 4.4.0     2024-04-24 [?] local
# P grDevices    * 4.4.0     2024-04-24 [?] local
# P grid           4.4.0     2024-04-24 [?] local
# gridtext       0.1.5     2022-09-16 [1] RSPM (R 4.4.0)
# gtable         0.3.6     2024-10-25 [1] RSPM (R 4.4.0)
# here         * 1.0.1     2020-12-13 [1] RSPM (R 4.4.0)
# hms            1.1.3     2023-03-21 [1] RSPM (R 4.4.0)
# htmltools      0.5.8.1   2024-04-04 [1] RSPM (R 4.4.0)
# janitor      * 2.2.1     2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite       1.8.9     2024-09-20 [1] RSPM (R 4.4.0)
# knitr          1.49      2024-11-08 [1] RSPM (R 4.4.0)
# labeling       0.4.3     2023-08-29 [1] RSPM (R 4.4.0)
# lifecycle      1.0.4     2023-11-07 [1] RSPM (R 4.4.0)
# lubridate    * 1.9.4     2024-12-08 [1] RSPM (R 4.4.0)
# magick         2.8.5     2024-09-20 [1] RSPM (R 4.4.0)
# magrittr       2.0.3     2022-03-30 [1] RSPM (R 4.4.0)
# markdown       1.13      2024-06-04 [1] RSPM (R 4.4.0)
# P methods      * 4.4.0     2024-04-24 [?] local
# munsell        0.5.1     2024-04-01 [1] RSPM (R 4.4.0)
# P pacman       * 0.5.1     2019-03-11 [?] RSPM (R 4.4.0)
# P parallel       4.4.0     2024-04-24 [?] local
# patchwork    * 1.3.0     2024-09-16 [1] RSPM (R 4.4.0)
# pillar         1.10.1    2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig      2.0.3     2019-09-22 [1] RSPM (R 4.4.0)
# purrr        * 1.0.2     2023-08-10 [1] RSPM (R 4.4.0)
# P quadprog       1.5-8     2019-11-20 [?] RSPM (R 4.4.0)
# R6             2.5.1     2021-08-19 [1] RSPM (R 4.4.0)
# ragg           1.3.3     2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp           1.0.14    2025-01-12 [1] RSPM (R 4.4.0)
# readr        * 2.1.5     2024-01-10 [1] RSPM (R 4.4.0)
# renv           1.0.3     2023-09-19 [1] CRAN (R 4.4.0)
# repr           1.1.7     2024-03-22 [1] RSPM (R 4.4.0)
# rlang          1.1.5     2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot      2.0.4     2023-11-05 [1] RSPM (R 4.4.0)
# P rstatix        0.7.2     2023-02-01 [?] RSPM (R 4.4.0)
# rstudioapi     0.17.1    2024-10-22 [1] RSPM (R 4.4.0)
# rsvg           2.6.1     2024-09-20 [1] RSPM (R 4.4.0)
# scales       * 1.3.0     2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo    1.2.2     2021-12-06 [?] RSPM (R 4.4.0)
# showtext     * 0.9-7     2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb   * 3.0       2020-06-04 [1] RSPM (R 4.4.0)
# skimr        * 2.1.5     2022-12-23 [1] RSPM (R 4.4.0)
# snakecase      0.11.1    2023-08-27 [1] RSPM (R 4.4.0)
# P stats        * 4.4.0     2024-04-24 [?] local
# stringi        1.8.4     2024-05-06 [1] RSPM (R 4.4.0)
# stringr      * 1.5.1     2023-11-14 [1] RSPM (R 4.4.0)
# svglite        2.1.3     2023-12-08 [1] RSPM (R 4.4.0)
# sysfonts     * 0.8.9     2024-03-02 [1] RSPM (R 4.4.0)
# systemfonts    1.2.1     2025-01-20 [1] RSPM (R 4.4.0)
# textshaping    1.0.0     2025-01-20 [1] RSPM (R 4.4.0)
# tibble       * 3.2.1     2023-03-20 [1] RSPM (R 4.4.0)
# tidyr        * 1.3.1     2024-01-24 [1] RSPM (R 4.4.0)
# tidyselect     1.2.1     2024-03-11 [1] RSPM (R 4.4.0)
# tidyverse    * 2.0.0     2023-02-22 [1] RSPM (R 4.4.0)
# timechange     0.3.0     2024-01-18 [1] RSPM (R 4.4.0)
# P tools          4.4.0     2024-04-24 [?] local
# tzdb           0.4.0     2023-05-12 [1] RSPM (R 4.4.0)
# utf8           1.2.4     2023-10-22 [1] RSPM (R 4.4.0)
# P utils        * 4.4.0     2024-04-24 [?] local
# vctrs          0.6.5     2023-12-01 [1] RSPM (R 4.4.0)
# vroom          1.6.5     2023-12-05 [1] RSPM (R 4.4.0)
# withr          3.0.2     2024-10-28 [1] RSPM (R 4.4.0)
# xfun           0.50      2025-01-07 [1] RSPM (R 4.4.0)
# xml2           1.3.6     2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────
# > 
