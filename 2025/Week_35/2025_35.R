## Challenge: #MakeoverMondnay 2025 week 35
## Data:      Meat Production by Livestock Type
## Author:    Steven Ponce
## Date:      2025-08-25

## Article
# https://ourworldindata.org/meat-production

## Data
# https://data.world/makeovermonday/meat-production-by-livestock-type

## Original Chart
# https://ourworldindata.org/meat-production

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork,  # The Composer of Plots
  tidytext    # Text Mining using 'dplyr', 'ggplot2', and Other Tidy Tools 
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
meat_production_raw <- readxl::read_excel("data/2025/Global meat production by livestock type.xlsx") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(meat_production_raw)
skimr::skim_without_charts(meat_production_raw)


## 4. TIDYDATA ----
meat_clean <- meat_production_raw |>
  # Remove rows with missing entity or where all meat columns are NA
  filter(!is.na(entity), !entity %in% c("World", "")) |>
  # Pivot longer to get meat types in one column
  pivot_longer(
    cols = starts_with("meat_"),
    names_to = "meat_type_raw",
    values_to = "production_tonnes",
    values_drop_na = TRUE
  ) |>
  # Clean meat type names
  mutate(
    meat_type = case_when(
      str_detect(meat_type_raw, "beef") ~ "Beef & Buffalo",
      str_detect(meat_type_raw, "pig") ~ "Pig",
      str_detect(meat_type_raw, "poultry") ~ "Poultry",
      str_detect(meat_type_raw, "sheep") ~ "Sheep & Goat",
      str_detect(meat_type_raw, "game") ~ "Game",
      str_detect(meat_type_raw, "horse") ~ "Horse",
      str_detect(meat_type_raw, "camel") ~ "Camel",
      TRUE ~ "Other"
    )
  ) |>
  # Focus on major meat types for cleaner visualizations
  filter(meat_type %in% c("Beef & Buffalo", "Pig", "Poultry", "Sheep & Goat")) |>
  select(entity, code, year, meat_type, production_tonnes)

# Create regional groupings for continental analysis
regional_data <- meat_clean |>
  mutate(
    region = case_when(
      entity %in% c(
        "China", "India", "Japan", "South Korea", "Indonesia", "Thailand",
        "Philippines", "Vietnam", "Malaysia", "Bangladesh", "Pakistan"
      ) ~ "Asia",
      entity %in% c("United States", "Canada", "Mexico") ~ "North America",
      entity %in% c(
        "Brazil", "Argentina", "Chile", "Colombia", "Peru", "Uruguay",
        "Venezuela", "Ecuador", "Bolivia"
      ) ~ "South America",
      entity %in% c(
        "Germany", "France", "United Kingdom", "Italy", "Spain", "Poland",
        "Netherlands", "Russia", "Ukraine", "Turkey"
      ) ~ "Europe",
      entity %in% c(
        "Nigeria", "Ethiopia", "South Africa", "Kenya", "Ghana", "Morocco",
        "Algeria", "Egypt", "Tanzania", "Uganda"
      ) ~ "Africa",
      entity %in% c("Australia", "New Zealand") ~ "Oceania",
      TRUE ~ "Other"
    )
  ) |>
  filter(region != "Other")

# P1: Regional trends over time data ----
trends_data <- regional_data |>
  group_by(region, year, meat_type) |>
  summarise(total_production = sum(production_tonnes, na.rm = TRUE), .groups = "drop") |>
  # Calculate growth rates for annotation
  group_by(region, meat_type) |>
  arrange(year) |>
  mutate(
    growth_from_start = (total_production / first(total_production) - 1) * 100
  ) |>
  ungroup() |>
  # Order regions by total 2023 production for meaningful sorting
  group_by(region) |>
  mutate(region_total_2023 = sum(total_production[year == 2023], na.rm = TRUE)) |>
  ungroup() |>
  mutate(region = fct_reorder(region, desc(region_total_2023)))

# P2: Statistical precision data ----
precision_data <- regional_data |>
  filter(year >= 2010) |>
  group_by(region, meat_type) |>
  summarise(
    n_years = n(),
    mean_production = mean(production_tonnes, na.rm = TRUE),
    sd_production = sd(production_tonnes, na.rm = TRUE),
    se_production = sd_production / sqrt(n_years),
    .groups = "drop"
  ) |>
  mutate(
    # 95% confidence intervals
    lower_ci = pmax(0, mean_production - 1.96 * se_production),
    upper_ci = mean_production + 1.96 * se_production
  ) |>
  # Order meat types by total global production for facet sorting
  group_by(meat_type) |>
  mutate(meat_total = sum(mean_production)) |>
  ungroup() |>
  mutate(meat_type_ordered = fct_reorder(meat_type, desc(meat_total))) |>
  # Use tidytext::reorder_within for proper within-facet ordering
  mutate(region_ordered = reorder_within(region, mean_production, meat_type_ordered))


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "Beef & Buffalo" = "#8B4513",  
  "Pig" = "#E91E63",            
  "Poultry" = "#FF9800",        
  "Sheep & Goat" = "#2E7D32"    
))   


### |-  titles and caption ----
title_text <- str_glue("Global Meat Production Analysis: Trends & Statistical Precision")

subtitle_text <- str_glue(
  "Understanding 60+ years of regional production patterns through time trends and recent statistical analysis"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 35,
  source_text = "FAO via Our World in Data"
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
      size = rel(1.4), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "plot",
    legend.justification = "top",
    legend.margin = margin(l = 12, b = 5),
    legend.key.size = unit(0.8, "cm"),
    legend.box.margin = margin(b = 10),
    legend.title = element_text(face = "bold"),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(r = 10)
    ),
    axis.text.x = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = colors$text
    ),
    axis.text.y = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = colors$text
    ),

    # Grid lines
    panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1  Regional trends over time chart ----
p1 <- trends_data |>
  ggplot(aes(x = year, y = total_production, color = meat_type)) +
  
  # Geoms
  geom_line(size = 1.1, alpha = 0.9) +
  geom_smooth(method = "loess", se = FALSE, size = 0.8, alpha = 0.6, span = 0.3) +
  # Scales
  scale_color_manual(values = colors$palette, name = "Meat Type") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, 25),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Labs
  labs(
    title = "Regional Meat Production Evolution (1961-2023)",
    subtitle = str_glue(
      "Six decades of evolution (1961-2023) revealing long-term growth patterns and regional shifts<br>",
      "Fixed y-scale enables cross-regional comparison"
    ),
    x = "Year",
    y = "Production (Million Tonnes)"
  ) +
  # Facets
  facet_wrap(~region, scales = "fixed", ncol = 3) +
  # Theme
  theme(
    strip.text = element_text(face = "bold", size = rel(0.9), color = "white", family = fonts$text),
    strip.background = element_rect(fill = "#34495e", color = NA),
    )

### |- P2  Statistical precision chart----
p2 <- precision_data |>
  ggplot(aes(x = mean_production, y = region_ordered, color = meat_type)) +
  # Geoms
  geom_errorbarh(
    aes(xmin = lower_ci, xmax = upper_ci),
    height = 0.25, alpha = 0.7, linewidth = 0.5
  ) +
  geom_point(size = 2, alpha = 0.9) +
  # Scales
  scale_color_manual(values = colors$palette, guide = "none") +
  scale_x_continuous(
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_y_reordered() +
  # Labs
  labs(
    title = "Regional Production Averages with Statistical Confidence (2010-2023)",
    subtitle = str_glue(
      "Statistical precision analysis of current era (2010-2023) with 95% confidence intervals<br>",
      "Recent period provides reliable uncertainty measurements"
    ),
    x = "Average Annual Production (Million Tonnes)",
    y = "Region"
  ) +
  # Facets
  facet_wrap(~meat_type_ordered, scales = "free_y", ncol = 2) +
  # Theme
  theme(
    strip.text = element_text(face = "bold", size = rel(0.9), color = "white", family = fonts$text),
    strip.background = element_rect(fill = "#34495e", color = NA)
    )

### |-  combined plot ----
combined_plots <- p1 / p2 +
  plot_layout(heights = c(1, 1)) 

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.7),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ), 
      plot.subtitle = element_markdown(
        size = rel(0.95),
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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-26
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
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
# P ggplot2     * 3.5.2    2025-04-09 [?] RSPM (R 4.4.0)
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
# P janeaustenr   1.0.0    2022-08-26 [?] RSPM (R 4.4.0)
# janitor       2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
# knitr         1.49     2024-11-08 [1] RSPM (R 4.4.0)
# labeling      0.4.3    2023-08-29 [1] RSPM (R 4.4.0)
# P lattice       0.22-6   2024-03-20 [?] CRAN (R 4.4.0)
# lifecycle     1.0.4    2023-11-07 [1] RSPM (R 4.4.0)
# lubridate   * 1.9.4    2024-12-08 [1] RSPM (R 4.4.0)
# magick        2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
# magrittr      2.0.3    2022-03-30 [1] RSPM (R 4.4.0)
# markdown      1.13     2024-06-04 [1] RSPM (R 4.4.0)
# P Matrix        1.7-0    2024-03-22 [?] CRAN (R 4.4.0)
# P methods     * 4.4.0    2024-04-24 [?] local
# P mgcv          1.9-1    2023-12-21 [?] CRAN (R 4.4.0)
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P nlme          3.1-164  2023-11-27 [?] CRAN (R 4.4.0)
# P pacman      * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# readxl        1.4.3    2023-07-06 [1] RSPM (R 4.4.0)
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
# P SnowballC     0.7.1    2023-04-25 [?] RSPM (R 4.4.0)
# P splines       4.4.0    2024-04-24 [?] local
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
# P tidytext    * 0.4.3    2025-07-25 [?] RSPM (R 4.4.0)
# tidyverse   * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
# timechange    0.3.0    2024-01-18 [1] RSPM (R 4.4.0)
# P tokenizers    0.3.0    2022-12-22 [?] RSPM (R 4.4.0)
# P tools         4.4.0    2024-04-24 [?] local
# tzdb          0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
# utf8          1.2.4    2023-10-22 [1] RSPM (R 4.4.0)
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────
# > 
