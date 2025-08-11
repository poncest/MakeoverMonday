## Challenge: #MakeoverMondnay 2025 week 33
## Data:      Access to Electricity
## Author:    Steven Ponce
## Date:      2025-08-11

## Article
# https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS?view=map

## Data
# https://data.world/makeovermonday/2025w33-access-to-electricity-of-population

## Original Chart
# https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS?view=map


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork   # The Composer of Plots
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
access_electricity_raw <- read_csv("data/2025/Access to Electricity.csv") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(access_electricity_raw)
skimr::skim(access_electricity_raw)


## 4. TIDYDATA ----
access_electricity_tidy <- access_electricity_raw |>
  # Remove completely empty year columns (1960-1989, 2024)
  select(-c(x1960:x1989, x2024)) |>
  pivot_longer(
    cols = starts_with("x"),
    names_to = "year",
    values_to = "access_rate",
    names_prefix = "x" # Remove the "x" prefix
  ) |>
  mutate(year = as.numeric(year)) |>
  filter(!is.na(access_rate)) |>
  rename(
    country = country_name,
    code = country_code,
    indicator = indicator_name,
    indicator_id = indicator_code
  ) |>
  arrange(country, year)

# convergence analysis data
convergence_data <- access_electricity_tidy |>
  filter(year %in% c(2000, 2023)) |>
  group_by(country) |>
  filter(n() == 2) |>
  summarise(
    access_2000 = access_rate[year == 2000],
    access_2023 = access_rate[year == 2023],
    improvement = access_2023 - access_2000,
    .groups = "drop"
  ) |>
  filter(!is.na(access_2000), !is.na(access_2023)) |>
  mutate(
    cohort = case_when(
      access_2000 < 25 ~ "Very Low Start (<25%)",
      access_2000 < 50 ~ "Low Start (25-50%)",
      access_2000 < 75 ~ "Medium Start (50-75%)",
      access_2000 < 95 ~ "High Start (75-95%)",
      TRUE ~ "Universal Start (≥95%)"
    )
  )

# Countries with dramatic improvements (>= 50 percentage points)
# from Very Low Start only
top_improvers <- convergence_data |>
  filter(cohort == "Very Low Start (<25%)", improvement >= 50) |>
  arrange(desc(improvement))

# Define the logical 
cohort_levels <- c(
  "Very Low Start (<25%)",
  "Low Start (25-50%)",
  "Medium Start (50-75%)",
  "High Start (75-95%)",
  "Universal Start (≥95%)"
)

# cohort analysis data
cohort_analysis <- access_electricity_tidy |>
  filter(year == 2000) |>
  mutate(
    starting_cohort = case_when(
      access_rate < 25 ~ "Very Low Start (<25%)",
      access_rate < 50 ~ "Low Start (25-50%)",
      access_rate < 75 ~ "Medium Start (50-75%)",
      access_rate < 95 ~ "High Start (75-95%)",
      TRUE ~ "Universal Start (≥95%)"
    ),
    # Order factor levels for meaningful legend ordering (low to high)
    starting_cohort = factor(starting_cohort, levels = cohort_levels)
  ) |>
  select(country, starting_cohort) |>
  inner_join(access_electricity_tidy, by = "country") |>
  filter(year >= 2000) |>
  group_by(starting_cohort, year) |>
  summarise(
    median_access = median(access_rate, na.rm = TRUE),
    q25 = as.numeric(quantile(access_rate, 0.25, na.rm = TRUE)),  
    q75 = as.numeric(quantile(access_rate, 0.75, na.rm = TRUE)), 
    countries = n(),
    .groups = "drop"
  ) |>
  filter(countries >= 3)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  "Very Low Start (<25%)" = "#8B0000",    
  "Low Start (25-50%)" = "#FF8C00",       
  "Medium Start (50-75%)" = "#006400",   
  "High Start (75-95%)" = "#1E90FF",      
  "Universal Start (≥95%)" = "#4B0082"    
))

### |-  titles and caption ----
title_text <- str_glue("Global convergence in electricity access, 2000-2023")

subtitle_text <- str_glue(
  "Evidence shows countries starting with less electricity access are improving faster"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 33,
  source_text = "World Bank Group"
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
    plot.subtitle = element_text(
      size = rel(0.9), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "right",
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

### |- P1  scatter plot----
p1 <- ggplot(convergence_data, aes(x = access_2000, y = improvement)) +
  # Geoms
  geom_vline(
    xintercept = 25,
    linetype = "dashed",
    color = "#E8516F",
    alpha = 0.6,
    linewidth = 0.5
  ) +
  geom_point(
    aes(color = cohort),
    size = 2.8,
    alpha = 0.85,
    stroke = 0
  ) +
  ggrepel::geom_text_repel(
    data = top_improvers,
    aes(label = country),
    vjust = 1.3,
    hjust = 0.5,
    size = 3,
    family = "sans",
    color = alpha(colors$text, 0.9),
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.5,        
    point.padding = 0.3,      
    segment.color = alpha(colors$text, 0.6),  
    segment.size = 0.3,       
    max.overlaps = Inf,      
    min.segment.length = 0.1, 
    seed = 42                
  ) +
  # Scales
  scale_x_continuous(
    labels = percent_format(scale = 1),
    name = "Access Rate in 2000",
    breaks = seq(0, 100, 20),
    limits = c(-5, 100),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    name = "Improvement (percentage points)",
    breaks = seq(-30, 100, 20),
    limits = c(-30, 100),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_color_manual(
    values = colors$palette,
    name = "Starting Level\n(2000)"
  ) +
  # Labs
  labs(
    title = "Convergence in electricity access",
    subtitle = "Lower starting access rates correlate with greater improvements (2000-2023)"
  ) +
  # Theme
  theme(
    legend.position = "none"
  )

### |- P2  line chart----
p2 <- ggplot(cohort_analysis, aes(x = year, y = median_access, color = starting_cohort)) +
  # Geoms
  geom_ribbon(
    aes(ymin = q25, ymax = q75, fill = starting_cohort),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    linewidth = 1.2,
    alpha = 0.9
  ) +
  geom_hline(
    yintercept = c(50, 75, 90),
    linetype = "dotted",
    color = "#bdc3c7",
    alpha = 0.7,
    linewidth = 0.4
  ) +
  # Scales
  scale_x_continuous(
    name = "Year",
    breaks = seq(2000, 2025, 5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    name = "Median Access Rate",
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_color_manual(
    values = unlist(colors$palette),
    name = "Starting Level\n(2000)"
  ) +
  scale_fill_manual(
    values = colors$palette,
    name = "Starting Level\n(2000)"
  ) +
  # Labs
  labs(
    title = "Progress by starting access level",
    subtitle = "Median access rates over time, grouped by 2000 baseline levels"
  ) +
  # Theme
  theme(
    legend.position = "right",
    legend.justification = "top"
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(linewidth = 2, alpha = 1)
    ),
    fill = "none"
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

# ─ Session info ─────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-11
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
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
# P ggrepel       0.9.6    2024-09-07 [?] RSPM (R 4.4.0)
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
# ────────────────────────────────────────────────────────────────────────
# > 