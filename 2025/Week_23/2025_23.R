
## Challenge: #MakeoverMondnay 2025 week 23
## Data:      What future will U.S. truck manufacturing have under Trump?
## Author:    Steven Ponce
## Date:      2025-06-02

## Original Chart
# Change in employment from baseline scenario, Job-years, 2024–2032, Figure B
# https://www.epi.org/publication/future-clean-trucks-buses/

## Article
# What future will U.S. truck manufacturing have under Trump?
# https://www.epi.org/publication/future-clean-trucks-buses/

## Data
# What future will U.S. truck manufacturing have under Trump?
# https://data.world/makeovermonday/what-future-will-us-truck-manufacturing-have-under-trump


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    lubridate,      # Make Dealing with Dates a Little Easier
    patchwork       # The Composer of Plots
    )

### |- figure size ----
camcorder::gg_record(
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
truck_manufacturing_raw <- readxl::read_excel('data/2025/US truck manufacturing under Trump.xlsx') |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(truck_manufacturing_raw)
skimr::skim(truck_manufacturing_raw)


## 4. TIDYDATA ----

# P1. heatmap data----
heatmap_data <- truck_manufacturing_raw |>
  mutate(
    scenario_clean = case_when(
      str_detect(scenario, "policy retreat") ~ "Policy Retreat",
      str_detect(scenario, "union wages") ~ "Clean + Market + Union",
      str_detect(scenario, "50% clean.*10% market") ~ "Clean + Market Share",
      str_detect(scenario, "Baseline.*10%") ~ "Baseline + Market",
      str_detect(scenario, "50% clean") ~ "50% Clean Vehicles",
      TRUE ~ scenario
    ),
    # Order scenarios logically
    scenario_factor = factor(scenario_clean,
      levels = c(
        "50% Clean Vehicles",
        "Baseline + Market",
        "Clean + Market Share",
        "Clean + Market + Union",
        "Policy Retreat"
      )
    )
  ) |>
  # Transform to long format
  select(scenario_factor, assembly, supply_chain, total) |>
  pivot_longer(
    cols = -scenario_factor,
    names_to = "job_category",
    values_to = "jobs"
  ) |>
  # Create clean category labels
  mutate(
    job_category_clean = case_when(
      job_category == "assembly" ~ "Assembly",
      job_category == "supply_chain" ~ "Supply Chain",
      job_category == "total" ~ "Total"
    ),
    job_category_factor = factor(job_category_clean,
      levels = c("Assembly", "Supply Chain", "Total")
    ),
    # Create scaled values for color intensity (-1 to 1 scale)
    jobs_scaled = case_when(
      jobs < 0 ~ pmax(jobs / abs(min(jobs)), -1), # negative values
      jobs > 0 ~ pmin(jobs / max(jobs), 1), # positive values
      TRUE ~ 0
    ),
    # Text color based on intensity
    text_color = ifelse(abs(jobs_scaled) > 0.6, "white", "black"),
    # Formatted labels
    jobs_label = case_when(
      jobs == 0 ~ "0",
      jobs > 0 ~ paste0("+", comma(jobs, accuracy = 1)),
      jobs < 0 ~ comma(jobs, accuracy = 1)
    )
  )

# P2. waterfall chart data -----
waterfall_data <- tibble(
  category = c(
    "Baseline", "50% Clean Vehicles", "Add Market Share",
    "Add Union Wages", "Policy Retreat Alternative"
  ),
  # Calculate incremental values
  value = c(
    0, # Baseline starting point
    truck_manufacturing_raw$total[1], # 50% clean vehicles: 11,182
    truck_manufacturing_raw$total[2] - truck_manufacturing_raw$total[1], # Market share increment: 13,272 - 11,182 = 2,090
    truck_manufacturing_raw$total[4] - truck_manufacturing_raw$total[3], # Union wages increment: 19,122 - 15,441 = 3,681
    truck_manufacturing_raw$total[5] # Policy retreat: -3,332 (standalone negative scenario)
  ),
  type = c("baseline", "positive", "positive", "positive", "negative")
) |>
  # Calculate cumulative totals and bar positioning
  mutate(
    cumulative = case_when(
      category == "Policy Retreat Alternative" ~ value,
      TRUE ~ cumsum(value)
    ),
    # Calculate where each bar should start and end
    ymin = case_when(
      category == "Baseline" ~ 0,
      category == "Policy Retreat Alternative" ~ 0,
      TRUE ~ lag(cumulative, default = 0)
    ),
    ymax = case_when(
      category == "Policy Retreat Alternative" ~ value,
      TRUE ~ cumulative
    ),
    # Labels
    category_label = case_when(
      category == "Baseline" ~ "Baseline\n(Starting Point)",
      category == "50% Clean Vehicles" ~ "50% Clean\nVehicles",
      category == "Add Market Share" ~ "Increase\nMarket Share",
      category == "Add Union Wages" ~ "Add Union\nWages",
      category == "Policy Retreat Alternative" ~ "Policy Retreat\n(Alternative Path)"
    ),
    category = factor(category, levels = category),
    category_label = factor(category_label, levels = category_label)
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "#DC143C", "#FFFFFF",  "#2E8B57",                                         # heatmap    
  "baseline" = "#708090", "positive" = "#2E8B57", "negative" = "#DC143C"    # waterfall chart
))
  
### |-  titles and caption ----
title_text <- str_glue("The Stakes for U.S. Truck Manufacturing Under Shifting Federal Policy")
subtitle_text <- str_glue("Employment impact analysis across clean vehicle scenarios and policy retreat alternatives")

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 23,
    source_text = "<br>EPI analysis of S&P Global (2024), IMPLAN (2024), and FRED data | Job-years, 2024-2032"
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
    legend.position = "plot",
    legend.title = element_text(face = "bold"),

    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5), 
    axis.ticks.length = unit(0.2, "cm"), 

    # Axis formatting
    axis.title.x = element_text(face = "bold", size = rel(0.85)),
    axis.title.y = element_text(face = "bold", size = rel(0.85)),
    axis.text.y = element_text(face = "bold", size = rel(0.85)),
    
    # Grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

# P1. Heatmap ----
p1 <- heatmap_data |>
  ggplot(aes(x = job_category_factor, y = scenario_factor)) +

  # Geoms
  geom_tile(aes(fill = jobs_scaled),
    color = "white",
    linewidth = 1.5,
    alpha = 0.9
  ) +
  geom_text(aes(label = jobs_label, color = text_color),
    fontface = "bold",
    size = 4.2
  ) +

  # Scales
  scale_fill_gradient2(
    low = colors$palette[1],
    mid = colors$palette[2],
    high = colors$palette[3],
    midpoint = 0,
    name = "Job Impact\nIntensity",
    labels = c("High Loss", "Neutral", "High Gain"),
    breaks = c(-1, 0, 1),
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 1.2,
      barheight = 8,
      frame.colour = "gray70",
      ticks.colour = "gray70"
    )
  ) +
  scale_color_identity() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +

  # Labs
  labs(
    subtitle = "Job breakdown shows potential gains and losses across Assembly and Supply Chain sectors",
    x = "Job Category",
    y = "Policy Scenario"
  ) +

  # Theme
  theme_minimal(base_size = 11) +
  theme(
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      margin = margin(b = 15)
    ),
    axis.title = element_text(
      size = 10,
      face = "bold",
      color = "gray30"
    ),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(
      size = 9,
      color = "gray20",
      face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 9,
      face = "bold",
      color = "gray30"
    ),
    legend.text = element_text(
      size = 8,
      color = "gray40"
    ),
    legend.margin = margin(l = 15),
    panel.grid = element_blank(),
    plot.margin = margin(10, 15, 15, 15),
    plot.background = element_rect(fill = colors$background, color = NA),
    panel.background = element_rect(fill = colors$background, color = NA)
  )

# P2. waterfall chart -----
p2 <- waterfall_data |>   
  ggplot(aes(x = category_label)) +

  # Geoms
  geom_segment(
    data = waterfall_data[2:4, ],
    aes(
      x = as.numeric(category_label) + 0.45,
      xend = as.numeric(category_label) + 1 - 0.45,
      y = ymax,
      yend = ymax
    ),
    linetype = "dashed", alpha = 0.6, linewidth = 0.8, color = "gray50"
  ) +
  geom_rect(
    aes(
      xmin = as.numeric(category_label) - 0.35,
      xmax = as.numeric(category_label) + 0.35,
      ymin = ymin,
      ymax = ymax,
      fill = type
    ),
    alpha = 0.9, color = "white", linewidth = 0.5
  ) +
  geom_text(
    aes(
      y = (ymin + ymax) / 2,
      label = ifelse(value == 0, "0",
        paste0(ifelse(value > 0, "+", ""), comma(value, accuracy = 1))
      )
    ),
    fontface = "bold", size = 4, color = "white"
  ) +
  geom_text(
    data = filter(waterfall_data, type != "baseline" & type != "negative"),
    aes(
      y = ymax + 500,
      label = paste("Total:", comma(cumulative, accuracy = 1))
    ),
    fontface = "bold", size = 3.5, color = "gray30"
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1, alpha = 0.8) +

  # Scales
  scale_fill_manual(values = colors$palette, guide = "none") +
  scale_y_continuous(
    labels = function(x) {
      case_when(
        x >= 0 ~ paste0("+", comma(x)),
        x < 0 ~ comma(x),
        TRUE ~ as.character(x)
      )
    },
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0.15, 0.1))
  ) +
  scale_x_discrete() +

  # Labs
  labs(
    subtitle = "Policy scenarios build progressively from baseline to maximum employment potential",
    x = NULL,
    y = "Change in Employment (Job-years)"
  ) +

  # Theme
  theme_minimal(base_size = 11) +
  theme(
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      margin = margin(b = 15)
    ),
    axis.title.y = element_text(
      size = 10,
      face = "bold",
      color = "gray30",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      size = 9,
      face = "bold",
      color = "gray20",
      lineheight = 1.1
    ),
    axis.text.y = element_text(
      size = 9,
      color = "gray30"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    plot.margin = margin(15, 15, 10, 15),
    plot.background = element_rect(fill = colors$background, color = NA),
    panel.background = element_rect(fill = colors$background, color = NA)
  )

# Combined Plot ----
combined_plot <- p1 / p2 +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.6),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_text(
        size = rel(1),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 0.9,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.6),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      ),
     plot.margin = margin(25, 25, 25, 25),
    )
  )

combined_plot


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
# date     2025-06-02
# rstudio  2025.05.0+496 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# P annotater     0.2.3    2024-01-26 [?] RSPM (R 4.4.0)
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
# janitor       2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
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
# P R.cache       0.16.0   2022-07-21 [?] RSPM (R 4.4.0)
# P R.methodsS3   1.8.2    2022-06-13 [?] RSPM (R 4.4.0)
# P R.oo          1.27.0   2024-11-01 [?] RSPM (R 4.4.0)
# P R.utils       2.12.3   2023-11-18 [?] RSPM (R 4.4.0)
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
# P stats       * 4.4.0    2024-04-24 [?] local
# stringi       1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
# stringr     * 1.5.1    2023-11-14 [1] RSPM (R 4.4.0)
# P styler        1.10.3   2024-04-07 [?] RSPM (R 4.4.0)
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
# viridisLite   0.4.2    2023-05-02 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────────────────────
# >