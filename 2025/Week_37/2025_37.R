## Challenge: #MakeoverMondnay 2025 week 37
## Data:      AI Impact on Job Market: (2024–2030)
## Author:    Steven Ponce
## Date:      2025-09-09

## Article
# https://www.kaggle.com/datasets/sahilislam007/ai-impact-on-job-market-20242030?

## Data
# https://data.world/makeovermonday/2025week-37

## Original Chart
# None available

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
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
ai_job_trends <- read_csv("data/2025/ai_job_trends_dataset.csv") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(ai_job_trends)
skimr::skim_without_charts(ai_job_trends)


## 4. TIDYDATA ----
# Helpers
fmt_km <- function(x) {
  case_when(
    abs(x) >= 1e6 ~ paste0(number(x / 1e6, accuracy = 0.1), "M"),
    abs(x) >= 1e3 ~ paste0(number(x / 1e3, accuracy = 1), "K"),
    TRUE ~ number(x, accuracy = 1)
  )
}

ai_data_clean <- ai_job_trends |>
  filter(!is.na(job_openings_2024), !is.na(projected_openings_2030)) |>
  mutate(job_change = projected_openings_2030 - job_openings_2024)

### |- P1: Slope chart data ----
slope_data <- ai_data_clean |>
  group_by(industry) |>
  summarise(
    openings_2024 = sum(job_openings_2024),
    openings_2030 = sum(projected_openings_2030),
    net_change = sum(job_change),
    abs_change = abs(sum(job_change)),
    .groups = "drop"
  ) |>
  arrange(desc(abs_change)) |>
  mutate(
    rank_by_change = row_number(),
    is_top_change = rank_by_change <= 2,
    story_type = case_when(
      is_top_change & net_change > 0 ~ "biggest_growth",
      is_top_change & net_change < 0 ~ "biggest_decline",
      TRUE ~ "background"
    )
  ) |>
  pivot_longer(c(openings_2024, openings_2030), names_to = "year", values_to = "openings") |>
  mutate(year_clean = if_else(year == "openings_2024", 2024, 2030))

# labels for P1
labels_2030 <- slope_data |>
  filter(story_type != "background", year_clean == 2030) |>
  mutate(x_lab = year_clean + 0.25)

### |- P2: Waterfall chart data ----
wf_core <- ai_data_clean |>
  group_by(industry) |>
  summarise(change = sum(job_change), .groups = "drop") |>
  arrange(change) |>
  mutate(idx = row_number())

waterfall_data <- wf_core |>
  arrange(idx) |>
  mutate(
    step_start = lag(cumsum(change), default = 0),
    step_end = cumsum(change),
    type = case_when(
      industry == "Total" ~ "Total",
      change >= 0 ~ "Job Gains",
      TRUE ~ "Job Losses"
    ),
    label_y = pmax(step_start, step_end) + max(abs(change)) * 0.035,
    lbl = if_else(industry == "Total",
      paste0(if_else(change >= 0, "+", ""), fmt_km(change)),
      fmt_km(change)
    ),
    lab_wrapped = str_wrap(industry, width = 10)
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = c(
    
  "biggest_growth"  = "#2E6F77",  
  "biggest_decline" = "#D97B66",
  
  "Job Gains" = "#3F5E9A",    
  "Job Losses" = "#A7B6CF",   
  "Total"     = "#2C4476"  
))   

### |-  titles and caption ----
title_text <- str_glue("AI's Job Market Impact: Transformation with Net Growth")

subtitle_text <- str_glue(
  "Analysis of **synthetic job market** data modeling AI adoption scenarios (2024-2030)"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 37,
  source_text = "AI Job Trend via Kaggle"
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
    # axis.ticks.length = unit(0.2, "cm"),
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
    panel.grid.minor = element_blank(), #element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)   

### |- P1: Slope chart ----
p1 <- slope_data |>
  ggplot(aes(x = year_clean, y = openings, group = industry)) +
  # Geoms
  geom_line(
    data = slope_data |> filter(story_type == "background"), # background lines
    color = "gray", linewidth = 1.5, alpha = 0.6
  ) +
  geom_line(
    data = slope_data |> filter(story_type != "background"), # highlights lines
    aes(color = story_type), linewidth = 3
  ) +
  geom_point(
    data = slope_data |> filter(story_type == "background"), # background points
    color = "gray", size = 3, alpha = 0.6
  ) +
  geom_point(
    data = slope_data |> filter(story_type != "background"), # highlights points
    aes(color = story_type), size = 5
  ) +
  geom_text(
    data = slope_data |> filter(story_type != "background"),
    aes(label = fmt_km(openings), colour = story_type),
    vjust = -1.8, fontface = "bold", size = 4.0, show.legend = FALSE
  ) +
  geom_text(
    data = labels_2030,
    aes(x = x_lab, y = openings, label = industry, color = story_type),
    hjust = 0, fontface = "bold", size = 4.2, show.legend = FALSE
  ) +
  # Scales
  scale_x_continuous(breaks = c(2024, 2030), limits = c(2023.6, 2031.7)) +
  scale_y_continuous(labels = function(x) fmt_km(x)) +
  scale_colour_manual(values = colors$palette) +
  # Labs
  labs(
    title = "AI Creates Winners and Losers",
    subtitle = "Industries with the largest absolute change in job openings (2024 - 2030)",
    x = NULL, y = "Total Job Openings"
  ) +
  # Theme
  theme(
    plot.margin = margin(t = 10, r = 72, b = 10, l = 10),
    legend.position = "none"
  )

### |- P2: Waterfall chart ----
p2 <- ggplot(waterfall_data) +
  # Geoms
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "gray") +
  geom_rect(
    aes(
      xmin = idx - 0.38, xmax = idx + 0.38,
      ymin = step_start, ymax = step_end,
      fill = type
    ),
    colour = "white", linewidth = 1.1, alpha = 0.96
  ) +
  geom_segment(
    data = filter(waterfall_data, idx < max(idx)),
    aes(
      x = idx + 0.38, xend = (idx + 1) - 0.38,
      y = step_end, yend = step_end
    ),
    linetype = "dashed", colour = "black", alpha = 0.7
  ) +
  geom_text(aes(x = idx, y = label_y, label = lbl),
    fontface = "bold", size = 4.0, color = colors$text,
    vjust = 0
  ) +
  # Scales
  scale_fill_manual(values = colors$palette, name = "Net Impact", drop = FALSE) +
  scale_x_continuous(
    breaks = waterfall_data$idx,
    labels = waterfall_data$lab_wrapped,
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 1, scale_cut = cut_short_scale())) +
  # Labs
  labs(
    title = "Net Result: AI Creates Jobs",
    subtitle = "Cumulative impact across industries",
    x = NULL, y = "Cumulative Job Change"
  ) +
  # Theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

### |- final combined plot ----
combined_plots <- p1 / p2 +
  plot_layout(heights = c(1.5, 1))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.8),
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
        lineheight = 1.2,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.6),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 05)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-09
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# bit           4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
# bit64         4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cli           3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
# colorspace    2.1-1    2024-07-26 [1] RSPM (R 4.4.0)
# P compiler      4.4.0    2024-04-24 [?] local
# crayon        1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
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
# here          1.0.1    2020-12-13 [1] RSPM (R 4.4.0)
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
# P methods     * 4.4.0    2024-04-24 [?] local
# munsell       0.5.1    2024-04-01 [1] RSPM (R 4.4.0)
# P pacman        0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
# P parallel      4.4.0    2024-04-24 [?] local
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
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────
# > 
