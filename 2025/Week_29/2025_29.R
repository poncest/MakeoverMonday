
## Challenge: #MakeoverMondnay 2025 week 29
## Data:      Trump’s deportation agenda will destroy millions of jobs
## Author:    Steven Ponce
## Date:      2025-07-15

## Article
# https://www.epi.org/publication/trumps-deportation-agenda-will-destroy-millions-of-jobs-both-immigrants-and-u-s-born-workers-would-suffer-job-losses-particularly-in-construction-and-child-care/

## Data
# https://data.world/makeovermonday/2025wk-29-trumps-deportations-will-reduce-employment

# IMPORTANT CONTEXT:
# - Baseline data: 2024 Current Population Survey + FY 2024 deportation data (330K annually)
# - Projection period: 2025-2029 (4 years at 1M deportations annually)
# - Analysis published: July 10, 2025 by Economic Policy Institute
# - These are PROJECTED impacts, not historical data

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  scales,         # Scale Functions for Visualization
  glue,           # Interpreted String Literals
  patchwork       # The Composer of Plots
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
deportations_raw <- readxl::read_excel("data/2025/Trump Deportations.xlsx") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(deportations_raw)
skimr::skim(deportations_raw)


## 4. TIDYDATA ----
deportations <- deportations_raw |>
  filter(state != "United States") |> # Remove national totals
  mutate(
    impact_tier = case_when(
      total_level >= 500000 ~ "mega",
      total_level >= 200000 ~ "high",
      total_level >= 100000 ~ "medium",
      TRUE ~ "lower"
    )
  )

### |-  Plot 1 Data ----
barchart_data <- deportations |>
  top_n(20, total_level) |>
  mutate(state = fct_reorder(state, total_level))

### |-  Plot 2 Data ----
# Get the same top 20 states as P1 in the same order
p1_states_ordered <- deportations |>
  top_n(20, total_level) |>
  arrange(desc(total_level)) |>
  pull(state)

dumbbell_data <- deportations |>
  filter(state %in% p1_states_ordered) |>
  mutate(state = factor(state, levels = rev(p1_states_ordered))) |> # rev() for ggplot ordering
  pivot_longer(cols = c(u_s_born, immigrant), names_to = "worker_type", values_to = "jobs_lost") |>
  mutate(
    worker_type = case_when(
      worker_type == "u_s_born" ~ "U.S.-Born Workers",
      worker_type == "immigrant" ~ "Immigrant Workers"
    )
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  # Scale tiers for bar chart
  mega = "#c0392b", # mega impact (500K+)
  high = "#f39c12", # high impact (200-500K)
  medium = "#3498db", # medium impact (100-200K)
  lower = "#95a5a6",

  # Worker types for dumbbell
  immigrant = "#e74c3c", # immigrant workers
  us_born = "#3498db", # US-born workers
  connector = "#bdc3c7" # connecting lines
))

### |-  titles and caption ----
title_text <- str_glue("Mass Deportations Would Devastate Employment Across Multiple States")
subtitle_text <- str_glue("Projected employment impacts 2025-2029 vs. 2024 baseline")

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 29,
  source_text = "<br>Economic Policy Institute (July 2025) | Projections based on 1M annual deportations vs. 330K baseline"
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
      color = colors$title, lineheight = 1.1,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9), hjust = 0.5, family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9), lineheight = 0.9,
      margin = margin(t = 5, b = 20)
    ),

    # Legend formatting
    legend.position = "top",
    legend.direction = "horizontal",
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

    # Legends

    # Grid lines
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot 1  bar chart ----
p1 <- barchart_data |>
  ggplot(aes(x = total_level, y = state, fill = impact_tier)) +
  # Geoms
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(
    aes(label = case_when(
      total_level >= 1000000 ~ paste0(round(total_level / 1000000, 1), "M"),
      total_level >= 1000 ~ paste0(round(total_level / 1000, 0), "K"),
      TRUE ~ scales::comma(total_level)
    )),
    hjust = -0.1,
    size = 3,
    color = "gray30"
  ) +
  # Scales
  scale_x_continuous(
    labels = comma_format(suffix = "K", scale = 1 / 1000),
    expand = c(0, 0, 0.15, 0), # More space for labels
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_fill_manual(
    values = colors$palette,
    labels = c(
      "mega" = "500K+ jobs",
      "high" = "200-500K jobs",
      "medium" = "100-200K jobs",
      "lower" = "<100K jobs"
    ),
    name = NULL
  ) +
  # Labs
  labs(
    title = "Scale of Impact: CA and TX Dominate Job Losses",
    subtitle = NULL,
    x = "Projected Job Losses (thousands)",
    y = NULL
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    override.aes = list(alpha = 0.9)
  ))

### |-  Plot 2  dumbbell chart ----
p2 <- dumbbell_data |>
  ggplot(aes(y = state)) +
  # Geoms
  geom_segment(
    data = deportations |>
      filter(state %in% p1_states_ordered) |>
      mutate(state = factor(state, levels = rev(p1_states_ordered))),
    aes(x = u_s_born, xend = immigrant, y = state, yend = state),
    color = colors$palette[7],
    linewidth = 1.5,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  geom_point(aes(x = jobs_lost, color = worker_type), size = 3.2, alpha = 0.9) +
  geom_text(
    data = deportations |>
      filter(state %in% c("California", "Texas")) |>
      select(state, u_s_born, immigrant) |>
      pivot_longer(cols = c(u_s_born, immigrant), names_to = "worker_type", values_to = "jobs_lost") |>
      mutate(
        state = factor(state, levels = rev(p1_states_ordered)),
        label = paste0(round(jobs_lost / 1000), "K")
      ),
    aes(x = jobs_lost, y = state, label = label),
    hjust = -0.3,
    vjust = 1.4,
    size = 2.5,
    color = "gray30"
  ) +
  # Scales
  scale_x_continuous(
    labels = comma_format(suffix = "K", scale = 1 / 1000),
    expand = c(0.05, 0, 0.2, 0),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_color_manual(
    values = c(
      "U.S.-Born Workers" = colors$palette$us_born,
      "Immigrant Workers" = colors$palette$immigrant
    ),
    breaks = c("U.S.-Born Workers", "Immigrant Workers"),
    name = NULL
  ) +
  # Labs
  labs(
    title = "Both Worker Types Affected: No One is Spared",
    subtitle = NULL,
    x = "Jobs Lost (thousands)",
    y = NULL,
    caption = "Top 20 states by total job losses shown"
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
  ) +
  guides(color = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    nrow = 1,
    override.aes = list(size = 4)
  ))

### |-  combined plot ----
# Create an invisible spacer plot
spacer <- ggplot() +
  theme_void()

# Use it between charts
combined_plots <- (p1 + spacer + p2) +
  plot_layout(widths = c(1, 0.2, 1.1))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.8),
        family = fonts$title,
        face = "bold",
        hjust = 0.5,
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1.2),
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

# ─ Session info ─────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-07-15
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
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
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────────
# > 