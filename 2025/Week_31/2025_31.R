
## Challenge: #MakeoverMondnay 2025 week 31
## Data:      Wages Across Europe
## Author:    Steven Ponce
## Date:      2025-07-29

## Article
# https://www.datawrapper.de/blog/low-wages-europe

## Data
# https://data.world/makeovermonday/2025-week-31-wages-across-europe

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
  width  = 12,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
eu_wages <- read_csv("data/2025/wages.csv") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(eu_wages)
skimr::skim(eu_wages)


## 4. TIDYDATA ----
# Custom euro formatting function
format_euro <- function(x) {
  paste0("€", scales::comma(x, accuracy = 1))
}

# Data tidying
eu_wages_long <- eu_wages |>
  mutate(date = dmy(date)) |>
  pivot_longer(
    cols = -date, 
    names_to = "country",
    values_to = "wage"
  ) |>
  mutate(
    country = str_to_title(country),
    year = year(date),
    wage_euros = wage,
    wage_thousands = wage / 1000
  ) |>
  arrange(country, date)

# Calculate additional metrics
eu_wages_summary <- eu_wages_long |>
  group_by(country) |>
  summarise(
    start_wage = first(wage_euros),
    end_wage = last(wage_euros),
    total_change = end_wage - start_wage,
    pct_change = (end_wage - start_wage) / start_wage * 100,
    avg_wage = mean(wage_euros),
    .groups = "drop"
  ) |>
  arrange(desc(end_wage))

# Calculate average 
current_avg <- mean(eu_wages_summary$end_wage)

# Dumbell data
dumbell_data <- eu_wages_summary |>
  mutate(
    country = fct_reorder(country, end_wage),
    above_avg = end_wage > current_avg
  )

# Diverging data
diverging_chart_data <- eu_wages_summary |>
  mutate(
    diff_from_avg = end_wage - current_avg,
    country = fct_reorder(country, diff_from_avg),
    above_avg = diff_from_avg > 0
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  above_avg = "#1f77b4", 
  below_avg = "#d62728", 
  start_point = "#ff7f0e", 
  end_point = "#2ca02c", 
  connection = "#7f7f7f", 
  text_dark = "#2f2f2f", 
  text_light = "#666666"
))

### |-  titles and caption ----
title_text <- str_glue("European Minimum Wages Rose 23-87% Since 2020, With Eastern Europe Leading Growth")

subtitle_text <- str_glue(
  "Growth trajectories and current positioning relative to €1,686 EU average, monthly statutory wages<br><br>",
  "Gray circles: 2020 starting wages | Colored dots/bars: 2025 final position (Blue: Above average, Red: Below average)"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 31,
  source_text = "Eurostat"
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
    legend.position = "plot",
    legend.box.margin = margin(b = 10),
    legend.margin = margin(b = 5),
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
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |-  Plot 1  Dumbbell Chart (The Journey) ----
p1 <- dumbell_data |> 
  ggplot(aes(y = country)) +
  # Geoms
  geom_segment(aes(x = start_wage, xend = end_wage, 
                   color = above_avg), 
               linewidth = 1.1, alpha = 0.8) +
  geom_point(aes(x = start_wage), 
             color = "gray30", fill = "white",
             size = 3.5, shape = 21, stroke = 1.5) +
  geom_point(aes(x = end_wage, color = above_avg), 
             size = 4, alpha = 0.9) +
  geom_text(aes(x = end_wage,
                label = paste0("+", round(pct_change, 1), "%")),
            hjust = -0.35, size = 3.2, fontface = "bold", 
            color = colors$palette$text_dark) +
  # Scales
  scale_color_manual(
    values = c("TRUE" = colors$palette$above_avg, 
               "FALSE" = colors$palette$below_avg),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = format_euro,
    expand = expansion(mult = c(0.1, 0.25)),
    limits = c(500, 2800),
    breaks = seq(500, 2500, 500)
  ) +
  # Labs
  labs(
    title = "A. Growth Journey (2020 --> 2025)",
    subtitle = "How countries transformed their minimum wages",
    x = "Monthly Minimum Wage",
    y = NULL
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      hjust = 0.5,
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(1.1),
      hjust = 0.5,
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 0.9,
      margin = margin(t = 5, b = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
  )

### |-  Plot 2 Diverging Bar Chart (Current Position) ----
p2 <- diverging_chart_data |> 
  ggplot(aes(x = country, y = diff_from_avg, fill = above_avg)) +
  # Geoms
  geom_col(width = 0.75, alpha = 0.85) +
  geom_hline(yintercept = 0, color = colors$palette$text_dark, 
             size = 1, alpha = 0.8) +
  geom_text(
    aes(label = format_euro(abs(diff_from_avg)),
        hjust = ifelse(diff_from_avg > 0, -0.15, 1.15)),
    size = 3.2, fontface = "bold", 
    color = colors$palette$text_dark
  ) +
  # Scales
  scale_fill_manual(
    values = c("TRUE" = colors$palette$above_avg, 
               "FALSE" = colors$palette$below_avg),
    labels = c("Below Average", "Above Average"),
    name = NULL
  ) +
  scale_y_continuous(
    labels = format_euro,
    breaks = scales::pretty_breaks(n = 6),
    limits = c(-1100, 1200)
  ) +
  coord_flip() +
  # Labs
  labs(
    title = "B. Current Position vs European Average",
    subtitle = paste0("Deviation from ", format_euro(round(current_avg)), " average"),
    x = NULL,
    y = "Difference from Average"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.5),
      family = fonts$title,
      face = "bold",
      hjust = 0.5,
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(1.1),
      hjust = 0.5,
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 0.9,
      margin = margin(t = 5, b = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
  )

### |-  combined plot ----
# Create an invisible spacer plot
spacer <- ggplot() + 
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

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
      plot.title = element_text(
        size = rel(1.6),
        family = fonts$title,
        face = "bold",
        hjust = 0.5,
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1.1),
        hjust = 0.5,
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 0.9,
        margin = margin(t = 5, b = 0)
      ),
      plot.caption = element_markdown(
        size = rel(0.75),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/La_Paz
# date     2025-07-29
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────
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
# ───────────────────────────────────────────────
# > 
