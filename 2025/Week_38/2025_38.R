## Challenge: #MakeoverMondnay 2025 week 38
## Data:      Which ‘kidult’ hobbies do Britons think are for children?
## Author:    Steven Ponce
## Date:      2025-09-16

## Article
# https://yougov.co.uk/society/articles/52851-which-kidult-hobbies-and-practices-do-britons-think-are-really-for-children

## Data
# https://data.world/makeovermonday/do-younger-britons-see-kidult-hobbies-as-less-childish

## Original Chart
# https://yougov.co.uk/society/articles/52851-which-kidult-hobbies-and-practices-do-britons-think-are-really-for-children

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue        # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
kidult_hobbies_raw <- readxl::read_excel("data/2025/pct saying kidult hobbies are entirely or mostly for children.xlsx") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(kidult_hobbies_raw)
skimr::skim_without_charts(kidult_hobbies_raw)


## 4. TIDYDATA ----
kidult_hobbies_long <- kidult_hobbies_raw |>
  pivot_longer(
    cols = -age_group,
    names_to = "hobby",
    values_to = "percentage"
  ) |>
  mutate(
    hobby_clean = str_replace_all(hobby, "_", " ") |>
      str_to_title() |>
      str_replace("D D", "(D&D)") |>
      str_replace("Dungeons And Dragons", "Dungeons & Dragons"),
    age_group = factor(age_group, levels = c("18-34", "35-49", "50-64", "65+"))
  )

# plot data
plot_data <- kidult_hobbies_long |>
  filter(age_group %in% c("18-34", "65+")) |>
  pivot_wider(names_from = age_group, values_from = percentage, names_prefix = "age_") |>
  mutate(
    age_gap = `age_65+` - `age_18-34`,
    gap_direction = ifelse(age_gap > 0, "Older view as more childish", "Younger view as more childish"),
    abs_gap = abs(age_gap),
    # Add category for story-telling
    activity_type = case_when(
      hobby_clean %in% c("Comic Books", "Receiving Birthday Presents", "Advent Calendars") ~ "Traditional Childhood",
      hobby_clean %in% c("Star Wars", "Disney Films") ~ "Entertainment Reversal",
      hobby_clean %in% c("Stuffed Animals", "Dressing Up In Costumes") ~ "Physical Play",
      TRUE ~ "Other"
    )
  ) |>
  arrange(desc(age_gap))


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "#2c3e50", "#e17055"
))   


### |-  titles and caption ----
title_text <- str_glue("The Great Generational Divide in 'Childish' Perceptions")

subtitle_text <-str_glue(
  "Difference between 65+ and 18-34 age groups reveals surprising patterns: while older Britons\n",
  "typically view more activities as childish, Star Wars and Disney Films show the opposite trend"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 38,
  source_text = "YouGov"
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
    panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- final plot ----
ggplot(plot_data, aes(x = age_gap, y = reorder(hobby_clean, age_gap))) +

  # Annotate
  annotate("rect",
    xmin = -15, xmax = 0, ymin = -Inf, ymax = Inf,
    fill = colors$palette[2], alpha = 0.08
  ) +
  annotate("text",
    x = -7.5, y = 8.5, label = "Younger see as\nMORE childish",
    size = 3.5, color = colors$palette[2], fontface = "bold",
    family = fonts$text,
    hjust = 0.5, vjust = 0.5, alpha = 0.7
  ) +
  annotate("text",
    x = 15, y = 8.5, label = "Older see as\nMORE childish",
    family = fonts$text,
    size = 3.5, color = colors$palette[1], fontface = "bold",
    hjust = 0.5, vjust = 0.5, alpha = 0.7
  ) +

  # Geoms
  geom_col(aes(fill = gap_direction), alpha = 0.85, width = 0.75) +
  geom_text(
    aes(
      label = ifelse(abs(age_gap) >= 3,
        paste0(ifelse(age_gap > 0, "+", ""), round(age_gap, 0)), ""
      ),
      x = age_gap + ifelse(age_gap > 0, 1.2, -1.2)
    ),
    size = 3.2, fontface = "bold", color = colors$text,
    hjust = ifelse(plot_data$age_gap > 0, 0, 1)
  ) +
  geom_vline(xintercept = 0, color = "gray20", linewidth = 1, alpha = 0.8) +

  # Scales
  scale_fill_manual(
    values = c(
      "Older view as more childish" = colors$palette[1],
      "Younger view as more childish" = colors$palette[2]
    ),
    name = "Generation Gap Direction"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pts"),
    breaks = seq(-15, 30, 5),
    limits = c(-15, 33),
    expand = expansion(mult = c(0.02, 0.02)),
    position = "top"
  ) +
  scale_y_discrete() +

  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Percentage Point Difference (65+ minus 18-34)",
    y = NULL,
  ) +

  # Theme
  theme(
    # Grid
    panel.grid.major.x = element_line(color = "gray80", linewidth = 0.15),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),

    # Legend
    legend.position = "none",
    axis.title.x.top = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10, b = 10)
    ),
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_text(
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
      margin = margin(t = 15)
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-15
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────
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
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────
# > 
