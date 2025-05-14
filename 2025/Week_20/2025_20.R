
## Challenge: #MakeoverMondnay 2025 week 20
## Data:      The Religious Composition of the World’s Migrants
## Author:    Steven Ponce
## Date:      2025-05-14

## Original Chart
# The Religious Composition of the World’s Migrants
# https://data.world/makeovermonday/2025w20-the-religious-composition-of-the-worlds-migrants

## Article
# Pew Research Center "The Religious Composition of the World’s Migrants"
# https://www.pewresearch.org/religion/2024/08/19/the-religious-composition-of-the-worlds-migrants/

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,      # Easily Install and Load the 'Tidyverse'
    ggtext,         # Improved Text Rendering Support for 'ggplot2'
    showtext,       # Using Fonts More Easily in R Graphs
    scales,         # Scale Functions for Visualization
    glue,           # Interpreted String Literals
    here,           # A Simpler Way to Find Your Files
    lubridate,      # Make Dealing with Dates a Little Easier
    paletteer       # Comprehensive Collection of Color Palettes
)

### |- figure size ----
camcorder::gg_record(
    dir    = here::here("temp_plots"),
    device = "png",
    width  =  10,
    height =  8,
    units  = "in",
    dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
migrants_raw <- readxl::read_excel('data/2025/Incoming and Outgoing Migrant Counts.xlsx',
                            sheet = 1) |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(migrants_raw)
skimr::skim(migrants_raw)

## 4. TIDYDATA ----

### |-  tidy data ----
# Extract the global data for 2020 (most recent)
migrants_global <- migrants_raw |>
  filter(direction == "Incoming", year == "2020", country == "Global Total") |>
  select(religion, migrant_percent = percent)

# Derive general population data from on the chart
general_pop <- tibble(
  religion = c("Christian", "Muslim", "Jewish", "Buddhist", "Unaffiliated", "Hindu", "Other religions"),
  general_percent = c(30, 25, 0.2, 4, 23, 15, 2.8)
)

# Join the datasets
comparison_data <- migrants_global |>
  filter(religion != "All") |>
  mutate(religion = case_when(
    religion == "Jew" ~ "Jewish",
    religion == "Religiously unaffiliated" ~ "Unaffiliated",
    TRUE ~ religion
  )) |>
  left_join(general_pop, by = "religion") |>
  filter(!is.na(general_percent)) |>
  mutate(
    difference = migrant_percent - general_percent,
    migrant_percent_display = round(migrant_percent, 1),
    general_percent_display = round(general_percent, 1)
  ) |>
  mutate(
    religion = fct_reorder(religion, difference),
    abs_diff = abs(difference),
    direction = ifelse(difference >= 0, "overrepresented", "underrepresented"),
    percent_label = paste0(general_percent_display, "% → ", migrant_percent_display, "%")
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "overrepresented" =  "#6761A8",
  "underrepresented" = "#CC3363"
))
  
### |-  titles and caption ----
title_text <- str_glue("Religious Representation Gap: Migrants vs. General Population")
subtitle_text <- str_wrap("Christians are significantly overrepresented among global migrants,\nwhile Hindus and the religiously unaffiliated are underrepresented",
                          width = 100)

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 20,
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


### |-  Initial Plot  ----
p <- comparison_data |>
  ggplot(aes(x = religion, y = difference, fill = direction)) +
  # Geoms
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "gray40", size = 0.5) +
  geom_text( # primary labels: difference in percentage points
    aes(
      label = paste0(ifelse(difference >= 0, "+", ""), round(difference, 1), " pts"),
      y = ifelse(difference >= 0,
        difference + 0.8,
        difference - 0.8
      )
    ),
    hjust = ifelse(comparison_data$difference >= 0, 0, 1),
    size = 4,
    fontface = "bold"
  ) +
  geom_text( # secondary labels: comparison of percentages
    aes(
      label = percent_label,
      y = ifelse(difference >= 0,
        difference + 5.5,
        difference - 5.5
      )
    ),
    hjust = ifelse(comparison_data$difference >= 0, 0, 1),
    size = 3.5,
    color = "gray30"
  ) +
  # Scale
  scale_fill_manual(
    values = colors$palette
  ) +
  scale_y_continuous(
    breaks = seq(-25, 30, 10),
    limits = c(-25, 30),
    expand = c(0, 0)
  ) +
  coord_flip(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = NULL,
    y = "Percentage Point Difference",
    fill = NULL
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.8),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_text(
      size = rel(0.9),
      family = fonts$subtitle,
      color = colors$subtitle,
      lineheight = 1.2,
      margin = margin(t = 5, b = 30)
    ),
    plot.caption = element_markdown(
      size = rel(0.6),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0.5,
      margin = margin(t = 10)
    )
  )

### |-  Annotated Plot  ----
p +
  annotate(
    "label",
    x = 5.2,
    y = -24.5,
    label = paste(
      "Christians make up 30% of the general population but",
      "46.6% of migrants, a difference of +16.6 percentage points.",
      "This pattern may reflect historical migration flows and",
      "religious persecution.",
      sep = "\n"
    ),
    hjust = 0,
    vjust = 0,
    fill = NA,
    color = "gray20",
    size = 3.5,
    fontface = "italic",
    alpha = 0.9,
    label.size = NA
  ) +
  annotate("segment",
    x = 3.5, xend = 3.5,
    y = 5, yend = 8,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = colors$palette["overrepresented"],
    size = 1
  ) +
  annotate("text",
    x = 3.5, y = 15,
    label = "Overrepresented",
    color = colors$palette["overrepresented"],
    fontface = "bold",
    size = 4,
    hjust = 0.5
  ) +
  annotate("segment",
    x = 3.5, xend = 3.5,
    y = -5, yend = -8,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = colors$palette["underrepresented"],
    size = 1
  ) +
  annotate("text",
    x = 3.5, y = -15,
    label = "Underrepresented",
    color = colors$palette["underrepresented"],
    fontface = "bold",
    size = 4,
    hjust = 0.5
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
# tz       America/New_York
# date     2025-05-14
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────
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
# P paletteer   * 1.6.0    2024-01-21 [?] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# readxl        1.4.3    2023-07-06 [1] RSPM (R 4.4.0)
# rematch2      2.1.2    2020-05-01 [1] RSPM (R 4.4.0)
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────
# > 