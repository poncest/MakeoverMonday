
## Challenge: #MakeoverMondnay 2025 week 21
## Data:      Metacritic Best Games of All Time
## Author:    Steven Ponce
## Date:      2025-05-19

## Original Chart
# Metacritic Best Games of All Time
# https://data.world/makeovermonday/2025w21-metacritic-best-games-of-all-time

## Article
# Metacritic Best Games of All Time
# https://www.metacritic.com/browse/game/

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
    ggrepel,        # Automatically Position Non-Overlapping Text Labels with ggplot2
    treemapify,     # Draw Treemaps in 'ggplot2'
    marquee,        # Markdown Parser and Renderer for R Graphic
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
metacritic_top_games_raw <- read_csv('data/2025/metacritic_top_games.csv') |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(metacritic_top_games_raw )
skimr::skim(metacritic_top_games_raw )


## 4. TIDYDATA ----

### |-  tidy data ----
# Split the platforms into separate rows
games_platforms <- metacritic_top_games_raw |>
  separate_rows(platforms, sep = ", ") |>
  mutate(
    platform_type = case_when(
      platforms %in% c("PC") ~ "PC",
      platforms %in% c("iOS (iPhone/iPad)") ~ "Mobile",
      TRUE ~ "Console"
    ),
    release_year = year(release_date),
    decade = paste0(floor(release_year / 10) * 10, "s"),
    publisher_group = ifelse(publisher %in% c("Nintendo", "Rockstar Games"), publisher, "Other")
  )

# Comprehensive tidy dataset
games_tidy <- metacritic_top_games_raw |>
  mutate(
    release_year = year(release_date),
    decade = paste0(floor(release_year / 10) * 10, "s"),
    genre_primary = case_when(
      str_detect(genre, "Action") ~ "Action",
      str_detect(genre, "Shooter") ~ "Shooter",
      str_detect(genre, "Platform") ~ "Platformer",
      str_detect(genre, "Sports") ~ "Sports",
      str_detect(genre, "Fighting") ~ "Fighting",
      TRUE ~ genre
    ),
    # Short title for visualization
    short_title = case_when(
      str_length(title) > 20 ~ paste0(str_sub(title, 1, 17), "..."),
      TRUE ~ title
    )
  )

# P1. treemap data----
treemap_data <- games_platforms |>
  count(platforms) |>
  mutate(
    platform_group = case_when(
      platforms == "PC" ~ "PC",
      TRUE ~ "Console"
    ),
    platform_label = paste0(platforms, "\n(", n, ")")
  )

# P2. dot plot data -----
dotplot_data <- games_tidy |>
  mutate(
    platform_group = case_when(
      str_detect(platforms, "PC") ~ "PC",
      TRUE ~ "Console"
    ),
    is_top_game = metascore >= 97,
    should_label = case_when(
      # Only label the very top PC games
      (platform_group == "PC" & metascore >= 97) ~ TRUE,
      # Only label the very top Console games
      (platform_group == "Console" & metascore >= 98) ~ TRUE,
      TRUE ~ FALSE
    )
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "PC" = "#1F77B4",     
  "Console" = "#A9A9A9"
))
  
### |-  titles and caption ----
title_text <- str_glue("The Enduring Legacy of PC Gaming Excellence")
subtitle_text <- "While consoles produce occasional standout hits, PC gaming demonstrates sustained quality and unmatched volume\n
{#1F77B4 **_PC games_**} demonstrate sustained excellence, while {#A9A9A **_console games_**} include some standout titles"

# Create caption
caption_text <- create_social_caption(
    mm_year = 2025,
    mm_week = 21,
    source_text = "Metacritic"
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

# P1. Treemap ----
p1 <- ggplot(treemap_data, aes(area = n, fill = platform_group, label = platform_label)) +
  # Geoms
  geom_treemap(
    color = "white",
    size = 1.5
  ) +
  geom_treemap_text(
    aes(label = platforms),
    colour = "white",
    place = "centre",
    size = 11,
    fontface = "bold",
    padding.y = grid::unit(4, "mm")
  ) +
  geom_treemap_text(
    aes(label = paste0("n = ", n)),
    colour = "white",
    place = "bottom",
    size = 8,
    padding.y = grid::unit(4, "mm")
  ) +
  # Scales
  scale_fill_manual(values = colors$palette) +
  coord_equal() +
  # Labs
  labs(
    title = "PC: The Single Most Prolific Platform for Top-Rated Games",
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 15)
    )
  )

# P2. dot plot -----
p2 <- ggplot(dotplot_data, aes(x = release_date, y = metascore)) +
  # Geoms
  geom_hline(yintercept = c(90, 95), linetype = "dashed", color = "gray80", alpha = 0.7) +
  # Annotate
  annotate("rect",
    xmin = as.Date("1997-01-01"), xmax = as.Date("2000-01-01"),
    ymin = 89.5, ymax = 100, fill = "gray90", alpha = 0.5
  ) +
  annotate("rect",
    xmin = as.Date("2000-01-01"), xmax = as.Date("2010-01-01"),
    ymin = 89.5, ymax = 100, fill = "gray85", alpha = 0.5
  ) +
  annotate("rect",
    xmin = as.Date("2010-01-01"), xmax = as.Date("2021-01-01"),
    ymin = 89.5, ymax = 100, fill = "gray90", alpha = 0.5
  ) +
  annotate("text",
    x = as.Date("1998-07-01"), y = 90.5, label = "1990s",
    color = "gray50", size = 3, fontface = "bold", alpha = 0.7
  ) +
  annotate("text",
    x = as.Date("2005-01-01"), y = 90.5, label = "2000s",
    color = "gray50", size = 3, fontface = "bold", alpha = 0.7
  ) +
  annotate("text",
    x = as.Date("2015-01-01"), y = 90.5, label = "2010s",
    color = "gray50", size = 3, fontface = "bold", alpha = 0.7
  ) +

  # Geoms
  geom_point(
    aes(
      color = platform_group,
      alpha = if_else(platform_group == "PC", 0.9, 0.6)
    ),
    size = 3,
    stroke = 0.2
  ) +
  geom_text_repel(
    data = dotplot_data |> filter(should_label),
    aes(label = short_title, color = platform_group),
    size = 2.8,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.3,
    force = 2,
    segment.color = "gray60",
    segment.size = 0.2,
    segment.alpha = 0.8,
    max.overlaps = 15,
    seed = 42
  ) +
  # Scales
  scale_color_manual(values = colors$palette) +
  scale_alpha_identity() +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(90, 100, by = 2.5),
    limits = c(89.5, 100),
    labels = function(x) paste0(x)
  ) +
  # Labs
  labs(
    title = "PC Games Show Consistent Quality Across Three Decades",
    x = "Release Year",
    y = "Metascore"
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 15)
    )
  )

# Combined Plot ----
combined_plot <- (p1 | plot_spacer() | p2) +
  plot_layout(
    widths = c(1, 0.005, 1),
    guides = "collect"
  ) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(2.4),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_marquee(
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
    )
  )

combined_plot


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-05-19
# rstudio  2024.12.1+563 Kousa Dogwood (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────
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
# P ggfittext     0.10.2   2024-02-01 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
# P ggrepel     * 0.9.6    2024-09-07 [?] RSPM (R 4.4.0)
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
# P marquee     * 1.0.0    2025-01-20 [?] RSPM (R 4.4.0)
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
# P treemapify  * 2.5.6    2023-09-30 [?] CRAN (R 4.4.0)
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
# ──────────────────────────────────
# > 
