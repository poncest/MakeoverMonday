## Challenge: #MakeoverMonday 2025 week 46
## Data:      School’s out and the TV’s on: What kids in the U.S. watched in June
## Author:    Steven Ponce
## Date:      2025-11-25

## Article
# https://www.nielsen.com/insights/2025/what-us-kids-watched-tv-streaming-summer-june/

## Data
# https://data.world/makeovermonday/schools-out-and-the-tvs-on

## Citation
# Nielsen. (2025). School's out and the TV's on: What kids in the U.S. watched in June.
# Nielsen Insights. Retrieved from https://www.nielsen.com/insights/2025/what-us-kids-watched-tv-streaming-summer-june/

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  glue           # Interpreted String Literals
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
top_10 <- readxl::read_excel("data/2025/Top 10 Streaming (Kids 6 17).xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(top_10)


## 4. TIDY DATA ----
top_10_clean <- top_10 |>
  mutate(
    # Standardize program names
    program_clean = case_when(
      str_detect(program, "Spongebob") ~ "SpongeBob SquarePants",
      TRUE ~ program
    )
  )

# SOURCES:
# - Content Ratings: IMDb (www.imdb.com) - displays TV Parental Guidelines ratings
# - Show Type & Genre: IMDb and streaming service platforms (Netflix, Disney+, etc.)
# - Additional verification: Common Sense Media (www.commonsensemedia.org)
#
# NOTE: These ratings reflect US TV Parental Guidelines:
#   TV-Y   = All Children
#   TV-Y7  = Directed to Older Children (7+)
#   TV-G   = General Audience
#   TV-PG  = Parental Guidance Suggested
#   TV-14  = Parents Strongly Cautioned (14+)
#   TV-MA  = Mature Audience Only (17+)

show_info <- tribble(
  ~program_clean,                ~type,         ~rating,   ~genre,          ~imdb_id,
  "Ginny & Georgia",             "Live-action", "TV-14",   "Drama",         "tt10624432",
  "Bluey",                       "Animation",   "TV-Y",    "Comedy",        "tt7678620",
  "Squid Game",                  "Live-action", "TV-MA",   "Thriller",      "tt10919420",
  "SpongeBob SquarePants",       "Animation",   "TV-Y7",   "Comedy",        "tt0206512",
  "Phineas and Ferb",            "Animation",   "TV-G",    "Comedy",        "tt0852863",
  "The Amazing World of Gumball","Animation",   "TV-PG",   "Comedy",        "tt1942683",
  "Stranger Things",             "Live-action", "TV-14",   "Sci-Fi",        "tt4574334",
  "Young Sheldon",               "Live-action", "TV-PG",   "Comedy",        "tt6226232",
  "Love Island USA",             "Reality",     "TV-14",   "Reality",       "tt8230780",
  "Alvin! and the Chipmunks",    "Animation",   "TV-Y7",   "Comedy",        "tt0084119"
)

## |-  join and create analysis variables ----
top_10_analysis <- top_10_clean |>
  left_join(show_info, by = "program_clean") |>
  mutate(
    mins_millions = round(mins_viewed / 1e6, 0),
    age_appropriate = case_when(
      rating %in% c("TV-Y", "TV-Y7", "TV-G") ~ "Kids",
      rating == "TV-PG" ~ "Family",
      rating == "TV-14" ~ "Teen",
      rating == "TV-MA" ~ "Mature",
      TRUE ~ "Unknown"
    ),
    age_appropriate = factor(age_appropriate,
                             levels = c("Kids", "Family", "Teen", "Mature")
    ),
    program_with_provider = glue("{program_clean}<br><span style='font-size:8pt;color:gray50'>({svod_provider})</span>"),
    program_ordered = fct_reorder(program_with_provider, -rank)
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    netflix       = "#B20710", 
    neutral_dark  = "#9E9E9E"           # "#BDBDBD"
  )
)

### |-  Main titles ----
title_text <- "Kids' June 2025 Streaming Top 10"
subtitle_text <- str_glue(
  "Netflix leads June with 4 of the top 10 programs, totaling 3.3B minutes viewed<br>",
  "Nielsen measures household accounts with viewers 6–17; programming reflects actual viewing, not age-appropriateness."
)

### |-  Data source caption ----
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 46,
  source_text = str_glue(
    "Nielsen National TV Panel<br>",
    "**Note:** Kids 6-17, June 2025 (05/26/25 - 06/29/25). Metadata from IMDb."
  )
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
    # # Text styling
    plot.title = element_text(
      size = rel(1.5), family = fonts$title, face = "bold",
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
    # legend.title = element_text(face = "bold"),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
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
    axis.text.y = element_markdown(
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

## |-  prepare data for plotting ----
plot_data <- top_10_analysis |>
  mutate(
    # Netflix highlighting
    is_netflix = svod_provider == "Netflix",
    bar_color = ifelse(is_netflix, colors$palette$netflix, colors$palette$neutral_dark),
    text_color = ifelse(is_netflix, "white", "gray30"),
  ) |>
  mutate(
    mins_billions = mins_millions / 1000,
    mins_label_smart = ifelse(
      mins_billions >= 1,
      paste0(round(mins_billions, 2), "B"),
      paste0(mins_millions, "M")
    )
  )

### |-  main plot ----
plot_data |>
  ggplot(aes(x = mins_millions, y = program_ordered)) +
  # Geoms
  geom_col(aes(fill = bar_color), width = 0.65) +
  geom_text(
    aes(x = 25, label = paste0(genre, " | ", type), color = text_color),
    hjust = 0,
    size = 2.5
  ) +
  geom_label(
    aes(x = mins_millions - 40, label = rating),
    hjust = 1,
    size = 2.5,
    fontface = "bold",
    fill = "white",
    label.size = 0,
    label.padding = unit(0.15, "lines")
  ) +
  geom_text(
    aes(label = mins_label_smart),
    hjust = -0.1,
    size = 3.5,
    fontface = "bold"
  ) +
  # Annotate
  annotate(
    "text", x = 850, y = 3,
    label = "60% of the Top 10 are Kids/Family titles,\nwhile several Teen/Mature shows still rank highly\namong households with viewers 6–17.",
    hjust = 0, size = 3.2, lineheight = 1.1,
    family = fonts$subtitle, color = "gray30"
  ) +
  # Scales
  scale_x_continuous(
    position = "top",
    expand = expansion(mult = c(0.02, 0.1)),
    labels = label_comma(suffix = "M")
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    x = "Total Minutes Viewed (Millions)",
    y = NULL,
    caption = caption_text
  ) +
  # Theme
  theme(
    plot.title = element_text(
      size = rel(1.85),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.1,
      margin = margin(t = 5, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.9),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9),
      lineheight = 1.5,
      margin = margin(t = 5, b = 25)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$caption,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.2,
      margin = margin(t = 10)
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank()
  )


# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts,
# and formatting across all my #MakeoverMonday projects. The core analysis logic
# (data tidying and visualization) uses only standard tidyverse packages.
#
# -----------------------------------------------------------------------------
# FUNCTIONS USED IN THIS SCRIPT:
# -----------------------------------------------------------------------------
#
# 📂 R/utils/fonts.R
#    • setup_fonts()       - Initialize Google Fonts with showtext
#    • get_font_families() - Return standardized font family names
#
# 📂 R/utils/social_icons.R
#    • create_social_caption() - Generate formatted caption with social handles
#                                and #MakeoverMonday attribution
#
# 📂 R/themes/base_theme.R
#    • create_base_theme()   - Create consistent base ggplot2 theme
#    • extend_weekly_theme() - Add weekly-specific theme customizations
#    • get_theme_colors()    - Get color palettes for highlight/text
#
# -----------------------------------------------------------------------------
# WHY CUSTOM FUNCTIONS?
# -----------------------------------------------------------------------------
# These utilities eliminate repetitive code and ensure visual consistency
# across X+ weekly visualizations. Instead of copy-pasting 30+ lines of
# theme() code each week, I use create_base_theme() and extend as needed.
#
# -----------------------------------------------------------------------------
# VIEW SOURCE CODE:
# -----------------------------------------------------------------------------
# All helper functions are open source on GitHub:
# 🔗 https://github.com/poncest/MakeoverMonday/tree/master/R
#
# Main files:
#   • R/utils/fonts.R         - Font setup and management
#   • R/utils/social_icons.R  - Caption generation with icons
#   • R/themes/base_theme.R   - Reusable ggplot2 themes
#
# -----------------------------------------------------------------------------
# REPRODUCIBILITY:
# -----------------------------------------------------------------------------
# To run this script:
#
# Option 1 - Use the helper functions (recommended):
#   1. Clone the repo: https://github.com/poncest/MakeoverMonday/tree/master
#   2. Make sure the R/ directory structure is maintained
#   3. Run the script as-is
#
# Option 2 - Replace with standard code:
#   1. Replace setup_fonts() with your own font setup
#   2. Replace get_theme_colors() with manual color definitions
#   3. Replace create_base_theme() with theme_minimal() + theme()
#   4. Replace create_social_caption() with manual caption text
#
## ============================================================================ ##


# 7. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-11-25
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
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
# skimr       * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
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
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────
# > 