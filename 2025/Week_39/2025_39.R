## Challenge: #MakeoverMondnay 2025 week 39
## Data:      Family spending in the UK
## Author:    Steven Ponce
## Date:      2025-09-24

## Article
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/expenditure/bulletins/familyspendingintheuk/april2023tomarch2024#data-sources-and-quality

## Data
# https://data.world/makeovermonday/2025w39-family-spending-in-the-uk

## Original Chart
# https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/expenditure/bulletins/familyspendingintheuk/april2023tomarch2024#data-sources-and-quality

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork,  # Using Fonts More Easily in R Graphs
  ggridges    # Ridgeline Plots in 'ggplot2'
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 10,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
households_spending_raw <- read_csv("data/2025/Figure_5__Poorer_households_spent_proportionally_more_on_housing,_fuel_and_power_than_richer_households_in_FYE_2024.csv") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(households_spending_raw)
skimr::skim_without_charts(households_spending_raw)


## 4. TIDY DATA ----
# Create lookup tables
quintile_labels <- c(
  "bottom_fifth" = "Bottom fifth",
  "x2nd" = "2nd quintile",
  "x3rd" = "3rd quintile",
  "x4th" = "4th quintile",
  "top_fifth" = "Top fifth",
  "all_households" = "All households"
)

# Use the category names from the data
category_lookup <- c(
  "Food and non-alcoholic drinks" = "Food & non-alcoholic drinks",
  "Alcoholic drink tobacco and narcotics" = "Alcohol, tobacco & narcotics", # Note: no comma in original
  "Clothing and footwear" = "Clothing & footwear",
  "Housing (net) fuel and power" = "Housing, fuel & power", # Note: no comma in original
  "Household goods and services" = "Household goods & services",
  "Transport" = "Transport",
  "Communication" = "Communication",
  "Recreation and culture" = "Recreation & culture",
  "Restaurants and hotels" = "Restaurants & hotels",
  "Miscellaneous goods and services" = "Miscellaneous goods & services",
  "Other expenditure items" = "Other expenditure"
)

# Data prep
households_spending_clean <- households_spending_raw |>
  pivot_longer(
    cols = -percent_of_total_weekly_expendtiure,
    names_to = "income_quintile",
    values_to = "percentage"
  ) |>
  rename(category = percent_of_total_weekly_expendtiure) |>
  mutate(
    # Clean quintile names
    income_quintile = quintile_labels[income_quintile],
    income_quintile = factor(income_quintile, levels = quintile_labels),

    # Clean category names using direct lookup
    category_clean = category_lookup[category]
  ) |>
  filter(income_quintile != "All households") |>
  group_by(category_clean) |>
  mutate(gap = max(percentage) - min(percentage)) |>
  ungroup() |>
  mutate(category_clean = fct_reorder(category_clean, gap, .desc = FALSE))

# Dot plot data
dot_data <- households_spending_clean |>
  group_by(category_clean) |>
  summarise(
    min_pct = min(percentage),
    max_pct = max(percentage),
    gap = first(gap),
    .groups = "drop"
  )

# Ridges plot data
ridges_data <- households_spending_clean


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  primary = "#2C3E50",      
  secondary = "#E67E22",    
  accent = "#95A5A6",       
  light_gray = "#BDC3C7",   
  dark_gray = "#7F8C8D"     
))   

### |-  titles and caption ----
title_text <- str_glue("UK Household Spending Inequality by Income Level in FYE 2024")

subtitle_text <-str_glue(
  "Housing costs create the largest spending gap between rich and poor households"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 39,
  source_text = "UK Office for National Statistics"
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
    # Text styling
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

### |- P1: Range Dot Plot ----
p1 <- dot_data |>
  ggplot(aes(y = category_clean)) +
  # Geoms
  geom_segment(aes(x = min_pct, xend = max_pct, yend = category_clean),
    linewidth = 1, color = colors$palette$light_gray, alpha = 0.6
  ) +
  geom_point(aes(x = min_pct), size = 4, color = colors$palette$secondary) +
  geom_point(aes(x = max_pct), size = 4, color = colors$palette$primary) +
  geom_text(
    aes(
      x = (min_pct + max_pct) / 2,
      label = glue("{round(gap,1)} pp")
    ),
    vjust = -1.8, size = 3.3,
    color = colors$palette$primary
  ) +

  # IAnnotate
  annotate("point", x = 22, y = 1.5, size = 4, color = colors$palette$secondary) +
  annotate("text",
    x = 23, y = 1.5, label = "Poorest households",
    color = colors$palette$secondary, size = 3.2, fontface = "bold", hjust = 0
  ) +
  annotate("point", x = 22, y = 1, size = 4, color = colors$palette$primary) +
  annotate("text",
    x = 23, y = 1, label = "Richest households",
    color = colors$palette$primary, size = 3.2, fontface = "bold", hjust = 0
  ) +

  # Scales
  scale_x_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0.02, 0.1)),
    limits = c(0, 40)
  ) +

  # Labs
  labs(
    title = "Spending Inequality Across Categories",
    subtitle = str_wrap("Range between highest and lowest income quintiles (percentage points gap shown)",
      width = 65
    ),
    x = "Percentage of total weekly expenditure",
    y = NULL,
  ) +

  # Theme
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F0F0F0", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )

### |- P2: Ridgeline Plot ----
p2 <- ridges_data |>
  ggplot(aes(x = percentage, y = category_clean)) +
  # Geoms
  geom_density_ridges(
    alpha = 0.8,
    scale = 1.2,
    rel_min_height = 0.01,
    color = "white",
    linewidth = 0.8,
    fill = colors$palette$accent
  ) +

  # Scales
  scale_x_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +

  # Labs
  labs(
    title = "Distribution Across Quintiles",
    subtitle = str_wrap("Shape shows variation pattern across income groups",
      width = 40
    ),
    x = "Percentage of total weekly expenditure",
    y = NULL,
  ) +

  # Theme
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#F0F0F0", linewidth = 0.5),
    panel.grid.minor = element_blank()
  )

### |- Combined Plots ----
combined_plots <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(1.4, 1.1))

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

# ─ Session info ───────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-23
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────
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
# P ggplot2     * 3.5.2    2025-04-09 [?] RSPM (R 4.4.0)
# P ggridges    * 0.5.6    2024-01-23 [?] RSPM (R 4.4.0)
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────
# > 
