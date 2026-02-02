## Challenge: #MakeoverMonday 2026 week 05
## Data:      Gold Prices
## Author:    Steven Ponce
## Date:      2026-02-02

## Article
# https://www.bullionvault.com/gold-price-chart.do

## Data
# https://data.world/makeovermonday/2025w5-gold-prices

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  patchwork, fredr, janitor, lubridate
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
gold_prices_raw <- read_csv("data/2026/MM2026 WK5 Gold Price.csv") |>
  clean_names()

# Read saved FRED data (instead of fredr call)
real_rates_raw <- read_csv("data/2026/MM2026_WK5_fredr_real_rates.csv") |>
  clean_names() |>
  select(date, value) |>
  rename(real_rate = value)


## 3. EXAMINING THE DATA ----
glimpse(gold_prices_raw)
glimpse(real_rates_raw)


## 4. TIDY DATA ----
### |- Clean gold prices ----
gold_clean <- gold_prices_raw |>
  mutate(
    date = case_when(
      str_detect(date, " ") ~ as.Date(str_extract(date, "^[^ ]+")),
      TRUE ~ as.Date(date)
    ),
    price_kg = close_kg
  ) |>
  filter(date >= as.Date("2003-01-01")) |>
  arrange(date)

### |- Aggregate gold to monthly ----
gold_monthly <- gold_clean |>
  mutate(year_month = floor_date(date, "month")) |>
  group_by(year_month) |>
  summarise(
    price_kg = last(price_kg),
    .groups = "drop"
  ) |>
  rename(date = year_month)

### |- Merge datasets ----
combined_data <- gold_monthly |>
  left_join(real_rates_raw, by = "date") |>
  filter(!is.na(real_rate)) |>
  mutate(
    # Index gold to base 100 (first observation)
    gold_indexed = (price_kg / first(price_kg)) * 100,
    year = year(date),
    # Flag negative rate periods
    negative_rate = real_rate < 0
  )

### |- Identify negative rate periods for shading ----
# Find continuous spans where real_rate < 0
negative_periods <- combined_data |>
  mutate(
    # Create group ID for consecutive negative periods
    neg_group = cumsum(negative_rate != lag(negative_rate, default = FALSE))
  ) |>
  filter(negative_rate) |>
  group_by(neg_group) |>
  summarise(
    xmin = min(date),
    xmax = max(date),
    .groups = "drop"
  ) |>
  # Add small buffer for visual clarity
  mutate(
    xmax = xmax + days(15)
  )

### |- Key annotations ----
annotations <- tibble(
  date = as.Date(c("2008-10-01", "2020-04-01", "2024-06-01")),
  label = c(
    "2008\nFinancial\nCrisis",
    "2020\nPandemic\nResponse",
    "2024\nRally\nResumes"
  )
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    gold          = "#D4A03E",
    gold_dark     = "#8B6914",
    rate_line     = "#2E5A87",
    negative_zone = "#E07A5F",
    text_dark     = "#2D2D2D",
    text_mid      = "#5A5A5A",
    text_light    = "#8A8A8A",
    grid          = "#E8E8E8",
    background    = "#FAFAFA",
    zero_line     = "#4A4A4A"
  )
)

### |-  Main titles ----
title_text <- "Gold Rallies When Real Interest Rates Turn Negative"

subtitle_text <- str_glue(
  "Periods of negative real interest rates consistently coincide with rising U.S. gold prices<br>",
  "<span style='color:{colors$palette$negative_zone}'>■</span> ",
  "Shaded periods show when real interest rates fell below zero"
)

caption_text <- create_social_caption(
  mm_year = 2026, 
  mm_week = 05,
  source_text = "BullionVault (gold prices) | FRED DFII10 (10-Year Real Interest Rate)"
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
      size = rel(1.3), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, face = "italic",
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "plot",
    legend.justification = "right",
    legend.margin = margin(l = 12, b = 5),
    legend.key.size = unit(0.8, "cm"),
    legend.box.margin = margin(b = 10),

    # Axis formatting
    # axis.line.x  = element_line(color = "#252525", linewidth = .1),
    # axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    axis.title.x = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(t = 10), family = fonts$subtitle,
      color = "gray40"
    ),
    axis.title.y = element_text(
      face = "bold", size = rel(0.85),
      margin = margin(r = 10), family = fonts$subtitle,
      color = "gray40"
    ),
    axis.text.x = element_text(
      size = rel(0.85), family = fonts$subtitle,
      color = "gray40"
    ),
    axis.text.y = element_markdown(
      size = rel(0.85), family = fonts$subtitle,
      color = "gray40"
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

### |- Top panel: Gold price (indexed) ----
p_top <- ggplot(combined_data, aes(x = date, y = gold_indexed)) +
  # Geoms
  geom_rect(
    data = negative_periods,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = colors$palette$negative_zone,
    alpha = 0.2
  ) +
  geom_line(color = colors$palette$gold, linewidth = 1) +
  geom_vline(
    data = annotations,
    aes(xintercept = date),
    linetype = "dotted",
    color = colors$palette$text_light,
    linewidth = 0.5
  ) +
  geom_text(
    data = annotations,
    aes(x = date, y = max(combined_data$gold_indexed) * 0.95, label = label),
    size = 2.8,
    color = colors$palette$text_mid,
    lineheight = 0.85,
    hjust = 0.5,
    vjust = 1,
    family = "source"
  ) +
  # Scales
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0.02, 0.1)),
    breaks = seq(0, 800, 200)
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  # Labels
  labs(
    y = "Gold Price\n(indexed, 2003 = 100)"
  ) +
  # Theme
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(
      size = rel(0.6),
      angle = 0,           # Makes it horizontal
      vjust = 1.01,        # Pushes it to the top of the axis
      hjust = 0.5,
      margin = margin(r = -50) # Adjusts distance from the axis
    ),
  )

### |- Bottom panel: Real interest rate ----
p_bottom <- ggplot(combined_data, aes(x = date, y = real_rate)) +
  # Geoms
  geom_rect(
    data = negative_periods,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = colors$palette$negative_zone,
    alpha = 0.2
  ) +
  geom_hline(
    yintercept = 0, 
    color = colors$palette$zero_line, 
    linewidth = 0.6
  ) +
  geom_line(color = colors$palette$rate_line, linewidth = 0.8) +
  geom_vline(
    data = annotations,
    aes(xintercept = date),
    linetype = "dotted",
    color = colors$palette$text_light,
    linewidth = 0.5
  ) +
  # Scales
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1),
    breaks = seq(-2, 3, 1),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Labels
  labs(
    x = NULL,
    y = "Real Interest\nRate (%)"
  ) +
  # Theme
  theme(
    axis.title.y = element_text(
      size = rel(0.6),
      angle = 0,          
      vjust = 1.12,        
      hjust = 0.5,
      margin = margin(r = -50)
    ),
  )

### |- Combine panels ----
# combined_plot <- 
p_top / p_bottom +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text, 
    theme = theme(
      plot.title = element_text(
        size = rel(1.45
                   ),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.8),
        family = 'sans',
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.5,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.55),
        family = fonts$subtitle,
        color = colors$caption,
        hjust = 0,
        lineheight = 1.4,
      margin = margin(t = 20, b = 5)
      )
    )
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

# ─ Session info ────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-02-02
# rstudio  2026.01.0+392 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
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
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
# forcats     * 1.0.0    2023-01-29 [1] RSPM (R 4.4.0)
# P fredr       * 2.1.0    2021-01-29 [?] RSPM (R 4.4.0)
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
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
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# P rprojroot     2.1.1    2025-08-26 [?] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg          2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# showtext    * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb  * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
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
# ───────────────────────────────────────────────────────────────────────────────────────────────
# > 