## Challenge: #MakeoverMonday 2026 week 06
## Data:      Global Big Mac Index
## Author:    Steven Ponce
## Date:      2026-02-10

## Article
# https://www.economist.com/interactive/big-mac-index

## Data
# https://data.world/makeovermonday/2026w6-global-big-mac-index

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue,
  janitor, lubridate
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
big_mac_index_raw <- read_csv("data/2026/big-mac-full-index.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(big_mac_index_raw)


## 4. TIDY DATA ----

### |-  prepare data ----
time_series_data <- big_mac_index_raw |>
  mutate(date = mdy(date))

latest_date <- max(time_series_data$date, na.rm = TRUE)

current_data <- time_series_data |>
  filter(date == latest_date) |>
  arrange(desc(usd_raw))

### |-  calculate 25-year change ----
first_obs <- time_series_data |>
  filter(year(date) == 2000) |>
  group_by(name) |>
  slice_min(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(name, usd_raw_start = usd_raw)

# Compute movers from 2000
currency_movers <- current_data |>
  inner_join(first_obs, by = "name") |>
  mutate(
    total_change = (usd_raw - usd_raw_start) * 100,
    direction = if_else(total_change > 0, "Strengthened", "Weakened")
  ) |>
  slice_max(order_by = abs(total_change), n = 15, with_ties = FALSE) |>
  arrange(total_change) |>
  mutate(name = fct_inorder(name))


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    strengthened  = "#2C5F8D",    
    weakened      = "#C97A4A",    
    neutral_dark  = "#5378A6",
    neutral_light = "gray90"
  )
)

### |-  Main titles ----
title_text <- "25 Years of Currency Shifts"

subtitle_text <- str_glue(
  "Change in Big Mac purchasing power vs the US dollar, 2000–2025. Most currencies saw Big Mac prices rise relative to the dollar"
)

caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 06,
  source_text = "The Economist Big Mac Index<br>**Note:** Values show change in Big Mac price competitiveness over 25 years. Based on raw prices (not GDP-adjusted)."
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

### |- final plot ----
ggplot(currency_movers, aes(x = total_change, y = name)) +
  # Annotate
  annotate(
    "rect",
    xmin = -0.75, xmax = 0.75,
    ymin = -Inf, ymax = Inf,
    fill = "gray95", alpha = 1
  ) +
  # Geoms
  geom_vline(xintercept = 0, linewidth = 0.8, color = "gray55") +
  geom_col(aes(fill = direction), width = 0.72, show.legend = FALSE) +
  geom_text(
    aes(
      label = sprintf("%+.0f", total_change),
      hjust = if_else(total_change >= 0, -0.10, 1.10)
    ),
    size = 3,
    fontface = "bold"
  ) +
  # Annotate
  annotate(
    "segment",
    x = -63, xend = -68,
    y = 14, yend = 14,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = colors$palette$weakened,
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = -68, y = 14.5,
    label = "More expensive\nin USD",
    hjust = 0,
    vjust = 0,
    size = 2.8,
    color = colors$palette$weakened,
    fontface = "italic",
    lineheight = 0.9
  ) +
  annotate(
    "segment",
    x = 40, xend = 45,
    y = 14, yend = 14,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = colors$palette$strengthened,
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = 40, y = 14.5,
    label = "Cheaper\nin USD",
    hjust = 0,
    vjust = 0,
    size = 2.8,
    color = colors$palette$strengthened,
    fontface = "italic",
    lineheight = 0.9
  ) +
  # Scales
  scale_fill_manual(values = c(
    "Strengthened" = colors$palette$strengthened,
    "Weakened" = colors$palette$weakened
  )) +
  scale_x_continuous(
    labels = label_number(style_positive = "plus"),
    expand = expansion(mult = c(0.10, 0.12)),
    breaks = seq(-60, 30, by = 20)
  ) +
  coord_cartesian(clip = "off") +
  # Labs
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Change in valuation since 2000 (pp, USD-based Big Mac Index)",
    y = NULL
  ) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    plot.title = element_markdown(
      size = rel(1.4),
      family = fonts$title,
      face = "bold",
      color = colors$title,
      lineheight = 1.15,
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8),
      family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.88),
      lineheight = 1.5,
      margin = margin(t = 5, b = 25)
    ),
    plot.caption = element_markdown(
      size = rel(0.5),
      family = fonts$subtitle,
      color = colors$caption,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 20, b = 5)
    )
  )

# save
ggsave(
  filename = "2026/Week_06/2026_06.png", 
  plot = last_plot(), 
  device = "png", 
  width = 10, 
  height = 8, 
  units = "in", 
  dpi = 320)


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

# ─ Session info ──────────────────────────────────────────────────────────────────────────
#  setting  value
#  version  R version 4.4.1 (2024-06-14 ucrt)
#  os       Windows 11 x64 (build 26100)
#  system   x86_64, mingw32
#  ui       RStudio
#  language (EN)
#  collate  English_United States.utf8
#  ctype    English_United States.utf8
#  tz       America/New_York
#  date     2026-02-10
#  rstudio  2026.01.0+392 Apple Blossom (desktop)
#  pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────────────
#  ! package      * version  date (UTC) lib source
#  V base         * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
#    bit            4.5.0.1  2024-12-03 [1] RSPM (R 4.4.0)
#    bit64          4.6.0-1  2025-01-16 [1] RSPM (R 4.4.0)
#    camcorder      0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
#    cli            3.6.3    2024-06-21 [1] RSPM (R 4.4.0)
#    commonmark     1.9.2    2024-10-04 [1] RSPM (R 4.4.0)
#  P compiler       4.4.0    2024-04-24 [?] local
#    crayon         1.5.3    2024-06-20 [1] RSPM (R 4.4.0)
#    curl           6.2.0    2025-01-23 [1] RSPM (R 4.4.0)
#  P datasets     * 4.4.0    2024-04-24 [?] local
#    dplyr        * 1.2.0    2026-02-03 [1] RSPM
#    farver         2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
#    forcats      * 1.0.1    2025-09-25 [1] RSPM
#    generics       0.1.3    2022-07-05 [1] RSPM (R 4.4.0)
#    ggplot2      * 4.0.2    2026-02-03 [1] RSPM
#    ggtext       * 0.1.2    2022-09-16 [1] RSPM (R 4.4.0)
#    gifski         1.32.0-1 2024-10-13 [1] RSPM (R 4.4.1)
#    glue         * 1.8.0    2024-09-30 [1] RSPM (R 4.4.0)
#  P graphics     * 4.4.0    2024-04-24 [?] local
#  P grDevices    * 4.4.0    2024-04-24 [?] local
#  P grid           4.4.0    2024-04-24 [?] local
#    gridtext       0.1.5    2022-09-16 [1] RSPM (R 4.4.0)
#    gtable         0.3.6    2024-10-25 [1] RSPM (R 4.4.0)
#    here         * 1.0.1    2020-12-13 [1] RSPM (R 4.4.0)
#    hms            1.1.4    2025-10-17 [1] RSPM
#    janitor      * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
#    jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.4.3)
#    lifecycle      1.0.5    2026-01-08 [1] RSPM
#    lubridate    * 1.9.5    2026-02-04 [1] RSPM
#    magick         2.8.5    2024-09-20 [1] RSPM (R 4.4.0)
#    magrittr       2.0.4    2025-09-12 [1] RSPM
#    markdown       1.13     2024-06-04 [1] RSPM (R 4.4.0)
#  P methods      * 4.4.0    2024-04-24 [?] local
#  P pacman       * 0.5.1    2019-03-11 [?] RSPM (R 4.4.0)
#  P parallel       4.4.0    2024-04-24 [?] local
#    pillar         1.11.1   2025-09-17 [1] RSPM
#    pkgconfig      2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
#    purrr        * 1.2.1    2026-01-09 [1] RSPM
#    R6             2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
#    ragg           1.5.0    2025-09-02 [1] RSPM
#    RColorBrewer   1.1-3    2022-04-03 [1] RSPM (R 4.4.0)
#    Rcpp           1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
#    readr        * 2.1.6    2025-11-14 [1] RSPM
#    renv           1.0.3    2023-09-19 [1] CRAN (R 4.4.0)
#    rlang          1.1.7    2026-01-09 [1] RSPM
#  P rprojroot      2.1.1    2025-08-26 [?] RSPM (R 4.4.0)
#    rstudioapi     0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
#    rsvg           2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
#    S7             0.2.1    2025-11-14 [1] RSPM
#    scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.4.3)
#  P sessioninfo    1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
#    showtext     * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
#    showtextdb   * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
#    snakecase      0.11.1   2023-08-27 [1] RSPM (R 4.4.0)
#  P stats        * 4.4.0    2024-04-24 [?] local
#    stringi        1.8.4    2024-05-06 [1] RSPM (R 4.4.0)
#    stringr      * 1.6.0    2025-11-04 [1] RSPM
#    svglite        2.1.3    2023-12-08 [1] RSPM (R 4.4.0)
#    sysfonts     * 0.8.9    2024-03-02 [1] RSPM (R 4.4.0)
#    systemfonts    1.2.1    2025-01-20 [1] RSPM (R 4.4.0)
#    textshaping    1.0.0    2025-01-20 [1] RSPM (R 4.4.0)
#    tibble       * 3.3.1    2026-01-11 [1] RSPM
#    tidyr        * 1.3.2    2025-12-19 [1] RSPM
#    tidyselect     1.2.1    2024-03-11 [1] RSPM (R 4.4.0)
#    tidyverse    * 2.0.0    2023-02-22 [1] RSPM (R 4.4.0)
#    timechange     0.4.0    2026-01-29 [1] RSPM
#  P tools          4.4.0    2024-04-24 [?] local
#    tzdb           0.4.0    2023-05-12 [1] RSPM (R 4.4.0)
#  P utils        * 4.4.0    2024-04-24 [?] local
#    vctrs          0.7.1    2026-01-23 [1] RSPM
#    vroom          1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
#    withr          3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
#    xfun           0.50     2025-01-07 [1] RSPM (R 4.4.0)
#    xml2           1.5.2    2026-01-17 [1] RSPM
# 
#  V ── Loaded and on-disk version mismatch.
#  P ── Loaded and on-disk path mismatch.
#  
#  ─────────────────────────────────────────────────────────────────────────────────────────
#  > 