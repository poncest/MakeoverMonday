## Challenge: #MakeoverMonday 2025 week 47
## Data:      Do cats really loaf all day?
## Author:    Steven Ponce
## Date:      2025-12-02

## Article
# https://lazy-cats.netlify.app/

## Data
# https://data.world/makeovermonday/2025-week-47-lazy-cats
# https://figshare.com/articles/dataset/How_lazy_are_pet_cats_really_Using_machine_learning_and_accelerometry_to_get_a_glimpse_into_the_behaviour_of_privately_owned_cats_in_different_households/24848292?file=43720347


## Citations
# Smit, M., Corner-Thomas, R. A., Draganova, I., Andrews, C. J., & Thomas, D. G. (2024). 
# How Lazy Are Pet Cats Really? Using Machine Learning and Accelerometry to Get a Glimpse into the Behaviour of 
# Privately Owned Cats in Different Households. Sensors, 24(8), 2623. https://doi.org/10.3390/s24082623
#
# Smit, Michelle (2023). Weekly data cats for home trial. figshare. Dataset. https://doi.org/10.6084/m9.figshare.24848292.v2

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
  glue,          # Interpreted String Literals
  patchwork      # The Composer of Plots
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
cats_data <- read_csv("data/2025/cats_data.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(cats_data)
skim(cats_data) |> summary()


## 4. TIDY DATA ----
### |- clean and factor variables ----
cats_clean <- cats_data |>
  mutate(
    season = factor(season, levels = c("Summer", "Winter")),
    cat_age = factor(cat_age, levels = c("Junior", "Prime", "Mature")),
    bcs_ord = factor(bcs_ord, levels = c("Ideal", "Overweight", "Heavy", "Obese")),
    housing = factor(housing,
      levels = c("Indoor", "Indoor Outdoor"),
      labels = c("Indoor", "Indoor & Outdoor")
    ),
    area = factor(area, levels = c("Urban", "Rural")),
    cat2 = factor(cat2, levels = c("Single", "Multi")),
    children = factor(children,
      levels = c("No", "Yes"),
      labels = c("No Children", "With Children")
    ),
    dog = factor(dog,
      levels = c("No", "Yes"),
      labels = c("No Dog", "With Dog")
    )
  ) |>
  mutate(
    resting_sec = lying + sitting,
    prop_resting = prop_lying + prop_sitting,
    pct_resting = prop_resting * 100,
    pct_lying = prop_lying * 100,
    pct_sitting = prop_sitting * 100,
    pct_active = prop_active * 100,
    pct_standing = prop_standing * 100,
    pct_grooming = prop_grooming * 100,
    pct_eating = prop_eating * 100,
    pct_scratching = prop_scratching * 100,
    pct_littering = prop_littering * 100
  )

### |- wide format for seasonal comparisons ----
cats_seasonal <- cats_clean |>
  select(
    cat_id, season, pct_resting, pct_lying, pct_sitting, pct_active,
    pct_standing, pct_grooming, pct_eating,
    cat_age, cat_sex, bcs, bcs_ord, housing, area, diet, children, cat2, dog
  ) |>
  pivot_wider(
    names_from = season,
    values_from = c(
      pct_resting, pct_lying, pct_sitting, pct_active,
      pct_standing, pct_grooming, pct_eating
    ),
    names_glue = "{.value}_{season}"
  ) |>
  mutate(
    resting_diff = pct_resting_Winter - pct_resting_Summer,
    resting_avg = (pct_resting_Winter + pct_resting_Summer) / 2,
    active_diff = pct_active_Winter - pct_active_Summer
  ) |>
  # Keep only cats with complete data (n = 28)
  filter(!is.na(pct_resting_Summer) & !is.na(pct_resting_Winter))

### |- prepare environmental effects data ----
env_effects <- cats_clean |>
  select(cat_id, season, pct_resting, children, dog, housing) |>
  pivot_longer(
    cols = c(children, dog),
    names_to = "factor_type",
    values_to = "factor_level"
  ) |>
  mutate(
    factor_type = case_when(
      factor_type == "children" ~ "Children in household",
      factor_type == "dog" ~ "Dog in household"
    ),
    factor_type = factor(factor_type,
      levels = c("Children in household", "Dog in household")
    )
  )

### |- calculate means ----
env_means <- env_effects |>
  group_by(factor_type, factor_level, season) |>
  summarise(
    mean_pct = mean(pct_resting, na.rm = TRUE),
    se_pct = sd(pct_resting, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

### |- calculate medians ----
median_summer <- median(cats_seasonal$pct_resting_Summer, na.rm = TRUE)
median_winter <- median(cats_seasonal$pct_resting_Winter, na.rm = TRUE)

# "Lazy band" (most cats spend 70–80% of the day resting)
lazy_low  <- 70
lazy_high <- 80

# data frame for lazy band per facet
lazy_band_df <- tibble(
  factor_type = factor(
    levels(env_effects$factor_type),
    levels = levels(env_effects$factor_type)
  ),
  xmin = -Inf, xmax = Inf,
  ymin = lazy_low, ymax = lazy_high
)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    col_indoor  = "#2C5F8D",
    col_outdoor = "#E07B42",
    col_summer = "#D4952A",
    col_winter = "#6AADC4",
    col_gray = "gray45",
    col_gray_light = "gray70",
    col_grid = "gray92"        
  )
)

### |-  Main titles ----
title_text <- "Pet cats rest 70–80% of the day regardless of season"
subtitle_text <- str_glue(
  "Accelerometer data from 28 New Zealand cats shows most time is spent lying or sitting,<br>",
  "with only small differences by season, housing, or household environment."
)

### |-  Data source caption ----
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 46,
  source_text = str_glue(
    "Smit et al. (2024) Sensors<br>",
    "**Note:** haded band = 70–80% of day resting | Dashed lines in panel A = medians | Points = individual cats, bars = mean ± SE"
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

### |- SCATTER PLOT (Panel A) ----
p1 <- ggplot(cats_seasonal, aes(x = pct_resting_Summer, y = pct_resting_Winter)) +
  # Lazy band
  annotate(
    "rect",
    xmin = lazy_low, xmax = lazy_high,
    ymin = lazy_low, ymax = lazy_high,
    fill = "grey60", alpha = 0.06
  ) +
  # Diagonal & medians
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = colors$palette$col_gray_light, linewidth = 0.7
  ) +
  geom_vline(
    xintercept = median_summer,
    linetype = "dashed", color = colors$palette$col_gray, linewidth = 0.5
  ) +
  geom_hline(
    yintercept = median_winter,
    linetype = "dashed", color = colors$palette$col_gray, linewidth = 0.5
  ) +
  geom_point(aes(color = housing), size = 3.5, alpha = 0.9) +
  # Annotations
  annotate("text",
           x = 57, y = 91,
           label = "More active in summer\nLazier in winter",
           hjust = 0, vjust = 1, size = 2.8, color = colors$palette$col_gray
  ) +
  annotate("text",
           x = 91.5, y = 91.5,
           label = "Consistently lazy",
           hjust = 1, vjust = 1, size = 2.8, color = colors$palette$col_gray
  ) +
  annotate("text",
           x = 57, y = 56,
           label = "Consistently active",
           hjust = 0, vjust = 0, size = 2.8, color = colors$palette$col_gray
  ) +
  annotate("text",
           x = 91.5, y = 56,
           label = "Lazier in summer\nMore active in winter",
           hjust = 1, vjust = 0, size = 2.8, color = colors$palette$col_gray
  ) +
  annotate("text",
           x = 88, y = 86,
           label = "No change",
           hjust = 1, size = 2.6, color = colors$palette$col_gray_light, angle = 45
  ) +
  # Scales
  scale_color_manual(
    values = c("Indoor" = colors$palette$col_indoor, "Indoor & Outdoor" = colors$palette$col_outdoor),
    name = "Housing:"
  ) +
  coord_fixed(xlim = c(55, 93), ylim = c(55, 93)) +
  # Labs
  labs(
    x = "Summer: % of day spent resting",
    y = "Winter: % of day spent resting"
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))
  ) +
  # Theme
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.margin = margin(b = 5),
    legend.box.margin = margin(b = -5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 10, 5, 5)
  )

### |- DOT PLOT (Panel B) ----
p2 <- env_effects |>
  ggplot(aes(x = factor_level, y = pct_resting, color = season)) +
  # Lazy band
  geom_rect(
    data = lazy_band_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.06
  ) +
  # Individual cats
  geom_point(
    position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.5, seed = 42),
    alpha = 0.45,
    size = 1.8
  ) +
  # Means with SE
  geom_pointrange(
    data = env_means,
    aes(
      x = factor_level, y = mean_pct,
      ymin = mean_pct - se_pct, ymax = mean_pct + se_pct,
      color = season
    ),
    position = position_dodge(width = 0.5),
    size = 0.6,
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  # Facet
  facet_wrap(~factor_type, scales = "free_x") +
  # Scales
  scale_color_manual(
    values = c("Summer" = colors$palette$col_summer, "Winter" = colors$palette$col_winter),
    name = "Season:"
  ) +
  scale_y_continuous(
    limits = c(55, 93),
    breaks = seq(60, 90, 10),
    labels = label_percent(accuracy = 1, scale = 1)
  ) +
  # Labs
  labs(
    x = NULL,
    y = "% of day spent resting"
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))
  ) +
  # Theme
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    legend.margin = margin(b = 5),
    legend.box.margin = margin(b = -5),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, 10)
  )

### |- COMBINED PLOTS ----
combined_plots <- p1 + p2 +
  plot_layout(widths = c(1.2, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    # tag_levels = "A",
    theme = theme(
      plot.title = element_text(
        size = rel(1.95),
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
        lineheight = 1.5,
        margin = margin(t = 5, b = 25)
      ),
      plot.caption = element_markdown(
        size = rel(0.55),
        family = fonts$caption,
        color = 'gray50',
        hjust = 0,
        lineheight = 1.2,
        margin = margin(t = 10, b = 10)
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
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

# ─ Session info ───────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-12-02
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
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
# skimr       * 2.1.5    2022-12-23 [1] RSPM (R 4.4.0)
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
# ──────────────────────────────────────────────────────
# > 