## Challenge: #MakeoverMondnay 2025 week 40
## Data:      Better things come to those who wait
## Author:    Steven Ponce
## Date:      2025-10-14

## Article
# https://www.epi.org/publication/better-things-come-to-those-who-wait-the-importance-of-patience-in-diagnosing-labor-force-participation-rates-and-prescribing-policy-solutions/

## Data
# https://data.bls.gov/timeseries/LNS11300060
# https://data.world/makeovermonday/better-things-come-to-those-who-wait (few year were missing)

## Original Chart (Fig A)
# https://www.epi.org/publication/better-things-come-to-those-who-wait-the-importance-of-patience-in-diagnosing-labor-force-participation-rates-and-prescribing-policy-solutions/

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
##       or view source code at: https://github.com/poncest/MakeoverMonday/tree/master/R


# https://data.bls.gov/timeseries/LNS11300060
# 
# Labor Force Statistics from the Current Population Survey					
# Original Data Value					
# 
# Series Id:	LNS11300060				
# Seasonally Adjusted					
# Series title:	(Seas) Labor Force Participation Rate - 25-54 yrs.				
# Labor force status:	Civilian labor force participation rate				
# Type of data:	Percent or rate				
# Age:	25 to 54 years				
# Years:	1995 to 2025	


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork   # Using Fonts More Easily in R Graphs
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 16,
  height = 9,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
labor_force_raw <- readxl::read_excel("data/2025/SeriesReport-20251013201453_5eccfc.xlsx",
                                      skip = 11) |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(labor_force_raw)
skimr::skim_without_charts(labor_force_raw)


## 4. TIDY DATA ----
labor_force_clean <- labor_force_raw |>
  pivot_longer(
    cols = jan:dec,
    names_to = "month",
    values_to = "lfpr"
  ) |>
  mutate(
    month_num = match(month, tolower(month.abb)),
    date = make_date(year, month_num, 1)
  ) |>
  drop_na(lfpr) |>
  arrange(date) |>
  select(date, year, month, lfpr)

# Define recession periods
recessions <- tribble(
  ~start, ~end, ~label,
  "2001-03-01", "2001-11-01", "2001", # Dot-com
  "2007-12-01", "2009-06-01", "2008", # Housing
  "2020-02-01", "2020-04-01", "2020"  # Pandemic
) |>
  mutate(
    start = ymd(start),
    end = ymd(end)
  )

### |- P1: Dumbbell Chart Data ----
# Calculate peak before and trough after each recession
recession_impact <- recessions |>
  mutate(
    peak_before = map_dbl(start, ~ {
      labor_force_clean |>
        filter(date >= .x - years(2), date < .x) |>
        pull(lfpr) |>
        max()
    }),
    trough_after = map_dbl(start, ~ {
      labor_force_clean |>
        filter(date >= .x, date <= .x + years(3)) |>
        pull(lfpr) |>
        min()
    }),
    drop = peak_before - trough_after,
    label_full = paste0(label, " Recession")
  ) |>
  arrange(desc(year(start)))

### |- P2: Line Chart Data ----
# Create normalized recession cycles data
recession_cycles <- recessions |>
  mutate(
    cycle_data = map2(start, label, ~ {
      labor_force_clean |>
        filter(date >= (.x - years(2)), date <= (.x + years(5))) |>
        mutate(
          months_from_start = interval(.x, date) %/% months(1),
          recession = .y
        )
    })
  ) |>
  unnest(cycle_data) |>
  select(recession, months_from_start, date, lfpr, label)

# Calculate baseline and change
recession_cycles <- recession_cycles |>
  group_by(recession) |>
  mutate(
    baseline = lfpr[which.min(abs(months_from_start))],
    change_from_baseline = lfpr - baseline,
    label_full = paste0(recession, " Recession")
  ) |>
  ungroup() |>
  arrange(desc(recession))

recovery_annotation <- tibble(
  label_full = "2020 Recession",
  months_from_start = 30,
  change_from_baseline = 0.95,
  label_text = "Exceeded\npre-recession level"
)

endpoint_labels <- recession_cycles |> 
  group_by(label_full, recession) |> 
  filter(months_from_start == max(months_from_start)) |>
  ungroup() |>
  mutate(
    label_text = paste0(
      ifelse(change_from_baseline > 0, "+", ""), 
      round(change_from_baseline, 1), "pp"
    )
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  "2001" = "#5B8FA3",  
  "2008" = "#D4A373", 
  "2020" = "#6B5B73",   
  "peak" = "#2D5D4F",
  "trough" = "#B85C5C",
  "neutral" = "#4A5859"
))   

### |-  titles and caption ----
title_text <- str_glue("Recessions and Recovery: Not All Crises Are Equal")

subtitle_text <- str_glue(
  "Prime-age **labor force participation** drops during recessions but recovers at **vastly different speeds**. **2008** caused lasting damage, participation never recovered.<br>", 
  "**2020** was severe but brief bouncing back to exceed pre-recession levels"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 40,
  source_text = "U.S. Bureau of Labor Statistics | Prime-age workers (25-54 years) | Current Population Survey (LNS11300060)"
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

### |- P1: Dumbbell Chart ----
p1 <-
ggplot(recession_impact, aes(y = reorder(label_full, -year(start)))) +
  # Geoms
  geom_segment(
    aes(x = trough_after, xend = peak_before, y = label_full, yend = label_full),
    linewidth = 3, color = "#E8E8E8", lineend = "round"
  ) +
  geom_segment(
    aes(x = trough_after, xend = peak_before, y = label_full, yend = label_full),
    linewidth = 1.5, color = colors$palette["neutral"], lineend = "round"
  ) +
  geom_point(aes(x = peak_before),
    size = 7, color = colors$palette["peak"], shape = 21,
    fill = colors$palette["peak"], stroke = 0
  ) +
  geom_point(aes(x = trough_after),
    size = 7, color = colors$palette["trough"], shape = 21,
    fill = colors$palette["trough"], stroke = 0
  ) +
  geom_text(
    aes(x = peak_before, label = paste0(round(peak_before, 1), "%")),
    hjust = -0.6, size = 3.8, fontface = "bold", color = colors$palette["peak"],
    family = "sans"
  ) +
  geom_text(
    aes(x = trough_after, label = paste0(round(trough_after, 1), "%")),
    hjust = 1.6, size = 3.8, fontface = "bold", color = colors$palette["trough"],
    family = "sans"
  ) +
  geom_text(
    aes(
      x = (peak_before + trough_after) / 2,
      label = paste0("▼ ", round(drop, 1), " pp")
    ),
    vjust = -1.2, size = 3.3, color = "#333333", fontface = "bold",
    family = "sans"
  ) +
  # Scales
  scale_x_continuous(
    labels = label_percent(scale = 1),
    limits = c(79, 85),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  # Labs
  labs(
    title = "2020's Drop Was Steepest at -3.3 Percentage Points",
    subtitle = NULL,  
    x = "Labor Force Participation Rate",
    y = NULL,
    caption = NULL
  ) +
  # Theme
  theme(
    plot.title = element_text(
      face = "bold", size = 17, color = "#1a1a1a",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 11.5, color = "#5a5a5a",
      margin = margin(b = 20), lineheight = 1.2
    ),
    plot.caption = element_text(
      size = 9, color = "#7a7a7a", hjust = 0,
      margin = margin(t = 15)
    ),
    axis.text.y = element_text(face = "bold", size = 11.5, color = "#2a2a2a"),
    axis.text.x = element_text(size = 10, color = "#4a4a4a"),
    axis.title.x = element_text(size = 11, color = "#4a4a4a", margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#f0f0f0", linewidth = 0.3),
  )

### |- P2: Line Chart ----
p2 <-
ggplot(recession_cycles, aes(x = months_from_start, y = change_from_baseline)) +
  # Geoms
  geom_hline(yintercept = 0, color = "#CCCCCC", linetype = "solid", linewidth = 0.6) +
  geom_vline(
    xintercept = 0, color = colors$palette["trough"],
    linetype = "solid", linewidth = 1.2, alpha = 0.7
  ) +
  geom_line(aes(color = recession), linewidth = 1.4, alpha = 0.9) +
  geom_point(
    data = recession_cycles |> filter(months_from_start == 0),
    aes(color = recession), size = 3.5, shape = 21, fill = "white", stroke = 1.5
  ) +
  geom_text(
    data = recession_cycles |> 
      group_by(recession) |> 
      filter(months_from_start == max(months_from_start)) |>
      ungroup(),
    aes(label = paste0(ifelse(change_from_baseline > 0, "+", ""), 
                       round(change_from_baseline, 1), "pp")),
    hjust = -0.2, size = 3.2, fontface = "bold",
    family = "sans"
  ) +
  geom_text(
    data = endpoint_labels,
    aes(x = months_from_start, y = change_from_baseline, 
        label = label_text, color = recession),
    hjust = -0.2, size = 3.2, fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = recovery_annotation,
    aes(x = months_from_start, y = change_from_baseline, label = label_text),
    size = 2.8, color = "#2D5D4F", fontface = "italic", 
    lineheight = 0.9, hjust = 0.5,
    inherit.aes = FALSE
  ) +
  # Scales
  scale_color_manual(values = unlist(colors$palette[c("2001", "2008", "2020")])) +
  scale_x_continuous(
    breaks = seq(-24, 60, 12),
    labels = function(x) ifelse(x == 0, "Start", paste0(x, "m")),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " pp", style_positive = "plus"),
    breaks = seq(-4, 2, 1),
  ) +
  # Labs
  labs(
    title = "But 2020 Recovered Fastest—2008 Still Hasn't",
    subtitle = NULL,  
    x = "Months from Recession Start",
    y = "pp Change from Pre-Recession Level",
    caption = NULL
  ) +
  # Facets
  facet_wrap(~label_full, ncol = 1, scales = "free_x") +
  # Theme
  theme(
    plot.title = element_text(
      face = "bold", size = 17, color = "#1a1a1a",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      size = 11.5, color = "#5a5a5a",
      margin = margin(b = 20), lineheight = 1.2
    ),
    plot.caption = element_text(
      size = 9, color = "#7a7a7a", hjust = 0,
      margin = margin(t = 15)
    ),
    strip.text = element_text(
      face = "bold", size = 12, color = "#2a2a2a",
      hjust = 0, margin = margin(b = 10)
    ),
    strip.background = element_blank(),
    axis.text = element_text(size = 10, color = "#4a4a4a"),
    axis.title = element_text(size = 11, color = "#4a4a4a"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
    panel.spacing.y = unit(1.5, "lines"),
  )

### |- Combined Plots ----
combined_plots <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(0.8, 1.5))

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

# 6. HELPER FUNCTIONS DOCUMENTATION ----

## ============================================================================ ##
##                     CUSTOM HELPER FUNCTIONS                                  ##
## ============================================================================ ##
#
# This analysis uses custom helper functions for consistent theming, fonts, 
# and formatting across all my #TidyTuesday projects. The core analysis logic
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
#                                and #TidyTuesday attribution
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

# ─ Session info ───────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-14
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────────────────────────
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
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
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
# ──────────────────────────────────────────────────────────────────────────────────────
# > 
