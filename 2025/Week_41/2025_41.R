## Challenge: #MakeoverMondnay 2025 week 41
## Data:      Drug Consumption (UCI)
## Author:    Steven Ponce
## Date:      2025-10-21

## Article
# https://www.kaggle.com/datasets/obeykhadija/drug-consumptions-uci?resource=download

## Data
# https://data.world/makeovermonday/2025-week-41-drug-consumptions-uci
# https://archive.ics.uci.edu/dataset/373/drug+consumption+quantified

## Citation
# Fehrman, E., Egan, V., & Mirkes, E. (2015). 
# Drug Consumption (Quantified) [Dataset]. 
# UCI Machine Learning Repository. https://doi.org/10.24432/C5TC7S.

## NOTE: This script uses custom helper functions for theming and formatting.
## See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details,
## or view source code at: https://github.com/poncest/MakeoverMonday/tree/master/R


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
  width  = 14,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
drug_consumption_raw <- read_csv("data/2025/Drug_Consumption.csv") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(drug_consumption_raw)
skimr::skim_without_charts(drug_consumption_raw)


## 4. TIDY DATA ----
drug_consumption <- drug_consumption_raw |>
  mutate(
    across(alcohol:vsa, ~ case_when(
      .x == "CL0" ~ 0, .x == "CL1" ~ 1, .x == "CL2" ~ 2,
      .x == "CL3" ~ 3, .x == "CL4" ~ 4, .x == "CL5" ~ 5,
      .x == "CL6" ~ 6, TRUE ~ NA_real_
    ))
  )

### |- P1: Age trajectory data ----
age_trajectory_data <- drug_consumption |>
  select(age, cannabis, alcohol, nicotine, coke, ecstasy, lsd) |>
  mutate(age_numeric = case_when(
    age == "18-24" ~ 21, age == "25-34" ~ 29.5, age == "35-44" ~ 39.5,
    age == "45-54" ~ 49.5, age == "55-64" ~ 59.5, age == "65+" ~ 70
  )) |>
  pivot_longer(
    cannabis:lsd,
    names_to = "substance",
    values_to = "usage"
  ) |>
  filter(!is.na(age_numeric)) |>
  group_by(age_numeric, age, substance) |>
  summarise(
    pct_recent = mean(usage >= 3, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  mutate(
    substance = factor(substance,
      levels = c("cannabis", "alcohol", "nicotine", "coke", "ecstasy", "lsd"),
      labels = c("Cannabis", "Alcohol", "Nicotine", "Cocaine", "Ecstasy", "LSD")
    ),
    highlight = substance == "Cannabis"
  )

### |- P2: Polydrugs patterns data ----
polydrug_data <- drug_consumption |>
  select(id, age, gender, alcohol:vsa) |>
  filter(age != "65+") |>
  pivot_longer(
    alcohol:vsa,
    names_to = "drug",
    values_to = "level"
  ) |>
  filter(drug != "semer") |>
  mutate(recent_use = if_else(level >= 3, 1, 0)) |>
  group_by(id, age, gender) |>
  summarise(
    num_drugs = sum(recent_use, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    polydrug_category = case_when(
      num_drugs == 0 ~ "None",
      num_drugs == 1 ~ "Single Drug",
      num_drugs == 2 ~ "Two Drugs",
      num_drugs == 3 ~ "Three Drugs",
      num_drugs >= 4 ~ "4+ Drugs"
    ),
    polydrug_category = factor(polydrug_category,
      levels = c(
        "None", "Single Drug", "Two Drugs",
        "Three Drugs", "4+ Drugs"
      )
    ),
    age = factor(age, levels = c("18-24", "25-34", "35-44", "45-54", "55-64"))
  )

### |- P2a: Headline metrics for polydrug use (3+ substances) ----
polydrug_summary <- polydrug_data |>
  count(age, gender, polydrug_category, name = "n") |>
  group_by(age, gender) |>
  mutate(p = n / sum(n)) |>
  summarise(
    share_3plus = sum(p[polydrug_category %in% c("Three Drugs", "4+ Drugs")]),
    .groups = "drop"
  )

# Overall headline for the youngest group (used in the global subtitle)
share_3plus_18_24_overall <- polydrug_summary |>
  filter(age == "18-24") |>
  summarise(value = mean(share_3plus, na.rm = TRUE)) |>
  pull(value)

# Calculate 3+ drug percentages for annotation
p2_labels <- polydrug_data |>
  count(age, gender, polydrug_category) |>
  group_by(age, gender) |>
  mutate(pct = n / sum(n) * 100) |>
  filter(polydrug_category %in% c("Three Drugs", "4+ Drugs")) |>
  group_by(age, gender) |>
  summarise(
    pct_3plus = sum(pct),
    .groups = "drop"
  )

### |- P3: Conscientiousness data ----
conscientiousness_data <- drug_consumption |>
  mutate(
    high_risk = (nscore > 0) & (impulsive > 0) & (ss > 0),
    cscore_category = case_when(
      cscore < quantile(cscore, 0.25, na.rm = TRUE) ~ "Very Low",
      cscore < quantile(cscore, 0.5, na.rm = TRUE) ~ "Low",
      cscore < quantile(cscore, 0.75, na.rm = TRUE) ~ "High",
      TRUE ~ "Very High"
    ),
    cscore_category = factor(cscore_category,
      levels = c("Very Low", "Low", "High", "Very High")
    )
  ) |>
  rowwise() |>
  mutate(
    heavy_illegal = sum(c(
      cannabis, coke, ecstasy, lsd, mushrooms,
      ketamine, amphet
    ) >= 4, na.rm = TRUE) >= 2
  ) |>
  ungroup() |>
  filter(high_risk) |>
  group_by(cscore_category) |>
  summarise(
    pct_heavy_use = mean(heavy_illegal, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  )

# Calculate reduction
reduction_pp <- conscientiousness_data$pct_heavy_use[1] -
  conscientiousness_data$pct_heavy_use[4]


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    danger        = "#B5532F",     
    primary       = "#1E3A5F",
    neutral_dark  = "#5378A6",   
    neutral_mid   = "#9FB6D0",   
    neutral_light = "#E8E8E8",  
    other_lines   = "#B8B8B8",  
    text_primary  = "#2B2D42",
    text_secondary= "#6C757D"
    )
  )   

### |-  titles and caption ----
title_text <- str_glue("The Drug Use Prevention Playbook: Three Key Insights")

subtitle_text <- str_glue(
    "Peak use at 18–24 • {scales::percent(share_3plus_18_24_overall, accuracy = 1)} use 3+ substances • ",
    "{sprintf('%.1f', reduction_pp)} pp lower heavy use at very high conscientiousness"
  )

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 41,
  source_text = "UCI Drug Consumption Dataset (N = 1,884) | Recent use = past year or more frequently"
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

### |- P1: Age trajectory chart ----
p1 <-
ggplot() +
  # Annotate
  annotate("rect", xmin=18, xmax=26, ymin=0, ymax=100,
           fill="#EDEDED", color=NA, alpha=0.35) +
  annotate("text",
    x = 22, y = 95, label = "Peak Risk\nAge Group",
    color = colors$text_secondary, size = 3, fontface = "italic", lineheight = 0.9
  ) +
  annotate("text",
           x = 23, y = 82, label = "Cannabis",
           color = colors$palette$danger, fontface = "bold", size = 4, hjust = 0
  ) +
  annotate("text",
           x = 23, y = 15, label = "Other\nsubstances",
           color = colors$palette$other_lines, size = 3.2, hjust = 0, lineheight = 0.9
  ) +
  # Geoms
  geom_line(
    data = filter(age_trajectory_data, !highlight),
    aes(age_numeric, pct_recent, group = substance),
    color = colors$palette$other_lines, linewidth = 1.2, alpha = 0.7
  ) +
  geom_line(
    data = filter(age_trajectory_data, highlight),
    aes(age_numeric, pct_recent, group = substance),
    color = colors$palette$danger, linewidth = 2.8
  ) +
  geom_point(
    data = filter(age_trajectory_data, highlight),
    aes(age_numeric, pct_recent),
    size = 3.2, color = colors$palette$danger
  ) +
  # Scales
  scale_x_continuous(
    breaks = c(21, 29.5, 39.5, 49.5, 59.5, 70),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100), expand = c(0, 0)
  ) +
  # Labs
  labs(
    title = "Drug Use Peaks in Young Adulthood",
    subtitle = "Cannabis shows the highest usage rates, especially among 18-24 year olds",
    x = "Age Group",
    y = "Recent Usage Rate (%)"
  ) +
  # Theme
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

### |- P2: Polydrugs patterns chart ----
p2 <-
ggplot(polydrug_data, aes(age, fill = polydrug_category)) +
  # Geoms
  geom_bar(position = "fill", width = 0.75) +
  # Scales
  scale_y_continuous(labels = percent_format(), expand = c(0, 0.02)) +
  scale_fill_manual(values = c(
    "None"        = "gray",
    "Single Drug" = "gray",
    "Two Drugs"   = colors$palette$neutral_mid,
    "Three Drugs" = colors$palette$neutral_dark,
    "4+ Drugs"    = colors$palette$danger
  )) +
  coord_cartesian(clip = "off", ylim = c(0, 1)) +
  # labs
  labs(
    title = "Young Adults Stack Multiple Substances",
    subtitle = "The 'danger zone' (4+ drugs) is most prevalent among 18-24 year olds",
    x = "Age Group",
    y = "Percentage of Respondents (%)",
    fill = "Number of\nSubstances"
  ) +
  # Facets
  facet_wrap(~gender, labeller = labeller(gender = c(F = "Female", M = "Male"))) +
  # Theme
  theme(
    legend.position = "bottom",
    legend.justification = "top",
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(
      face = "bold", 
      color = "gray20", 
      size = rel(1),
      margin = margin(t = 8, b = 8)
    ),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

### |- P3: Conscientiousness chart ----
p3 <-
ggplot(
  conscientiousness_data,
  aes(cscore_category, pct_heavy_use)
) +
  # Geoms
  geom_col(aes(fill = cscore_category), width = 0.65, alpha = 0.95) +
  geom_text(aes(label = sprintf("%.1f%%", pct_heavy_use)),
    vjust = -0.8, fontface = "bold", size = 5.2,
    color = colors$palette$text_primary
  ) +
  # Annotate
  annotate("rect",
    xmin = 0.5, xmax = 2.5, ymin = 0, ymax = Inf,
    fill = alpha(colors$palette$danger, 0.08), color = NA
  ) +
  annotate("segment",
    x = 1.2, xend = 3.8,
    y = max(conscientiousness_data$pct_heavy_use) + 4,
    yend = max(conscientiousness_data$pct_heavy_use) + 4,
    arrow = arrow(ends = "both", length = unit(0.15, "inches")),
    color = colors$palette$text_secondary, linewidth = 0.3
  ) +
  annotate("text",
    x = 2.5,
    y = max(conscientiousness_data$pct_heavy_use) + 5.5,
    label = sprintf("%.1f pp reduction", reduction_pp),
    fontface = "bold", size = 4.2, color = colors$palette$text_primary
  ) +
  annotate("text",
    x = 1.5, y = 2, label = "Risk Zone",
    color = colors$palette$danger, fontface = "bold.italic", size = 3
  ) +
  annotate("text",
    x = 3.5, y = 2, label = "Protected Zone",
    color = colors$palette$neutral_dark, fontface = "bold.italic", size = 3
  ) +
  # Scales
  scale_fill_manual(values = c(
    "Very Low" = colors$palette$danger,
    "Low" = colors$palette$danger,
    "High" = colors$palette$neutral_dark,
    "Very High" = colors$palette$neutral_dark
  )) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(conscientiousness_data$pct_heavy_use) + 8),
    expand = c(0, 0)
  ) +
  # Labs
  labs(
    title = "Conscientiousness: The Protective Shield",
    subtitle = "High conscientiousness creates a protective effect against heavy drug use in high-risk individuals",
    x = "Conscientiousness Level",
    y = "Heavy Drug Use Rate (%)",
    caption = "High-risk = above avg in neuroticism, impulsivity & sensation seeking | Heavy use = monthly+ use of 2+ illegal drugs"
  ) +
  # Theme
  theme(
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(lineheight = 1.3, size = 8, family = fonts$caption, color = colors$caption),
    plot.margin = margin(0, 0, 0, 0)
  )

### |- Combined Plots ----
combined_plots <- (p1 | p2) / p3 +
  plot_layout(heights = c(1, 2))

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
        size = rel(0.65),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
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

# ─ Session info ───────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-10-21
# rstudio  2025.09.1+401 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────────────────────
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
# ──────────────────────────────────────────────────────────────────
# > 