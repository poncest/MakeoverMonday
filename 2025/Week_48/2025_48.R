## Challenge: #MakeoverMonday 2025 week 48
## Data:      Which AI Models Hallucinate the Most?
## Author:    Steven Ponce
## Date:      2025-12-08

## Article
# https://www.voronoiapp.com/technology/Which-AI-Models-Hallucinate-the-Most-7211

## Data
# https://data.world/makeovermonday/2025wk48-which-ai-models-hallucinate-the-most

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
  patchwork,     # The Composer of Plots
  ggrepel        # Automatically Position Non-Overlapping Text Labels with ggplot2
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 14,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
ai_models <- readxl::read_excel("data/2025/AI Model Hallucination Scores.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(ai_models)
skim(ai_models) |> summary()


## 4. TIDY DATA ----

### |-  data wrangling ----
ai_models_tidy <- ai_models |>
  mutate(
    model_type = case_when(
      str_detect(model, "Claude|GPT-5|Gemini|Grok") ~ "Proprietary",
      str_detect(model, "DeepSeek|Llama|Qwen|GPT-OSS|Kimi|Magistral") ~ "Open Weights",
      TRUE ~ "Unknown"
    ),
    model_family = case_when(
      str_detect(model, "Claude") ~ "Claude",
      str_detect(model, "GPT") ~ "GPT",
      str_detect(model, "Gemini") ~ "Gemini",
      str_detect(model, "Grok") ~ "Grok",
      str_detect(model, "DeepSeek") ~ "DeepSeek",
      str_detect(model, "Llama") ~ "Llama",
      str_detect(model, "Qwen") ~ "Qwen",
      str_detect(model, "Kimi") ~ "Kimi",
      str_detect(model, "Magistral") ~ "Magistral",
      TRUE ~ "Other"
    ),
    model_short = str_remove(model, " v\\d+\\.\\d+$") |>
      str_remove(" BA\\d+B \\d+$"),
    median_accuracy = median(accuracy_index_higher_is_better),
    median_hallucination = median(hallucination_index_lower_is_better),
    quadrant = case_when(
      accuracy_index_higher_is_better > median_accuracy &
        hallucination_index_lower_is_better < median_hallucination ~
        "High Accuracy, Low Hallucination",
      accuracy_index_higher_is_better > median_accuracy &
        hallucination_index_lower_is_better >= median_hallucination ~
        "High Accuracy, High Hallucination",
      accuracy_index_higher_is_better <= median_accuracy &
        hallucination_index_lower_is_better < median_hallucination ~
        "Low Accuracy, Low Hallucination",
      TRUE ~ "Low Accuracy, High Hallucination"
    ),
    combined_score = accuracy_index_higher_is_better - hallucination_index_lower_is_better,
    rank_combined = rank(-combined_score, ties.method = "min"),
    rank_label = glue("Rank {rank_combined} of {n()}")
  )

median_acc <- median(ai_models_tidy$accuracy_index_higher_is_better)
median_hall <- median(ai_models_tidy$hallucination_index_lower_is_better)

top_6_models <- ai_models_tidy |>
  arrange(desc(combined_score)) |>
  slice_head(n = 6)


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    proprietary   = "#0077B6",
    open_weights  = "#E07A5F",
    neutral_light = "#E8F4F8",
    neutral_mid   = "#90C9E8",
    success       = "#06A77D"       
  )
)

### |-  Main titles ----
title_text    <- "AI Model Performance: Accuracy vs. Hallucination"
subtitle_text <- "Top 6 models by combined accuracy–hallucination score, shown in context of 18 leading AI models"

### |-  Data source caption ----
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 48,
  source_text = str_glue(
    "artificialanalysis.ai (AA-Omniscience Index)"
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
    legend.justification = "right",
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

### |- PANEL 1: SCORE CARDS ----
p1 <- top_6_models |>
  ggplot(aes(x = 1, y = 1)) +
  # Geoms
  geom_tile(
    aes(fill = model_type),
    alpha = 0.18,
    linewidth = 3
  ) +
  geom_text(
    aes(label = glue(
      "{rank_label}\n\n",
      "Accuracy: {percent(accuracy_index_higher_is_better, accuracy = 1)}\n",
      "Hallucination: {percent(hallucination_index_lower_is_better, accuracy = 1)}\n\n",
      "Combined score: {sprintf('%.2f', combined_score)}"
    )),
    size = 3.8,
    lineheight = 1.1,
    fontface = "plain",
    color = "gray20"
  ) +
  # Facets
  facet_wrap(
    ~ reorder(model_short, -combined_score),
    ncol   = 3,
    scales = "free"
  ) +
  # Scales
  scale_fill_manual(
    values = c(
      "Proprietary" = colors$palette$proprietary,
      "Open Weights" = colors$palette$open_weights
    ),
    name = NULL
  ) +
  # Labs
  labs(title = "Top 6 Models by Combined Performance") +
  # Theme
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 9, face = "bold"),
    strip.text = element_text(
      size = rel(1),
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 4, b = 4)
    ),
    plot.margin = weekly_theme$plot.margin,
    panel.spacing = unit(10, "pt"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

### |- PANEL 2: SCATTERPLOT ----
p2 <- ai_models_tidy |>
  ggplot(aes(
    x = hallucination_index_lower_is_better,
    y = accuracy_index_higher_is_better
  )) +
  # Annotate
  annotate(
    "rect",
    xmin = -Inf, xmax = median_hall,
    ymin = median_acc, ymax = Inf,
    fill = colors$palette$neutral_light, alpha = 0.5
  ) +
  annotate(
    "text",
    x = median_hall * 0.6,
    y = Inf,
    label = "IDEAL ZONE\nHigh accuracy\nLow hallucination",
    vjust = 1.25,
    size = 3,
    color = "gray40",
    fontface = "bold",
    lineheight = 0.9
  ) +
  # Geoms
  geom_vline(
    xintercept = median_hall,
    linetype   = "dashed",
    color      = "gray55",
    linewidth  = 0.5
  ) +
  geom_hline(
    yintercept = median_acc,
    linetype   = "dashed",
    color      = "gray55",
    linewidth  = 0.5
  ) +
  geom_point(
    data = ai_models_tidy |> filter(rank_combined > 6),
    aes(color = model_type),
    size = 3.3,
    alpha = 0.25
  ) +
  geom_point(
    data = top_6_models,
    aes(color = model_type),
    size = 5,
    alpha = 0.95
  ) +
  geom_text_repel(
    data = top_6_models,
    aes(label = model_short, color = model_type),
    size = 3.2,
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "gray60",
    segment.size = 0.5,
    min.segment.length = 0,
    max.overlaps = 20,
    show.legend = FALSE,
    seed = 1234
  ) +
  # Scales
  scale_color_manual(
    values = c(
      "Proprietary" = colors$palette$proprietary,
      "Open Weights" = colors$palette$open_weights
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = 0.05)
  ) +
  # Labs
  labs(
    title = "All 18 Models in Context",
    x = "Hallucination rate (lower is better)",
    y = "Accuracy (higher is better)"
  )

### |- COMBINED PLOTS ----
# combined_plots <-
p1 / p2 +
  plot_layout(heights = c(1, 1.3)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
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
        color = "gray50",
        hjust = 0,
        lineheight = 1.2,
        margin = margin(t = 10, b = 10)
      ),
      plot.margin = margin(10, 15, 10, 15)
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

# ─ Session info ─────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-12-08
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────────────
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
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────────────
# > 
