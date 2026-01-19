## Challenge: #MakeoverMonday 2026 week 03
## Data:      Women+ Conductors on Broadway
## Author:    Steven Ponce
## Date:      2026-01-19

## Article
# https://maestramusic.org/resources/timelines/timeline-of-broadway-conductors-and-mds/

## Data
# https://data.world/makeovermonday/2026w3-women-conductors-on-broadway

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, patchwork, janitor, readxl
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 8,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
conductor_raw <- readxl::read_excel("data/2026/Conductor Timeline Data_Contest.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(conductor_raw)
skimr::skim(conductor_raw) |> summary()


## 4. TIDY DATA ----
# Define role tiers
role_tiers <- tribble(
  ~role,                         ~tier,          ~tier_order,
  "Music Supervisor",            "Leadership",   1,
  "Music Director",              "Leadership",   1,
  "Conductor",                   "Leadership",   1,
  "Co-Music Director",           "Leadership",   1,
  "Co-Music Supervisor",         "Leadership",   1,
  "Associate Music Director",    "Support",      2,
  "Associate Music Supervisor",  "Support",      2,
  "Associate Conductor",         "Support",      2,
  "Resident Music Supervisor",   "Support",      2,
  "Assistant Music Director",    "Support",      2,
  "Assistant Conductor",         "Support",      2,
  "Alternate Conductor",         "Supplemental", 3,
  "Substitute Conductor",        "Supplemental", 3,
  "Children's Music Director",   "Specialized",  4
)

conductor_clean <- conductor_raw |>
  mutate(
    year = year(opening_date),
    era = case_when(
      year < 1980 ~ "Pre-1980",
      year >= 1980 & year < 2000 ~ "1980-1999",
      year >= 2000 & year < 2020 ~ "2000-2019",
      year >= 2020 ~ "2020+"
    )
  ) |>
  left_join(role_tiers, by = "role") |>
  filter(!is.na(era), !is.na(tier))

# Panel 1 Data: Cumulative
cumulative_data <- conductor_clean |>
  distinct(person_id, year) |>
  group_by(person_id) |>
  summarize(first_year = min(year), .groups = "drop") |>
  count(first_year) |>
  arrange(first_year) |>
  mutate(cumulative = cumsum(n))

latest_val <- filter(cumulative_data, first_year == max(first_year))

m50 <- filter(cumulative_data, cumulative >= 50) |> slice(1)

# Panel 2 Data: Role Stats with Chronological Ordering
role_era_stats <- conductor_clean |>
  group_by(era) |>
  mutate(era_n = n()) |>
  ungroup() |>
  mutate(era_label = glue("**{era}**<br>(n={era_n})")) |>
  mutate(era_label = fct_reorder(era_label, year, .desc = TRUE)) |>
  count(era_label, tier) |>
  group_by(era_label) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  mutate(tier = factor(tier, levels = rev(c("Leadership", "Support", "Supplemental", "Specialized"))))


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(
  palette = list(
    leadership = "#134050", 
    support = "#4A90A4",
    supplemental = "#7FB3C4",
    specialized = "#B8D4DE"
  )
)

tier_colors <- c(
  "Leadership" = colors$palette$leadership,
  "Support" = colors$palette$support,
  "Supplemental" = colors$palette$supplemental,
  "Specialized" = colors$palette$specialized
)

### |-  Main titles ----
title_text <- "The Accelerating Rise of Women+ Conductors on Broadway"

subtitle_text <- str_glue(
  "While it took 46 years to reach the first 50 unique conductors, that number doubled in just the next 24 years.<br>",
  "Today, **Leadership roles** account for over 40% of positions in the 2020s."
)

caption_text <- create_social_caption(
  mm_year = 2026, mm_week = 03,
  source_text = str_glue(
    "Maestra Music"
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

    # Axis formatting
    axis.line.x           = element_line(color = "#252525", linewidth = .1),
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

### |-  p1: cummulative growth ----
p1 <-
  ggplot(cumulative_data, aes(first_year, cumulative)) +
  # Geoms
  geom_area(fill = colors$palette$leadership, alpha = 0.1) +
  geom_line(color = colors$palette$leadership, linewidth = 1.2) +
  geom_point(data = m50, color = colors$palette$leadership, size = 2) +
  geom_point(data = latest_val, color = colors$palette$leadership, size = 3) +
  # Annotate
  annotate("richtext",
    x = 1990, y = 62, label = "50th reached<br>in **2000**",
    fill = NA, label.color = NA, family = fonts$text, size = 3.5
  ) +
  annotate("text",
    x = 2024, y = 108, label = "103 Unique\nConductors",
    fontface = "bold", color = colors$leadership, family = fonts$text, hjust = 1, lineheight = 0.9
  ) +
  # Scales
  scale_x_continuous(limits = c(1954, 2025), breaks = seq(1960, 2020, 20)) +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
  # Labs
  labs(title = "Cumulative Growth", x = NULL, y = NULL) +
  # Theme
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

### |-  p2: role mix ----
# label data
label_data <- role_era_stats |>
  arrange(era_label, desc(tier)) |>
  group_by(era_label) |>
  mutate(
    cumsum_pct = cumsum(pct),
    label_pos = cumsum_pct - pct / 2,
    # Color logic: Light bars get dark text
    label_col = if_else(tier %in% c("Specialized", "Supplemental"), colors$text, "white"),
    # Label logic: Direct legend for top bar, % only for others if > 7%
    display_label = case_when(
      str_detect(era_label, "2020") ~ glue("**{percent(pct, 1)}**<br>{tier}"),
      pct >= 0.08 ~ glue("**{percent(pct, 1)}**"),
      TRUE ~ ""
    )
  )

p2 <-
ggplot(role_era_stats, aes(era_label, pct, fill = tier)) +
  # Geoms
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  geom_richtext(data = label_data,
                aes(y = label_pos, label = display_label, color = label_col),
                fill = NA, label.color = NA, family = fonts$text, size = 3, lineheight = 1) +
  # Scales
  scale_fill_manual(values = tier_colors) +
  scale_color_identity() +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  coord_flip() +
  # Labs
  labs(title = "Role Mix by Era", x = NULL, y = NULL) +
  # theme_minimal(base_family = fonts$text) +
  # Theme
  theme(
    legend.position = "none", 
    panel.grid = element_blank(),
    axis.text.y = element_markdown(lineheight = 1.1)
    )

### |-  combined plot ----
combined_plot <- p1 + p2 +
  plot_layout(widths = c(1, 1.15)) +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(1.8),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 5, b = 10)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.85),
        family = fonts$subtitle,
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
      ),
    )
  )

combined_plot

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

# ─ Session info ──────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-01-19
# rstudio  2025.09.2+418 Cucumberleaf Sunflower (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────
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
# patchwork   * 1.3.0    2024-09-16 [1] RSPM (R 4.4.0)
# pillar        1.10.1   2025-01-07 [1] RSPM (R 4.4.0)
# pkgconfig     2.0.3    2019-09-22 [1] RSPM (R 4.4.0)
# purrr       * 1.0.2    2023-08-10 [1] RSPM (R 4.4.0)
# R6            2.5.1    2021-08-19 [1] RSPM (R 4.4.0)
# ragg          1.3.3    2024-09-11 [1] RSPM (R 4.4.0)
# Rcpp          1.0.14   2025-01-12 [1] RSPM (R 4.4.0)
# readr       * 2.1.5    2024-01-10 [1] RSPM (R 4.4.0)
# readxl      * 1.4.3    2023-07-06 [1] RSPM (R 4.4.0)
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
# P utils       * 4.4.0    2024-04-24 [?] local
# vctrs         0.6.5    2023-12-01 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────
# > 