## Challenge: #MakeoverMonday 2026 week 16
## Data:      AI Risk Rankings
## Author:    Steven Ponce
## Date:      2026-04-20

## Article
# https://www.aiexposure.org/rankings

## Data
# https://data.world/makeovermonday/ai-risk-rankings

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, 
  glue, janitor, patchwork
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 12,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- readxl::read_excel("data/2026/AI Jobs Risk.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
df <- df_raw |>
  mutate(
    category = factor(category, levels = c("Highest Risk", "Lowest Risk")),
    # Shorten long occupation names for plot readability
    occupation_short = case_when(
      occupation == "Advertising, Marketing, Promotions, Public Relations, and Sales Managers" ~
        "Advertising & Marketing Managers",
      occupation == "Switchboard Operators, Including Answering Service" ~
        "Switchboard Operators",
      occupation == "Door-to-Door Sales Workers, News and Street Vendors, and Related Workers" ~
        "Door-to-Door Sales Workers",
      occupation == "Occupational Therapy and Physical Therapist Assistants and Aides" ~
        "OT & PT Assistants and Aides",
      occupation == "Substance Abuse, Behavioral Disorder, and Mental Health Counselors" ~
        "Substance Abuse & MH Counselors",
      occupation == "Supervisors of Office and Administrative Support Workers" ~
        "Office & Admin Support Supervisors",
      occupation == "Interviewers, Except Eligibility and Loan" ~
        "Interviewers (Excl. Eligibility)",
      occupation == "Dentists, All Other Specialists" ~ "Dentists",
      occupation == "Lawyers, Judges, and Related Workers" ~ "Lawyers & Judges",
      occupation == "Architectural and Engineering Managers" ~ "Architectural & Engineering Mgrs",
      occupation == "Artists and Related Workers, All Other" ~ "Artists & Related Workers",
      occupation == "Postsecondary Teachers, All Other" ~ "Postsecondary Teachers (Other)",
      TRUE ~ occupation
    )
  )

### |- Act 1 summary stats ----
act1_summary <- df |>
  group_by(category) |>
  summarise(
    avg_wage = mean(wage),
    total_emp = sum(employment),
    .groups = "drop"
  ) |>
  mutate(
    avg_wage_label = dollar(avg_wage, scale = 1 / 1000, suffix = "K", accuracy = 1)
  )

wage_gap <- act1_summary |>
  summarise(gap = diff(avg_wage)) |>
  pull(gap) |>
  abs() |>
  dollar(scale = 1 / 1000, suffix = "K", accuracy = 1)

### |- Act 2 scatter data ----
# Medians for quadrant lines
risk_median <- median(df$risk)
wage_median <- median(df$wage)

# Hero occupations: anchors for the narrative
heroes <- c(
  "Tellers",
  "Payroll and Timekeeping Clerks",
  "Dentists",
  "Engineers",
  "Advertising & Marketing Managers"
)

df_scatter <- df |>
  mutate(
    is_hero = occupation_short %in% heroes,
    hero_color = case_when(
      category == "Highest Risk" & is_hero ~ "hero_high",
      category == "Lowest Risk" & is_hero ~ "hero_low",
      category == "Highest Risk" ~ "field_high",
      TRUE ~ "field_low"
    )
  )

# Extract hero coordinates for annotate() callouts
hero_coords <- df_scatter |>
  filter(is_hero) |>
  select(occupation_short, risk, wage, employment, category)

### |- Act 3 employment bars (high-risk only) ----
df_act3 <- df |>
  filter(category == "Highest Risk") |>
  arrange(desc(employment)) |>
  mutate(
    occupation_short = fct_reorder(occupation_short, employment),
    emp_label = case_when(
      employment >= 1e6 ~ paste0(round(employment / 1e6, 1), "M"),
      employment >= 1e3 ~ paste0(round(employment / 1e3, 0), "K"),
      TRUE ~ as.character(employment)
    )
  )


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    col_high      = "#C0392B",   
    col_low       = "#2471A3",   
    col_hero_high = "#922B21",   
    col_hero_low  = "#1A5276",   
    col_field     = "#C8C8C8",   
    col_bg        = "#FAFAF8",
    col_text      = "#2D2D2D",
    col_grid      = "#E8E8E8",
    col_quad      = "#BBBBBB",   
    col_segment   = "gray55"     
  )
)

# Convenience aliases 
col_high      <- colors$palette$col_high
col_low       <- colors$palette$col_low
col_hero_high <- colors$palette$col_hero_high
col_hero_low  <- colors$palette$col_hero_low
col_field     <- colors$palette$col_field
col_bg        <- colors$palette$col_bg
col_text      <- colors$palette$col_text
col_grid      <- colors$palette$col_grid
col_quad      <- colors$palette$col_quad
col_segment   <- colors$palette$col_segment

### |- titles and caption ----
title_text    <- "AI Automation Risk Is a Low-Wage Problem"
subtitle_text <- "High-risk jobs pay less — and employ far more workers than rankings suggest"

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 16,
  source_text = "AI Exposure Index (aiexposure.org) | Note: Risk scores 0–100; wage = mean annual; employment = total US workers"
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- base theme ----
base_theme  <- create_base_theme(colors = list(background = col_bg, text = col_text))

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    plot.background  = element_rect(fill = col_bg, color = NA),
    panel.background = element_rect(fill = col_bg, color = NA),
    panel.grid.major = element_line(color = col_grid, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 8, color = "#666666"),
    axis.title = element_text(size = 8.5, color = col_text, face = "plain"),
    plot.margin = margin(8, 12, 8, 12)
  )
)

theme_set(weekly_theme)

### |- ACT 1: Wage gap headline bar ----
p1 <- act1_summary |>
  mutate(
    # More explicit category labels
    category_label = case_when(
      category == "Highest Risk" ~ "High AI Risk Jobs",
      category == "Lowest Risk" ~ "Low AI Risk Jobs"
    ),
    category_label = fct_rev(factor(category_label,
                                    levels = c("High AI Risk Jobs", "Low AI Risk Jobs")
    ))
  ) |>
  ggplot(aes(x = avg_wage, y = category_label, fill = category)) +
  # Geoms
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(
    aes(label = avg_wage_label),
    hjust = -0.2,
    size = 5,
    fontface = "bold",
    color = col_text,
    family = fonts$text
  ) +
  # Annotate
  annotate(
    "richtext",
    x = mean(act1_summary$avg_wage),
    y = 1.5,
    label = glue("<span style='color:#888888'>← &nbsp; <b>{wage_gap} gap</b> &nbsp; →</span>"),
    size = 3.0,
    fill = NA, label.color = NA,
    color = col_text
  ) +
  # Scales
  scale_x_continuous(
    labels  = dollar_format(scale = 1 / 1000, suffix = "K", accuracy = 1),
    expand  = expansion(mult = c(0, 0.18)),
    limits  = c(0, max(act1_summary$avg_wage) * 1.2)
  ) +
  scale_fill_manual(values = c("Highest Risk" = col_high, "Lowest Risk" = col_low)) +
  # Labs
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    x        = "Average annual wage",
    y        = NULL
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.3),
    plot.title = element_text(
      size = 15, face = "bold", color = col_text,
      family = fonts$title, margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 9, color = "#555555",
      family = fonts$subtitle, margin = margin(b = 10)
    )
  )


### |- ACT 2: Risk × Wage scatter ----
callout_df <- hero_coords |>
  mutate(
    lx = case_when(
      occupation_short == "Tellers" ~ risk - 18,
      occupation_short == "Payroll and Timekeeping Clerks" ~ risk - 20,
      occupation_short == "Dentists" ~ risk + 4,
      occupation_short == "Engineers" ~ risk - 18,
      occupation_short == "Advertising & Marketing Managers" ~ risk - 18,
      TRUE ~ risk
    ),
    ly = case_when(
      occupation_short == "Tellers" ~ wage + 16000,
      occupation_short == "Payroll and Timekeeping Clerks" ~ wage + 26000,
      occupation_short == "Dentists" ~ wage - 22000,
      occupation_short == "Engineers" ~ wage + 20000,
      occupation_short == "Advertising & Marketing Managers" ~ wage - 22000,
      TRUE ~ wage
    ),
    hjust_val = case_when(
      occupation_short %in% c(
        "Tellers", "Payroll and Timekeeping Clerks",
        "Engineers", "Advertising & Marketing Managers"
      ) ~ 1,
      TRUE ~ 0
    )
  )

p2 <- df_scatter |>
  ggplot(aes(x = risk, y = wage)) +
  
  # Geoms
  geom_vline(xintercept = risk_median, color = col_quad, linewidth = 0.4, linetype = "dashed") +
  geom_hline(yintercept = wage_median, color = col_quad, linewidth = 0.4, linetype = "dashed") +
  geom_point(
    data = filter(df_scatter, !is_hero),
    aes(size = employment),
    color = col_field, alpha = 0.6, show.legend = FALSE
  ) +
  geom_point(
    data = filter(df_scatter, is_hero),
    aes(size = employment),
    color = ifelse(
      filter(df_scatter, is_hero)$category == "Highest Risk", col_high, col_low
    ),
    alpha = 0.25, show.legend = FALSE,
    size = 14
  ) +
  geom_point(
    data = filter(df_scatter, is_hero),
    aes(
      size  = employment,
      color = category
    ),
    show.legend = FALSE
  ) +
  # Annotate
  annotate("text",
           x = 15, y = 215000, label = "Safe & well-paid",
           size = 2.5, color = "#AAAAAA", hjust = 0, family = fonts$text
  ) +
  annotate("text",
           x = 88, y = 215000, label = "Exposed & well-paid",
           size = 2.5, color = "#AAAAAA", hjust = 1, family = fonts$text
  ) +
  annotate("text",
           x = 15, y = 28000, label = "Safe & underpaid",
           size = 2.5, color = "#AAAAAA", hjust = 0, family = fonts$text
  ) +
  annotate("text",
           x = 88, y = 28000, label = "Exposed & underpaid",
           size = 2.5, color = "#AAAAAA", hjust = 1, family = fonts$text
  ) +
  annotate("segment",
           x = callout_df$risk, xend = callout_df$lx,
           y = callout_df$wage, yend = callout_df$ly,
           color = col_segment, linewidth = 0.35
  ) +
  annotate("richtext",
           x = callout_df$lx,
           y = callout_df$ly,
           label = glue::glue(
             "<span style='font-size:7pt'><b>{callout_df$occupation_short}</b><br>",
             "Risk: {callout_df$risk} · {dollar(callout_df$wage, scale=1/1000, suffix='K', accuracy=1)}</span>"
           ),
           hjust = callout_df$hjust_val,
           fill = NA, label.color = NA,
           size = 2.5,
           color = col_text
  ) +
  # Scales
  scale_color_manual(values = c("Highest Risk" = col_high, "Lowest Risk" = col_low)) +
  scale_size_area(max_size = 9) +
  scale_y_continuous(
    labels = dollar_format(scale = 1 / 1000, suffix = "K", accuracy = 1),
    limits = c(25000, 240000)
  ) +
  scale_x_continuous(limits = c(10, 100)) +
  # Labs
  labs(
    x = "AI automation risk score",
    y = "Annual wage",
    title = "Risk vs. Wage",
    subtitle = "Bubble size = workers employed · Dashed lines = group medians"
  ) +
  # Theme
  theme(
    plot.title = element_text(size = 10, face = "bold", color = col_text, family = fonts$title),
    plot.subtitle = element_text(
      size = 7.5, color = "#777777", family = fonts$subtitle,
      margin = margin(b = 6)
    )
  )


### |- ACT 3: Employment concentration bars ----
p3 <- df_act3 |>
  ggplot(aes(x = employment, y = occupation_short, fill = risk)) +
  # Geoms
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(
    aes(label = emp_label),
    hjust = -0.15,
    size = 2.6,
    color = col_text,
    family = fonts$text
  ) +
  # Annotate
  annotate(
    "richtext",
    x = max(df_act3$employment) * 0.98,
    y = nlevels(df_act3$occupation_short) - 0.3,
    label = "<span style='font-size:6.5pt; color:#922B21'>▲ Largest exposure</span>",
    hjust = 1, fill = NA, label.color = NA
  ) +
  # Scales
  scale_x_continuous(
    labels = label_comma(scale = 1 / 1000, suffix = "K"),
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_fill_gradient(
    low    = "#F5C6C0",
    high   = col_high,
    limits = c(80, 96)
  ) +
  # Labs
  labs(
    x = "Workers (thousands)",
    y = NULL,
    title = "Worker Concentration",
    subtitle = "High-risk occupations · sorted by employment · color = risk score"
  ) +
  # Theme
  theme(
    axis.text.y = element_text(size = 6.8, color = col_text, hjust = 1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = col_grid, linewidth = 0.3),
    plot.title = element_text(size = 10, face = "bold", color = col_text, family = fonts$title),
    plot.subtitle = element_text(
      size = 7.5, color = "#777777", family = fonts$subtitle,
      margin = margin(b = 6)
    )
  )


### |- Combine plots ----
layout <- "
AAAA
BBCC
"

p_final <- p1 + p2 + p3 +
  plot_layout(
    design    = layout,
    heights   = c(0.8, 1.6),
    widths    = c(1.25, 0.9)  
  ) +
  plot_annotation(
    caption = caption_text,
    theme   = theme(
      plot.background = element_rect(fill = col_bg, color = NA),
      plot.caption    = element_markdown(
        size    = 6.5,
        color   = "#999999",
        hjust   = 0,
        margin  = margin(t = 8),
        family = fonts$captions
      )
    )
  )

### |- Preview ----
snap(p_final)


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
 
# ─ Session info ──────────────────────────────────────
# setting  value
# version  R version 4.5.3 (2026-03-11 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-04-20
# rstudio  2026.01.2+418 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────
# ! package      * version  date (UTC) lib source
# base         * 4.5.3    2026-03-11 [?] local
# base64enc      0.1-6    2026-02-02 [1] CRAN (R 4.5.2)
# camcorder      0.1.0    2022-10-03 [1] CRAN (R 4.5.3)
# cellranger     1.1.0    2016-07-27 [1] CRAN (R 4.5.3)
# cli            3.6.6    2026-04-09 [1] CRAN (R 4.5.3)
# commonmark     2.0.0    2025-07-07 [1] CRAN (R 4.5.3)
# P compiler       4.5.3    2026-03-11 [2] local
# curl           7.0.0    2025-08-19 [1] CRAN (R 4.5.3)
# P datasets     * 4.5.3    2026-03-11 [2] local
# digest         0.6.39   2025-11-19 [1] CRAN (R 4.5.3)
# dplyr        * 1.2.1    2026-04-03 [1] CRAN (R 4.5.3)
# evaluate       1.0.5    2025-08-27 [1] CRAN (R 4.5.3)
# farver         2.1.2    2024-05-13 [1] CRAN (R 4.5.3)
# fastmap        1.2.0    2024-05-15 [1] CRAN (R 4.5.3)
# forcats      * 1.0.1    2025-09-25 [1] CRAN (R 4.5.3)
# generics       0.1.4    2025-05-09 [1] CRAN (R 4.5.3)
# ggplot2      * 4.0.2    2026-02-03 [1] CRAN (R 4.5.3)
# ggtext       * 0.1.2    2022-09-16 [1] CRAN (R 4.5.3)
# gifski         1.32.0-2 2025-03-18 [1] CRAN (R 4.5.3)
# glue         * 1.8.0    2024-09-30 [1] CRAN (R 4.5.3)
# P graphics     * 4.5.3    2026-03-11 [2] local
# P grDevices    * 4.5.3    2026-03-11 [2] local
# P grid           4.5.3    2026-03-11 [2] local
# gridtext       0.1.6    2026-02-19 [1] CRAN (R 4.5.3)
# gtable         0.3.6    2024-10-25 [1] CRAN (R 4.5.3)
# here         * 1.0.2    2025-09-15 [1] CRAN (R 4.5.3)
# hms            1.1.4    2025-10-17 [1] CRAN (R 4.5.3)
# htmltools      0.5.9    2025-12-04 [1] CRAN (R 4.5.3)
# janitor      * 2.2.1    2024-12-22 [1] CRAN (R 4.5.3)
# jsonlite       2.0.0    2025-03-27 [1] CRAN (R 4.5.3)
# knitr          1.51     2025-12-20 [1] CRAN (R 4.5.3)
# labeling       0.4.3    2023-08-29 [1] CRAN (R 4.5.2)
# lifecycle      1.0.5    2026-01-08 [1] CRAN (R 4.5.3)
# litedown       0.9      2025-12-18 [1] CRAN (R 4.5.3)
# lubridate    * 1.9.5    2026-02-04 [1] CRAN (R 4.5.3)
# magick         2.9.1    2026-02-28 [1] CRAN (R 4.5.3)
# magrittr       2.0.5    2026-04-04 [1] CRAN (R 4.5.3)
# markdown       2.0      2025-03-23 [1] CRAN (R 4.5.3)
# P methods      * 4.5.3    2026-03-11 [2] local
# otel           0.2.0    2025-08-29 [1] CRAN (R 4.5.3)
# pacman       * 0.5.1    2019-03-11 [1] CRAN (R 4.5.3)
# patchwork    * 1.3.2    2025-08-25 [1] CRAN (R 4.5.3)
# pillar         1.11.1   2025-09-17 [1] CRAN (R 4.5.3)
# pkgconfig      2.0.3    2019-09-22 [1] CRAN (R 4.5.3)
# purrr        * 1.2.2    2026-04-10 [1] CRAN (R 4.5.3)
# R6             2.6.1    2025-02-15 [1] CRAN (R 4.5.3)
# ragg           1.5.2    2026-03-23 [1] CRAN (R 4.5.3)
# RColorBrewer   1.1-3    2022-04-03 [1] CRAN (R 4.5.2)
# Rcpp           1.1.1    2026-01-10 [1] CRAN (R 4.5.3)
# readr        * 2.2.0    2026-02-19 [1] CRAN (R 4.5.3)
# readxl         1.4.5    2025-03-07 [1] CRAN (R 4.5.3)
# repr           1.1.7    2024-03-22 [1] CRAN (R 4.5.3)
# rlang          1.2.0    2026-04-06 [1] CRAN (R 4.5.3)
# rprojroot      2.1.1    2025-08-26 [1] CRAN (R 4.5.3)
# rstudioapi     0.18.0   2026-01-16 [1] CRAN (R 4.5.3)
# rsvg           2.7.0    2025-09-08 [1] CRAN (R 4.5.3)
# S7             0.2.1    2025-11-14 [1] CRAN (R 4.5.3)
# scales       * 1.4.0    2025-04-24 [1] CRAN (R 4.5.3)
# sessioninfo    1.2.3    2025-02-05 [1] CRAN (R 4.5.3)
# showtext     * 0.9-8    2026-03-21 [1] CRAN (R 4.5.3)
# showtextdb   * 3.0      2020-06-04 [1] CRAN (R 4.5.3)
# skimr          2.2.2    2026-01-10 [1] CRAN (R 4.5.3)
# snakecase      0.11.1   2023-08-27 [1] CRAN (R 4.5.3)
# P stats        * 4.5.3    2026-03-11 [2] local
# stringi        1.8.7    2025-03-27 [1] CRAN (R 4.5.2)
# stringr      * 1.6.0    2025-11-04 [1] CRAN (R 4.5.3)
# svglite        2.2.2    2025-10-21 [1] CRAN (R 4.5.3)
# sysfonts     * 0.8.9    2024-03-02 [1] CRAN (R 4.5.3)
# systemfonts    1.3.2    2026-03-05 [1] CRAN (R 4.5.3)
# textshaping    1.0.5    2026-03-06 [1] CRAN (R 4.5.3)
# tibble       * 3.3.1    2026-01-11 [1] CRAN (R 4.5.3)
# tidyr        * 1.3.2    2025-12-19 [1] CRAN (R 4.5.3)
# tidyselect     1.2.1    2024-03-11 [1] CRAN (R 4.5.3)
# tidyverse    * 2.0.0    2023-02-22 [1] CRAN (R 4.5.3)
# timechange     0.4.0    2026-01-29 [1] CRAN (R 4.5.3)
# P tools          4.5.3    2026-03-11 [2] local
# tzdb           0.5.0    2025-03-15 [1] CRAN (R 4.5.3)
# utf8           1.2.6    2025-06-08 [1] CRAN (R 4.5.3)
# P utils        * 4.5.3    2026-03-11 [2] local
# vctrs          0.7.3    2026-04-11 [1] CRAN (R 4.5.3)
# withr          3.0.2    2024-10-28 [1] CRAN (R 4.5.3)
# xfun           0.57     2026-03-20 [1] CRAN (R 4.5.3)
# xml2           1.5.2    2026-01-17 [1] CRAN (R 4.5.3)
# 
# [1] C:/Users/poncest/AppData/Local/R/win-library/4.5
# [2] C:/Program Files/R/R-4.5.3/library
# 
# * ── Packages attached to the search path.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────