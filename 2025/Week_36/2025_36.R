## Challenge: #MakeoverMondnay 2025 week 36
## Data:      Drug Harms in the UK
## Author:    Steven Ponce
## Date:      2025-09-01

## Article
# https://www.economist.com/graphic-detail/2019/06/25/what-is-the-most-dangerous-drug

## Data
# https://data.world/makeovermonday/2025w36-drug-harms-in-the-uk

## Original Chart
# https://www.economist.com/graphic-detail/2019/06/25/what-is-the-most-dangerous-drug

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Easily Install and Load the 'Tidyverse'
  ggtext,     # Improved Text Rendering Support for 'ggplot2'
  showtext,   # Using Fonts More Easily in R Graphs
  scales,     # Scale Functions for Visualization
  glue,       # Interpreted String Literals
  patchwork   # The Composer of Plots
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 8,
  units  = "in",
  dpi    = 300
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
drug_harm_raw <- read_csv("data/2025/Drug Harm MCDA - drug_harms_mcda.csv") |> 
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(drug_harm_raw)
skimr::skim_without_charts(drug_harm_raw)


## 4. TIDYDATA ----
drug_totals_actual <- drug_harm_raw |>
  filter(group %in% c("Harm to users", "Harm to others")) |>
  select(-c(category, criteria)) |>
  pivot_longer(cols = -group, names_to = "drug", values_to = "harm_score") |>
  group_by(drug, group) |>
  summarise(harm = sum(harm_score, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = harm) |>
  janitor::clean_names() |>
  mutate(total_harm = harm_to_users + harm_to_others) |>
  arrange(desc(total_harm))

# Prepare data for diverging bars
diverging_data <- drug_totals_actual |>
  mutate(
    drug_clean = case_when(
      drug == "crack" ~ "Crack cocaine",
      drug == "methylamphet" ~ "Methamphetamine",
      drug == "ghb" ~ "GHB",
      drug == "anabolic_steroids" ~ "Anabolic steroids",
      drug == "lsd" ~ "LSD",
      TRUE ~ str_to_title(drug)
    ),
    harm_to_others_neg = -harm_to_others,
    drug_clean = fct_reorder(drug_clean, total_harm)
  ) |>
  select(drug_clean, harm_to_users, harm_to_others_neg) |>
  pivot_longer(
    cols = c(harm_to_users, harm_to_others_neg),
    names_to = "harm_type", values_to = "harm_value"
  ) |>
  mutate(
    harm_type = case_when(
      harm_type == "harm_to_users" ~ "Harm to users",
      harm_type == "harm_to_others_neg" ~ "Harm to others"
    )
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = c(
  "Harm to users" = "#2E6F77",      
  "Harm to others" = "#D97B66"   
))   


### |-  titles and caption ----
title_text <- str_glue("Ranking Drug Harms in the UK")

subtitle_text <- str_glue(
  "**Harm scores** based on a multi-criteria decision analysis by a panel of experts,<br>",
  "ranking drugs from 0 to 100 on **16 distinct criteria**.<br><br>",
  "Harm to others (left) vs harm to users (right) • weighted scores • UK, 2010 • small panel shows total harm"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 36,
  source_text = "Nutt et al. (2010),<br>The Lancet – Drug harms in the UK: a multicriteria decision analysis"
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
    plot.title = element_text(
      size = rel(1.4), family = fonts$title, face = "bold",
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

### |- p1: diverging bar chart ----
p1 <- ggplot(diverging_data, aes(x = drug_clean, y = harm_value, fill = harm_type)) +
  # Geoms
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_text(
    data = diverging_data %>% filter(harm_value > 0), aes(label = round(harm_value, 1)),
    hjust = -0.5,
    size = 3
  ) +
  geom_text(
    data = diverging_data %>% filter(harm_value < 0), aes(label = abs(round(harm_value, 1))),
    hjust = 1.5,
    size = 3
  ) +
  # Annotations
  annotate("text",
    x = 1.5, y = 15, label = "Harm to users →",
    hjust = 0, color = colors$palette[1], fontface = "bold", size = 4
  ) +
  annotate("text",
    x = 1.5, y = -15, label = "← Harm to others",
    hjust = 1, color = colors$palette[2], fontface = "bold", size = 4
  ) +
  # Scales
  scale_fill_manual(values = colors$palette) +
  scale_y_continuous(
    labels = abs,
    breaks = pretty(c(-50, 40), n = 10),
    limits = c(-50, 40)
  ) +
  coord_flip(clip = "off") +
  # Labs
  labs(
    x = NULL,
    y = "Harm Score",
    fill = NULL
  ) +
  # Theme
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    # bold top-3
    axis.text.y = element_text(face = ifelse(levels(diverging_data$drug_clean) %in%
      tail(levels(diverging_data$drug_clean), 3),
    "bold", "plain"
    )),
    axis.text.x = element_blank()
  )

### |- p2: total harm bar chart ----
p_total <- ggplot(drug_totals_actual, aes(x = fct_reorder(drug, total_harm, .desc = FALSE), y = total_harm)) +
  # Geoms
  geom_col(width = 0.7, fill = "gray50") +
  geom_text(aes(label = round(total_harm, 1), y = total_harm + 2), hjust = 0, size = 3) +
  # Scales
  scale_y_continuous(
    limits = c(0, max(drug_totals_actual$total_harm) + 5)
  ) +
  coord_flip(clip = "off") +
  # Labs
  labs(
    x = NULL,
    y = "Total Harm"
  ) +
  # Theme
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.margin = margin(l = -10, r = 5)
  )

### |- final combined plot ----
combined_plots <- p1 + p_total +
  plot_layout(widths = c(2, 0.5))

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.8),
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

# ─ Session info ────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-09-02
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────
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
# ───────────────────────────────────────────────────────────────────────────
# > 
