
## Challenge: #MakeoverMondnay 2025 week 32
## Data:      Corruption Perceptions Index (CPI)
## Author:    Steven Ponce
## Date:      2025-07-29

## Article
# https://ourworldindata.org/corruption

## Data
# https://data.world/makeovermonday/corruption

## Original Chart
# https://ourworldindata.org/grapher/ti-corruption-perception-index?tab=table&time=earliest..2024&country=~NZL


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,      # Easily Install and Load the 'Tidyverse'
  ggtext,         # Improved Text Rendering Support for 'ggplot2'
  showtext,       # Using Fonts More Easily in R Graphs
  scales,         # Scale Functions for Visualization
  glue,           # Interpreted String Literals
  ggbeeswarm,     # Categorical Scatter (Violin Point) Plots
  patchwork       # The Composer of Plots
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
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
cpi_raw <- readxl::read_excel("data/2025/Corruption Perception Index.xlsx") |>
  janitor::clean_names()


## 3. EXAMINING THE DATA ----
glimpse(cpi_raw)
skimr::skim(cpi_raw)


## 4. TIDYDATA ----
# Data cleaning and preparation
cpi_clean <- cpi_raw |>
  filter(!is.na(corruption_perceptions_index)) |>
  filter(year >= 2012, year <= 2024)

# Helper function to assign World Bank income groups (FY2024 classification)
create_income_groups <- function(data) {
  data |>
    mutate(
      income_group = case_when(
        # ---- High Income Countries (based on WB FY2024) ----
        code %in% c(
          # Western Europe & North America
          "DNK", "NOR", "SWE", "CHE", "SGP", "NZL", "FIN", "DEU", "NLD",
          "LUX", "AUT", "AUS", "CAN", "USA", "GBR", "BEL", "JPN", "FRA",
          "IRL", "ISL", "ESP", "ITA", "KOR", "ISR", 
          # Eastern Europe (High Income)
          "SVN", "CZE", "EST", "SVK", "PRT", "POL", "LTU", "LVA", "HUN", 
          "HRV", "GRC", "CYP", "MLT",
          # Other High Income
          "URY", "CHL", "PAN", "ARE", "QAT", "KWT", "BHR", "OMN",
          "SAU", "BRB", "TTO"
        ) ~ "High\nIncome",
        
        # ---- Upper Middle Income ----
        code %in% c(
          # Latin America
          "ARG", "MYS", "THA", "MEX", "CRI", "BRA", "COL", "PER", "ECU", "DOM",
          # Europe & Central Asia
          "ROU", "BGR", "TUR", "MNE", "SRB", "MKD", "ALB", "BIH", "ARM", "GEO", 
          "AZE", "KAZ", "BLR",
          # Other Upper Middle
          "CHN", "RUS", "ZAF", "BWA", "NAM", "JAM", "LBN", "JOR", "TUN", 
          "DZA", "IRQ", "IRN", "LBY", "GAB"
        ) ~ "Upper Middle\nIncome",
        
        # ---- Lower Middle Income ----
        code %in% c(
          # South & East Asia
          "IND", "IDN", "PHL", "VNM", "BGD", "PAK", "LKA", "NPL", "KHM",
          "LAO", "MMR", "MNG",
          # Europe & Central Asia
          "UZB", "KGZ", "TJK", "UKR", "MDA", 
          # Middle East & North Africa
          "MAR", "EGY", "PSE", "SYR", "YEM", "SDN", "DJI",
          # Latin America
          "BOL", "PRY", "GTM", "HND", "NIC", "SLV", "GUY", "SUR",
          # Sub-Saharan Africa
          "KEN", "TZA", "UGA", "RWA", "ZMB", "AGO", "CIV", "GHA", 
          "CMR", "SEN", "NGA", "BEN"
        ) ~ "Lower Middle\nIncome",
        
        # ---- Default: Low Income ----
        TRUE ~ "Low\nIncome"
      ),
      # Create ordered factor for logical display
      income_group = factor(
        income_group,
        levels = c("Low\nIncome", "Lower Middle\nIncome", 
                   "Upper Middle\nIncome", "High\nIncome"),
        ordered = TRUE
      )
    )
}

# Apply income classification
cpi_with_income <- cpi_clean |>
  create_income_groups()

# Calculate country trends for dumbbell chart
country_trends <- cpi_with_income |>
  group_by(entity) |>
  filter(any(year == 2012) & any(year == 2024)) |>
  summarise(
    trend = corruption_perceptions_index[year == 2024][1] - corruption_perceptions_index[year == 2012][1],
    latest_score = corruption_perceptions_index[year == 2024][1],
    earliest_score = corruption_perceptions_index[year == 2012][1],
    income_group = first(income_group),
    .groups = "drop"
  ) |>
  filter(!is.na(trend), !is.na(latest_score), !is.na(earliest_score))

# Beeswarm plot data
latest_data_income <- cpi_with_income |>
  filter(year == 2024) |>
  arrange(desc(corruption_perceptions_index))

# Dumbbell plot data
biggest_movers <- country_trends |>
  filter(abs(trend) >= 5) |>
  arrange(desc(trend)) |>
  slice_head(n = 20) |>
  mutate(
    income_abbrev = case_when(
      income_group == "High\nIncome" ~ "HI",
      income_group == "Upper Middle\nIncome" ~ "UMI",
      income_group == "Lower Middle\nIncome" ~ "LMI",
      income_group == "Low\nIncome" ~ "LI",
      TRUE ~ "Unknown"
    ),
    entity_with_income = glue("{entity} ({income_abbrev})"),
    entity_short = if_else(
      nchar(entity_with_income) > 25,
      glue("{str_sub(entity, 1, 18)}... ({income_abbrev})"),
      entity_with_income
    ),
    entity_short = fct_reorder(entity_short, latest_score)
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  "Low\nIncome" = "#2166AC",
  "Lower Middle\nIncome" = "#5AAE61",
  "Upper Middle\nIncome" = "#9970AB",
  "High\nIncome" = "#E08214"
))

### |-  titles and caption ----
title_text <- str_glue("The economics of corruption perceptions: structure vs change, 2012-2024")

subtitle_text <- str_glue(
  "How economic development shapes corruption perceptions, yet dramatic improvements remain possible"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 32,
  source_text = "Our World in Data"
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
    plot.subtitle = element_text(
      size = rel(0.9), hjust = 0, family = fonts$subtitle,
      color = alpha(colors$subtitle, 0.9), lineheight = 1.1,
      margin = margin(t = 0, b = 20)
    ),

    # Legend formatting
    legend.position = "plot",
    legend.box.margin = margin(b = 10),
    legend.margin = margin(b = 5),
    legend.title = element_text(face = "bold"),

    # Axis formatting
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "gray", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
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
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
    panel.grid.minor = element_blank(),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1  Income beeswarm plot----
beeswarm_plot <- ggplot(
  latest_data_income,
  aes(
    x = income_group, y = corruption_perceptions_index,
    color = income_group
  )
) +
  # Geoms
  geom_quasirandom(size = 2.5, alpha = 0.75, width = 0.35) +
  stat_summary(
    fun = median, geom = "crossbar", width = 0.6,
    color = "black", size = 1, alpha = 0.8
  ) +
  # Scales
  scale_color_manual(values = colors$palette, guide = "none") +
  scale_y_continuous(
    breaks = seq(0, 100, 20), limits = c(0, 100),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  coord_flip(clip = "off") +
  # Labs
  labs(
    title = "Wealth strongly predicts clean governance",
    subtitle = str_glue(
      "2024 Corruption perceptions index by income level\n",
      "Each dot represents one country | Black bars show group medians"
    ),
    x = NULL,
    y = "Corruption Perceptions Index (0 = Most Corrupt, 100 = Least Corrupt)",
  ) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray95", size = 0.25),
    axis.line.x = element_line(color = "gray30", size = 0.5),
  )
### |- P2  Dumbbell plot----
dumbbell_plot <- ggplot(biggest_movers, aes(y = entity_short)) +
  # Geoms
  geom_segment(aes(x = earliest_score, xend = latest_score),
               color = "gray60", size = 1.2, alpha = 0.8
  ) +
  geom_point(aes(x = earliest_score), color = "gray40", size = 3.5, alpha = 0.9) +
  geom_point(aes(x = latest_score, color = income_group), size = 3.5, alpha = 0.9) +
  geom_text(
    aes(
      x = pmax(earliest_score, latest_score) + 3,
      label = paste0(ifelse(trend > 0, "+", ""), round(trend))
    ),
    size = 3, fontface = "bold", color = "gray30", hjust = 0.1
  ) +
  # Scales
  scale_color_manual(values = colors$palette, guide = "none") +
  scale_x_continuous(
    breaks = seq(0, 100, 20), limits = c(0, 105),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  # Labs
  labs(
    title = "Yet dramatic improvements are possible",
    subtitle = str_glue(
      "Largest changes in corruption perceptions (2012–2024)\n",
      "Gray dots: 2012 scores | Colored dots: 2024 scores (by income group)"
    ),
    x = "Corruption Perceptions Index",
    y = NULL,
  ) +
  # Theme
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "gray30", size = 0.5),
  )

### |-  combined plot ----
combined_plots <- beeswarm_plot | dumbbell_plot

combined_plots +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        size = rel(1.7),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.1,
        margin = margin(t = 5, b = 5)
      ),
      plot.subtitle = element_markdown(
        size = rel(1.1),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 0.9,
        margin = margin(t = 5, b = 0)
      ),
      plot.caption = element_markdown(
        size = rel(0.75),
        family = fonts$caption,
        color = colors$caption,
        hjust = 0.5,
        margin = margin(t = 10)
      )
    )
  )


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ─────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-05
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ─────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3    2015-07-28 [1] RSPM (R 4.4.0)
# P beeswarm      0.4.0    2021-06-01 [?] CRAN (R 4.4.0)
# camcorder     0.1.0    2022-10-03 [1] RSPM (R 4.4.0)
# cellranger    1.1.0    2016-07-27 [1] RSPM (R 4.4.0)
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
# P ggbeeswarm  * 0.7.2    2023-04-29 [?] CRAN (R 4.4.0)
# ggplot2     * 3.5.1    2024-04-23 [1] RSPM (R 4.4.0)
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
# P vipor         0.4.7    2023-12-18 [?] CRAN (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ────────────────────────────────────────────────────────────────────────
# > 
