## Challenge: #MakeoverMondnay 2025 week 34
## Data:      UK Unemployment Rate
## Author:    Steven Ponce
## Date:      2025-08-19

## Article
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms?utm_source=chatgpt.com

## Data
# https://data.world/makeovermonday/2025-week-34-unemployment-rate

## Original Chart
# https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms?utm_source=chatgpt.com

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
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
uk_unemployment_raw <- read_csv("data/2025/UK_unemployment rate_adjusted.csv", 
         skip = 7,
         col_names = c("date", "percent"),
         show_col_types = FALSE) |>  
  slice(-1)


## 3. EXAMINING THE DATA ----
glimpse(uk_unemployment_raw)
skimr::skim(uk_unemployment_raw)


## 4. TIDYDATA ----

# helper function for date parsing
parse_uk_dates <- function(date_str) {
  result <- rep(as.Date(NA), length(date_str))
  
  # Case 1: Year only (e.g., "1971")
  year_only <- str_detect(date_str, "^\\d{4}$")
  if(any(year_only, na.rm = TRUE)) {
    result[year_only] <- as.Date(paste0(date_str[year_only], "-01-01"))
  }
  
  # Case 2: Quarter format (e.g., "1971 Q1")
  quarter_format <- str_detect(date_str, "^\\d{4}\\s+Q[1-4]$")
  if(any(quarter_format, na.rm = TRUE)) {
    for(i in which(quarter_format)) {
      year <- str_extract(date_str[i], "^\\d{4}")
      quarter_num <- as.numeric(str_extract(date_str[i], "(?<=Q)\\d"))
      month <- (quarter_num - 1) * 3 + 1
      result[i] <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
    }
  }
  
  # Case 3: Month-Year formats (various separators)
  month_year <- str_detect(date_str, "[A-Z]{3}")
  if(any(month_year, na.rm = TRUE)) {
    for(i in which(month_year)) {
      if(str_detect(date_str[i], "^[A-Z]{3}-\\d{4}$")) {
        # Format: JAN-1971
        parts <- str_split(date_str[i], "-")[[1]]
        month_abbr <- parts[1]
        year <- parts[2]
      } else if(str_detect(date_str[i], "^\\d{4}-[A-Z]{3}$")) {
        # Format: 1971-JAN  
        parts <- str_split(date_str[i], "-")[[1]]
        year <- parts[1]
        month_abbr <- parts[2]
      } else if(str_detect(date_str[i], "^[A-Z]{3}\\s+\\d{4}$")) {
        # Format: JAN 1971
        parts <- str_split(date_str[i], "\\s+")[[1]]
        month_abbr <- parts[1]
        year <- parts[2]
      } else if(str_detect(date_str[i], "^\\d{4}\\s+[A-Z]{3}$")) {
        # Format: 1971 JAN
        parts <- str_split(date_str[i], "\\s+")[[1]]
        year <- parts[1]
        month_abbr <- parts[2]
      } else {
        next
      }
      
      month_num <- match(month_abbr, toupper(month.abb))
      if(!is.na(month_num)) {
        result[i] <- as.Date(paste0(year, "-", sprintf("%02d", month_num), "-01"))
      }
    }
  }
  
  return(result)
}

# Tidy
uk_unemployment_tidy <- uk_unemployment_raw |>
  mutate(
    # Parse dates and create time variables
    date = parse_uk_dates(date),
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    quarter = paste0("Q", quarter(date, with_year = FALSE)),
    decade = paste0(floor(year / 10) * 10, "s"),

    # Clean unemployment rate
    unemployment_rate = percent,

    # Add data frequency indicator
    data_frequency = case_when(
      str_detect(uk_unemployment_raw$date, "^\\d{4}$") ~ "Annual",
      str_detect(uk_unemployment_raw$date, "Q") ~ "Quarterly",
      str_detect(uk_unemployment_raw$date, "[A-Z]{3}") ~ "Monthly",
      TRUE ~ "Other"
    ),

    # Add economic periods for context
    economic_period = case_when(
      year >= 1971 & year <= 1979 ~ "1970s Oil Crisis Era",
      year >= 1980 & year <= 1989 ~ "1980s Recession & Recovery",
      year >= 1990 & year <= 1999 ~ "1990s Recession & Growth",
      year >= 2000 & year <= 2007 ~ "2000s Pre-Financial Crisis",
      year >= 2008 & year <= 2015 ~ "2008 Financial Crisis & Recovery",
      year >= 2016 & year <= 2019 ~ "Post-Brexit Vote Era",
      year >= 2020 ~ "COVID-19 Era",
      TRUE ~ "Other"
    ),

    # Flag recession periods (approximate UK recessions)
    recession_period = case_when(
      (year >= 1974 & year <= 1975) ~ "1974-75 Recession",
      (year >= 1980 & year <= 1981) ~ "1980-81 Recession",
      (year >= 1990 & year <= 1991) ~ "1990-91 Recession",
      (year >= 2008 & year <= 2009) ~ "2008-09 Financial Crisis",
      (year >= 2020 & year <= 2021) ~ "2020-21 COVID Recession",
      TRUE ~ "Non-recession"
    )
  ) |>
  # Keep only successfully parsed dates
  filter(!is.na(date)) |>
  arrange(date) |>
  # Keep only essential columns
  select(
    date, year, month, quarter, decade, unemployment_rate,
    data_frequency, economic_period, recession_period
  )

# Year-over-year data
yoy_data <- uk_unemployment_tidy |>
  arrange(date) |>
  mutate(
    yoy_change = unemployment_rate - lag(unemployment_rate, 12),
    change_direction = ifelse(yoy_change > 0, "Rising Unemployment", "Falling Unemployment")
  ) |>
  filter(!is.na(yoy_change), year >= 1975) |>
  mutate(
    decade_group = paste0(floor(year / 10) * 10, "s"),
    # Create better decade labels
    decade_label = case_when(
      decade_group == "1970s" ~ "1970s\nOil Crisis",
      decade_group == "1980s" ~ "1980s\nThatcher Era",
      decade_group == "1990s" ~ "1990s\nPost-Recession",
      decade_group == "2000s" ~ "2000s\nPre-Crisis",
      decade_group == "2010s" ~ "2010s\nAusterity",
      decade_group == "2020s" ~ "2020s\nCOVID Era",
      TRUE ~ decade_group
    ),
    # Flag extreme changes
    extreme_change = abs(yoy_change) > 2
  )

# Data for annotations (YoY)
annotation_data <- yoy_data |>
  group_by(decade_label) |>
  summarise(
    date = min(date),
    .groups = "drop"
  ) |>
  mutate(
    date = if_else(str_detect(decade_label, "2020s"), date + months(12), date)
  )

# Dumbbell data (min/max)
dumbbell_data <- uk_unemployment_tidy |>
  group_by(decade) |>
  summarise(
    min_rate = min(unemployment_rate),
    max_rate = max(unemployment_rate),
    range_width = max_rate - min_rate,
    .groups = "drop"
  ) |>
  # Better decade labels 
  mutate(
    decade_label = case_when(
      decade == "1970s" ~ "1970s",
      decade == "1980s" ~ "1980s",
      decade == "1990s" ~ "1990s",
      decade == "2000s" ~ "2000s",
      decade == "2010s" ~ "2010s",
      decade == "2020s" ~ "2020s*",
      TRUE ~ decade
    )
  ) |>
  filter(decade != "2020s") # Remove incomplete decade


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
# Get base colors with custom palette
colors <- get_theme_colors(palette = list(
  "#00a8f3", "#061c2c", "#2d2d2d","#6b6b6b"
))   

### |-  titles and caption ----
title_text <- str_glue("UK Unemployment: Five Decades of Volatility and Range")

subtitle_text <- str_glue(
  "A comprehensive view of when unemployment changed most dramatically and which decades were most stable"
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 34,
  source_text = "UK Office for National Statistics"
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
    panel.grid.minor = element_line(color = "#ecf0f1", linewidth = 0.2),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),

    # Margin
    plot.margin = margin(20, 20, 20, 20)
  )
)

# Set theme
theme_set(weekly_theme)

### |- P1  YoY bar chart ----
p1 <- ggplot(yoy_data, aes(x = date, y = yoy_change)) +
  # Geoms
  geom_text(
    data = annotation_data, # annotations for the decade labels
    aes(x = date, label = decade_label),
    y = 5,
    hjust = 0,
    vjust = 0,
    size = 3.1,
    family = fonts$text,
    color = colors$palette[3],
    fontface = "bold",
    lineheight = 1.2
  ) +
  geom_hline(yintercept = 0, color = 'gray', linetype = "solid", size = 0.4) +
  geom_col(aes(fill = change_direction), alpha = 0.85, width = 30, position = "identity") +
  geom_point( # Highlight extreme changes
    data = yoy_data |> filter(extreme_change),
    aes(color = change_direction), 
    size = 1.2, 
    alpha = 0.9
  ) +
  # Scales
  scale_fill_manual(
    values = c(
      "Rising Unemployment" = colors$palette[2],
      "Falling Unemployment" = colors$palette[1]
    ),
    name = "Unemployment",
    labels = c("Falling", "Rising")
  ) +
  scale_color_manual(
    values = c(
      "Rising Unemployment" = colors$palette[2],
      "Falling Unemployment" = colors$palette[1]
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "pp"),
    breaks = seq(-2,3, by = 1),
  ) +
  scale_x_date(
    breaks = date_breaks("5 years"),
    labels = date_format("%Y")
  ) +
  # Labs
  labs(
    title = "Unemployment Volatility Across Five Decades",
    subtitle = str_glue(
    "Year-over-year changes reveal when unemployment shifted most dramatically<br>
    <span style='color:{colors$palette[2]}'>**Rising unemployment**</span> periods vs 
    <span style='color:{colors$palette[1]}'>**falling unemployment**</span> periods • 
    Dots show changes >2 percentage points"
    ),
    x = NULL,
    y = "Annual Change (percentage points)",
  )

### |- P2  dumbbell chart----
p2 <- ggplot(dumbbell_data, aes(y = fct_reorder(decade_label, range_width))) +
  # Geoms
  geom_segment(
    aes(x = min_rate, xend = max_rate, yend = decade_label),
    color = 'gray', 
    linewidth = 0.35,
    alpha = 0.6
  ) +
  geom_point( # Minimum points
    aes(x = min_rate), 
    color = colors$palette[1], 
    size = 2,
    alpha = 0.9
  ) +
  geom_point( # Maximum points
    aes(x = max_rate), 
    color = colors$palette[2], 
    size = 2,
    alpha = 0.9
  ) +
  geom_text(
    aes(x = min_rate, label = paste0(round(min_rate, 1), "%")),
    nudge_x = -0.45, 
    size = 3.8, 
    color = colors$palette[1], 
    fontface = "bold",
    family = fonts$text
  ) +
  geom_text(
    aes(x = max_rate, label = paste0(round(max_rate, 1), "%")),
    nudge_x = 0.45, 
    size = 3.8, 
    color = colors$palette[2], 
    fontface = "bold",
    family = fonts$text
  ) +
  geom_text(  # Range labels
    aes(x = (min_rate + max_rate)/2, 
        label = paste0("Range: ", round(range_width, 1), "pp")),
    nudge_y = 0.2,
    size = 3.3,
    color = colors$palette[3],
    fontface = "italic",
    family = fonts$text
  ) +
  # Scales
  scale_x_continuous(
    labels = percent_format(scale = 1),
    breaks = seq(0, 12, 2),
    limits = c(0, 13)
  ) +
  # Labs
  labs(
    title = "Unemployment Range by Decade: Stability vs Volatility",
    subtitle = str_glue(
    "Each decade's <span style='color:{colors$palette[1]}'>**lowest**</span> and 
     <span style='color:{colors$palette[2]}'>**highest**</span> unemployment rates<br>
     The 1980s and 1990s show the greatest volatility with ranges >5 percentage points"
    ),
    x = "Unemployment Rate",
    y = NULL,
  ) +
  # Theme
  theme(
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

### |-  combined plot ----
combined_plots <- p1 / p2 +
  plot_layout(heights = c(1, 1)) 

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
        size = rel(0.95),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.9),
        lineheight = 0.9,
        margin = margin(t = 5, b = 0)
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


# 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2025-08-19
# rstudio  2025.05.1+513 Mariposa Orchid (desktop)
# pandoc   NA
# 
# ─ Packages ───────────────────────────────────────
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
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ──────────────────────────────────────────────────
# > 
