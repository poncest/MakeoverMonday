## Challenge: #MakeoverMondnay 2025 week 37
## Data:      
## Author:    Steven Ponce
## Date:      2025-09-09

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
)

# Create caption
caption_text <- create_social_caption(
  mm_year = 2025,
  mm_week = 37,
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



### |- p2: total harm bar chart ----


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

