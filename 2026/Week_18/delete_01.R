## Challenge: #MakeoverMonday 2026 week 18
## Data:      Eater San Diego — Best Local Tacos
## Author:    Steven Ponce
## Date:      2026-05-04

## Article
# https://sandiego.eater.com/maps/san-diego-best-local-tacos
# https://gemini.google.com/share/672e1babcb27

## Data
# https://data.world/makeovermonday/2026w18-best-tacos-in-san-diego/

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  scales,      # Scale Functions for Visualization
  glue,        # Interpreted String Literals
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  readxl       # Read Excel Files
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/utils/snap.R"))
source(here::here("R/themes/base_theme.R"))

## 2. READ IN THE DATA ----
df_raw <- read_excel("data/2026/top_tacos_in_san_diego_tc26.xlsx") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ----
field_mean <- mean(df_raw$score)

df <- df_raw |>
  mutate(
    # Score relative to field mean
    score_vs_mean = score - field_mean,
    
    # Tier assignment — meaningful thresholds, not arbitrary
    tier = case_when(
      score >= 4.6 ~ "elite",       # top ~16% of field (≥4.6)
      score >= 4.4 ~ "competitive", # middle cluster (4.4–4.59)
      TRUE         ~ "trailing"     # below 4.4
    ),
    tier = factor(tier, levels = c("elite", "competitive", "trailing")),
    
    # Rank by score (descending)
    rank = rank(-score, ties.method = "first"),
    
    # Label: top 10 by score only
    label = if_else(rank <= 10, restaurant_name, NA_character_)
  ) |>
  arrange(rank)

# Tier counts for annotation
tier_counts <- df |>
  count(tier) |>
  mutate(
    label_text = glue("{n} restaurants")
  )


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    elite       = "#8B2331",  # deep burgundy — standout performers
    competitive = "#4A7BA7",  # steel blue — solid field
    trailing    = "#C8C8C8",  # light gray — fade back
    mean_line   = "#555555",  # dark gray for reference line
    band_elite  = "#FAF0F1",  # very faint blush for elite band
    band_trail  = "#F5F5F5"   # very faint gray for trailing band
  )
)

### |-  titles and caption ----
title_text    <- "In San Diego's taco scene, excellence clusters in a narrow band"
subtitle_text <- glue(
  "Each dot is one of 50 restaurants, positioned by score relative to the field mean ({round(field_mean, 2)}). ",
  "Only **<span style='color:{colors$palette$elite}'>16 restaurants</span>** score 4.6 or above — ",
  "the rest are separated by less than a point."
)

caption_text <- create_social_caption(
  mm_year     = 2026,
  mm_week     = 18,
  source_text = "Eater San Diego | data.world/makeovermonday"
)

### |-  fonts ----
setup_fonts()
fonts <- get_font_families()

### |-  plot theme ----
base_theme   <- create_base_theme(colors)
weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    # Axes
    axis.title.x      = element_text(size = 9, color = "gray40", margin = margin(t = 8)),
    axis.title.y      = element_blank(),
    axis.text.x       = element_text(size = 8, color = "gray50"),
    axis.text.y       = element_text(size = 7.5, color = "gray30", hjust = 1),
    
    # Grid — horizontal only, faint
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    # Margins
    plot.margin = margin(t = 20, r = 30, b = 20, l = 10)
  )
)
theme_set(weekly_theme)


### |-  tier band boundaries ----
# Shade the elite zone (≥4.6) and trailing zone (<4.4) on x-axis
elite_threshold    <- 4.6  - field_mean   # right boundary of elite relative band
trailing_threshold <- 4.4  - field_mean   # left boundary of competitive zone
x_min <- min(df$score_vs_mean) - 0.02
x_max <- max(df$score_vs_mean) + 0.02


### |-  build plot ----
p <- df |>
  ggplot(aes(x = score_vs_mean, y = reorder(restaurant_name, score_vs_mean))) +
  
  # --- Background tier bands ---
  # Elite zone (right side)
  annotate(
    "rect",
    xmin = elite_threshold, xmax = x_max,
    ymin = -Inf, ymax = Inf,
    fill  = colors$palette$band_elite, alpha = 0.7
  ) +
  # Trailing zone (left side)
  annotate(
    "rect",
    xmin = x_min, xmax = trailing_threshold,
    ymin = -Inf, ymax = Inf,
    fill  = colors$palette$band_trail, alpha = 0.7
  ) +
  
  # --- Mean reference line (at 0) ---
  geom_vline(
    xintercept = 0,
    color      = colors$palette$mean_line,
    linewidth  = 0.5,
    linetype   = "dashed"
  ) +
  
  # --- Lollipop stems ---
  geom_segment(
    aes(x = 0, xend = score_vs_mean,
        y = reorder(restaurant_name, score_vs_mean),
        yend = reorder(restaurant_name, score_vs_mean),
        color = tier),
    linewidth = 0.45,
    alpha     = 0.6
  ) +
  
  # --- Dots ---
  geom_point(
    aes(color = tier, size = tier),
    alpha = 0.85
  ) +
  
  # --- Labels: top 10 only (right side of dot) ---
  ggrepel::geom_text_repel(
    aes(label = label),
    size          = 2.6,
    color         = "gray25",
    family        = fonts$text,
    hjust         = 0,
    direction     = "y",
    nudge_x       = 0.008,
    segment.color = "gray70",
    segment.size  = 0.25,
    max.overlaps  = 20,
    na.rm         = TRUE
  ) +
  
  # --- Band label: Elite ---
  annotate(
    "text",
    x      = (elite_threshold + x_max) / 2,
    y      = 2,
    label  = glue("Elite\n(≥4.6)\nn = {tier_counts$n[tier_counts$tier == 'elite']}"),
    size   = 2.6,
    color  = colors$palette$elite,
    family = fonts$text,
    fontface = "bold",
    hjust  = 0.5,
    lineheight = 1.2
  ) +
  
  # --- Band label: Trailing ---
  annotate(
    "text",
    x      = (x_min + trailing_threshold) / 2,
    y      = 2,
    label  = glue("Trailing\n(<4.4)\nn = {tier_counts$n[tier_counts$tier == 'trailing']}"),
    size   = 2.6,
    color  = "gray55",
    family = fonts$text,
    fontface = "bold",
    hjust  = 0.5,
    lineheight = 1.2
  ) +
  
  # --- Mean annotation ---
  annotate(
    "text",
    x      = 0.005,
    y      = nrow(df) - 1,
    label  = glue("Field mean\n({round(field_mean, 2)})"),
    size   = 2.4,
    color  = colors$palette$mean_line,
    family = fonts$text,
    hjust  = 0,
    lineheight = 1.2
  ) +
  
  # --- Scales ---
  scale_color_manual(
    values = c(
      "elite"       = colors$palette$elite,
      "competitive" = colors$palette$competitive,
      "trailing"    = colors$palette$trailing
    ),
    guide = "none"
  ) +
  scale_size_manual(
    values = c(
      "elite"       = 3.0,
      "competitive" = 2.4,
      "trailing"    = 2.0
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%+.2f", x),
    breaks = seq(-0.4, 0.4, by = 0.1),
    expand = expansion(mult = c(0.02, 0.15))  # room for labels on right
  ) +
  
  # --- Labels ---
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = glue("Score relative to field mean ({round(field_mean, 2)})")
  ) +
  
  # --- Title formatting ---
  theme(
    plot.title    = element_text(
      size = 14, face = "bold", color = "gray10",
      family = fonts$text, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size = 9.5, color = "gray35", family = fonts$text,
      lineheight = 1.4, margin = margin(b = 16)
    ),
    plot.caption  = element_markdown(
      size = 7, color = "gray55", family = fonts$text,
      hjust = 0, margin = margin(t = 12)
    )
  )

snap(p)

stop()


## 6. SAVE ----
snap(p,
     file   = "mm_2026_18.png",
     type   = "makeovermonday",
     year   = 2026,
     week   = 18,
     width  = 10,
     height = 12
)


## 7. HELPER FUNCTIONS DOCUMENTATION ----
# setup_fonts()         — initializes showtext fonts for the session
# get_font_families()   — returns named list of font family strings
# get_theme_colors()    — returns color object; access via colors$palette$<name>
# create_base_theme()   — base ggplot2 theme (panel, text defaults)
# extend_weekly_theme() — applies weekly overrides on top of base_theme
# create_social_caption()
#   Parameters: mm_year, mm_week, source_text
#   Returns:    HTML-formatted caption string with #MakeoverMonday tag
# snap()
#   Parameters: plot, file, type, year, week, width, height
#   Saves final output at consistent resolution


## 8. SESSION INFO ----
sessionInfo()