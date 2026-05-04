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
  readxl,      # Read Excel Files
  ggrepel      # Tidy Data Labels
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

# Score band boundaries (from data distribution)
elite_cut    <- 4.6   # ~75th percentile
trailing_cut <- 4.4   # ~25th percentile

df <- df_raw |>
  mutate(
    score_vs_mean = score - field_mean,
    
    tier = case_when(
      score >= elite_cut    ~ "elite",
      score >= trailing_cut ~ "competitive",
      TRUE                  ~ "trailing"
    ),
    tier = factor(tier, levels = c("elite", "competitive", "trailing")),
    
    rank = rank(-score, ties.method = "first"),
    
    # Label: top 5 + 2 notable below-mean spots for contrast
    label = case_when(
      rank <= 5                          ~ restaurant_name,
      restaurant_name == "Cafe Coyote"   ~ restaurant_name,  # lowest rated
      restaurant_name == "El Chingon"    ~ restaurant_name,  # 2nd lowest
      TRUE                               ~ NA_character_
    )
  ) |>
  arrange(score_vs_mean)

# Tier n counts for zone annotations
n_elite      <- sum(df$tier == "elite")
n_competitive <- sum(df$tier == "competitive")
n_trailing   <- sum(df$tier == "trailing")

# X axis bounds
x_min <- floor(min(df$score_vs_mean) * 10) / 10 - 0.02
x_max <- max(df$score_vs_mean) + 0.06


## 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    elite       = "#8B2331",   # deep burgundy
    competitive = "#4A7BA7",   # steel blue
    trailing    = "#BBBBBB",   # light gray
    mean_line   = "#444444",   # near-black for dashed reference
    band_elite  = "#FDF5F5",   # barely-there blush (alpha kept low)
    band_trail  = "#F8F8F8",   # barely-there cool gray
    zone_text   = "#888888"    # muted zone label color (SWD-style)
  )
)

### |-  titles and caption ----
title_text    <- "San Diego taco ratings barely separate the best from the rest"
subtitle_text <- glue(
  "Scores cluster tightly around the average ({round(field_mean, 2)}), ",
  "with most restaurants within ±0.2 points. ",
  "Just **<span style='color:{colors$palette$elite}'>{n_elite} of 50 spots</span>** ",
  "reach 4.6 or higher."
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
    axis.title.x       = element_text(size = 8.5, color = "gray45",
                                      margin = margin(t = 8), family = fonts$text),
    axis.title.y       = element_blank(),
    axis.text.x        = element_text(size = 7.5, color = "gray50"),
    axis.text.y        = element_text(size = 7, color = "gray35", hjust = 1),
    axis.ticks         = element_blank(),
    
    panel.grid.major.x = element_line(color = "gray93", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    
    plot.margin = margin(t = 20, r = 35, b = 20, l = 10)
  )
)
theme_set(weekly_theme)


### |-  zone thresholds on relative scale ----
elite_rel    <- elite_cut    - field_mean
trailing_rel <- trailing_cut - field_mean


### |-  build plot ----
p <- df |>
  ggplot(aes(x = score_vs_mean, y = reorder(restaurant_name, score_vs_mean))) +
  
  # --- Tier background bands (very subtle — data carries the story) ---
  annotate("rect",
           xmin = elite_rel, xmax = x_max,
           ymin = -Inf, ymax = Inf,
           fill = colors$palette$band_elite, alpha = 0.07
  ) +
  annotate("rect",
           xmin = x_min, xmax = trailing_rel,
           ymin = -Inf, ymax = Inf,
           fill = colors$palette$band_trail, alpha = 0.07
  ) +
  
  # --- Mean reference line ---
  geom_vline(
    xintercept = 0,
    color      = colors$palette$mean_line,
    linewidth  = 0.55,
    linetype   = "dashed"
  ) +
  
  # --- Lollipop stems ---
  geom_segment(
    aes(x = 0, xend = score_vs_mean,
        y    = reorder(restaurant_name, score_vs_mean),
        yend = reorder(restaurant_name, score_vs_mean),
        color = tier),
    linewidth = 0.5,
    alpha     = 0.55
  ) +
  
  # --- Dots ---
  geom_point(
    aes(color = tier, size = tier),
    alpha = 0.88
  ) +
  
  # --- Labels: top 5 + 2 low-end anchors ---
  ggrepel::geom_text_repel(
    aes(label = label),
    size          = 2.55,
    color         = "gray20",
    family        = fonts$text,
    hjust         = 0,
    direction     = "y",
    nudge_x       = 0.01,
    segment.color = "gray75",
    segment.size  = 0.25,
    max.overlaps  = 15,
    na.rm         = TRUE
  ) +
  
  # --- Zone labels (SWD-style: left-flush, muted, inside panel) ---
  # Elite zone — anchored in open space between 4.6 and 4.7 rows
  annotate("text",
           x      = 0.13,
           y      = 44.5,
           label  = glue("Elite\n≥{elite_cut} · n = {n_elite}"),
           hjust  = 0,
           size   = 2.5,
           color  = colors$palette$elite,
           family = fonts$text,
           fontface    = "bold",
           lineheight  = 1.25
  ) +
  # Competitive zone
  annotate("text",
           x      = 0.005,
           y      = n_trailing + (n_competitive * 0.5),
           label  = glue("Competitive\n{trailing_cut}–{elite_cut - 0.01} · n = {n_competitive}"),
           hjust  = 0,
           size   = 2.5,
           color  = colors$palette$zone_text,
           family = fonts$text,
           fontface   = "bold",
           lineheight = 1.25
  ) +
  # Trailing zone — pushed left into open space, clear of dots
  annotate("text",
           x        = -0.41,
           y        = 9.5,
           label    = glue("Trailing\n<{trailing_cut} · n = {n_trailing}"),
           hjust    = 0,
           vjust    = 0,
           size     = 2.5,
           color    = colors$palette$zone_text,
           family   = fonts$text,
           fontface   = "bold",
           lineheight = 1.25
  ) +
  
  # --- Key insight annotation (right of mean line, mid-chart) ---
  annotate("text",
           x        = 0.06,
           y        = 27,
           label    = "Most ratings fall\nwithin a 0.2-point band",
           hjust    = 0,
           size     = 2.6,
           color    = "gray50",
           family   = fonts$text,
           fontface = "italic",
           lineheight = 1.3
  ) +
  
  # --- Left-tail micro-insight — below trailing label ---
  annotate("text",
           x        = -0.41,
           y        = 7.8,
           label    = "Even the lowest-rated spots\nare within ~0.4 points of average",
           hjust    = 0,
           vjust    = 0,
           size     = 2.3,
           color    = "gray60",
           family   = fonts$text,
           fontface = "italic",
           lineheight = 1.3
  ) +
  
  # --- Mean line label ---
  annotate("text",
           x        = 0.01,
           y        = 49.2,
           label    = glue("Average\n({round(field_mean, 2)})"),
           hjust    = 0,
           size     = 2.2,
           color    = colors$palette$mean_line,
           family   = fonts$text,
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
      "elite"       = 2.8,
      "competitive" = 2.2,
      "trailing"    = 1.9
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%+.2f", x),
    breaks = seq(-0.4, 0.4, by = 0.1),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  
  # --- Labels ---
  labs(
    title    = title_text,
    subtitle = subtitle_text,
    caption  = caption_text,
    x        = glue("Difference from average ({round(field_mean, 2)})")
  ) +
  
  theme(
    plot.title = element_text(
      size     = 14, face = "bold", color = "gray10",
      family   = fonts$text, margin = margin(b = 6)
    ),
    plot.subtitle = element_markdown(
      size       = 9, color = "gray35", family = fonts$text,
      lineheight = 1.4, margin = margin(b = 16)
    ),
    plot.caption = element_markdown(
      size   = 7, color = "gray55", family = fonts$text,
      hjust  = 0, margin = margin(t = 12)
    )
  )

snap(p)

