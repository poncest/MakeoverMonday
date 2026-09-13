## Challenge: #MakeoverMonday 2026 week 37
## Data:      What are the world's deadliest animals?

## Author:    Steven Ponce
## Date:      2026-09-13

## Article
# https://ourworldindata.org/deadliest-animals?utm_source=chatgpt.com

## Data
# https://pub-cee805df54de4b6c8f93bee984e3c725.r2.dev/datasets/what-are-the-world-s-deadliest-animals/worlds_deadliest_animals.csv

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details.


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, 
  janitor, ggview
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))


## 2. READ IN THE DATA ----
df_raw <- read_csv(
  "data/2026/worlds_deadliest_animals.csv") |>
  clean_names()


## 3. EXAMINING THE DATA ----
glimpse(df_raw)
skimr::skim_without_charts(df_raw)


## 4. TIDY DATA ---- 

### |- source-audit enrichment (mechanism classification, derived from
###    OWID's technical documentation text, not from the numbers) ----
df_enriched <- tribble(
  ~animal,               ~deaths, ~mechanism,
  "Mosquitoes",          760000,  "vector-borne disease",
  "Humans",              600000,  "violence/conflict",
  "Snakes",              100000,  "envenomation",
  "Dogs",                40000,   "vector-borne disease",
  "Freshwater snails",   14000,   "vector-borne disease",
  "Kissing bugs",        8000,    "vector-borne disease",
  "Sandflies",           5000,    "vector-borne disease",
  "Roundworms",          4000,    "parasitic disease",
  "Scorpions",           3000,    "envenomation",
  "Tapeworms",           2000,    "parasitic disease",
  "Tsetse flies",        1500,    "vector-borne disease",
  "Elephants",           1000,    "attack/encounter",
  "Bees, wasps, hornets", 500,    "sting/anaphylaxis",
  "Big cats",            300,     "attack/encounter",
  "Crocodiles",          150,     "attack/encounter",
  "Jellyfish",           100,     "envenomation",
  "Hippopotamuses",      50,      "attack/encounter",
  "Spiders",             50,      "envenomation",
  "Bears",               20,      "attack/encounter",
  "Sharks",              6,       "attack/encounter",
  "Gray wolves",         5,       "attack/rabies (mixed)"
)

### |- three-mechanism hero structure  ----
### Covers ~99.3% of the non-human total. Parasitic disease, sting/
### anaphylaxis, and the mixed wolf category (~0.7% combined) are
### disclosed in the caption, not shown as a fourth point.
mechanism_levels <- c("attack/encounter", "envenomation", "vector-borne disease")

total_nonhuman <- df_enriched |>
  filter(animal != "Humans") |>
  summarise(total = sum(deaths)) |>
  pull(total)

df_hero <- df_enriched |>
  filter(animal != "Humans", mechanism %in% mechanism_levels) |>
  summarise(deaths = sum(deaths), .by = mechanism) |>
  mutate(
    mechanism = factor(mechanism, levels = mechanism_levels),
    label_top = mechanism %in% c("envenomation"),
    label_hjust = case_when(
      mechanism == "attack/encounter" ~ 0,
      mechanism == "vector-borne disease" ~ 1,
      TRUE ~ 0.5
    ),
    # round to ~2 significant figures, matching OWID's own stated rounding convention
    deaths_rounded = signif(deaths, 2),
    detail = case_when(
      mechanism == "attack/encounter" ~ "elephants, crocodiles, sharks, and others",
      mechanism == "envenomation" ~ "snakes alone ≈100,000",
      mechanism == "vector-borne disease" ~ "mosquitoes, dogs, freshwater snails, and others"
    )
  )

# display label distinct from the analytical `mechanism` field
df_hero <- df_hero |>
  mutate(
    display_label = case_when(
      mechanism == "vector-borne disease" ~ "disease transmission",
      TRUE ~ as.character(mechanism)
    )
  )

pct_covered <- sum(df_hero$deaths) / total_nonhuman
pct_excluded <- 1 - pct_covered


## 5. VISUALIZATION ----

### |- plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    attack = "#76844E", venom = "#DE4500", disease = "#531745"
  )
)

mechanism_colors <- c(
  "attack/encounter"     = colors$palette$attack,
  "envenomation"          = colors$palette$venom,
  "vector-borne disease" = colors$palette$disease
)

### |- titles and caption ----
title_text <- str_glue("The deadliest animals rarely kill by attacking us")

subtitle_text <- str_glue(
  "Estimated annual human deaths by mechanism. Envenomation — mostly ",
  "snakes — is the surprising middle case: far above direct attacks, ",
  "well below disease transmission."
)

caption_text <- create_social_caption(
  mm_year = 2026,
  mm_week = 37,
  source_text = glue(
    "Our World in Data, 'What are the world's deadliest animals?'<br>",
    "These three mechanisms account for ~{label_percent(accuracy = 0.1)(pct_covered)} ",
    "of non-human-animal deaths; parasitic disease, stings, and other causes ",
    "(~{label_percent(accuracy = 0.1)(pct_excluded)}) are omitted.<br>",
    "Estimates are OWID's triangulated approximations, not precise counts — ",
    "envenomation is driven almost entirely by snakes, one of OWID's most ",
    "uncertain figures (plausibly 50% higher)."
  )
)

### |- fonts ----
setup_fonts()
fonts <- get_font_families()

### |- plot theme ----
base_theme <- create_base_theme(colors)

weekly_theme <- extend_weekly_theme(
  base_theme,
  theme(
    axis.text.y      = element_blank(),
    axis.title       = element_blank(),
    axis.ticks.y      = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "gray88", linewidth = 0.25),
    legend.position    = "none",
    plot.margin        = margin(20, 40, 20, 40),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 24, face = "bold", family = fonts$title_1,
      width = grid::unit(1, "npc"), lineheight = 1.05,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      size = 12.5, family = fonts$subtitle, color = "grey30",
      width = grid::unit(1, "npc"), margin = margin(t = 2, b = 22)
    ),
    plot.caption = element_textbox_simple(
      size = 8, family = fonts$caption, color = "grey45",
      margin = margin(t = 14)
    ),
  )
)

theme_set(weekly_theme)

### |- plot ----
p <- df_hero |>
  ggplot(aes(x = deaths, y = 0, color = mechanism)) +
  geom_segment(
    x = 1000, xend = 1000000, y = 0, yend = 0,
    color = "gray70", linewidth = 0.4, inherit.aes = FALSE
  ) +
  geom_point(size = 7) +
  geom_text(
    aes(
      label = mechanism,
      y = if_else(label_top, 0.55, -0.55),
      hjust = label_hjust
    ),
    fontface = "bold", size = 4.2, family = fonts$text, show.legend = FALSE
  ) +
  geom_text(
    aes(
      label = glue("≈{comma(deaths_rounded)} · {detail}"),
      y = if_else(label_top, 0.32, -0.32),
      hjust = label_hjust
    ),
    size = 3.2, color = "gray40", family = fonts$text, show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 1000, y = 1.05, hjust = 0, size = 3.2, color = "gray40",
    label = "each step represents 10× more deaths", family = fonts$text
  ) +
  scale_x_log10(
    breaks = c(1e3, 1e4, 1e5, 1e6),
    labels = comma_format(),
    limits = c(1000, 1000000),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(values = mechanism_colors) +
  coord_cartesian(ylim = c(-1, 1.3), clip = "off") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme(
    plot.title = element_textbox_simple(
      size = 24, face = "bold", family = fonts$title,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      size = 12, color = "gray30", family = fonts$body,
      margin = margin(b = 20), lineheight = 1.3
    ),
    plot.caption = element_textbox_simple(
      size = 6.5, color = "gray50", family = fonts$body,
      margin = margin(t = 16), lineheight = 1.3
    )
  )

### |- Preview ----
p + canvas(width = 12, height = 6, units = "in")


### |- save ----
save_ggplot(
  plot = p,
  file = here::here("2026", "Week_37", "2026_37.png"),
  width  = 12, height = 6,
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
