## Challenge: #MakeoverMonday 2026 week 03
## Data:      Most Posted US Jobs by State
## Author:    Steven Ponce
## Date:      2026-01-26

## Article
# https://lightcast.io/resources/blog/most-posted-for-jobs-in-each-us-state

## Data
# https://data.world/makeovermonday/2025w4-most-posted-us-jobs-by-state

## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details


## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggtext, showtext, scales, glue, patchwork, janitor
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 14,
  height = 10,
  units  = "in",
  dpi    = 320
)

# Source utility functions
source(here::here("R/utils/fonts.R"))
source(here::here("R/utils/social_icons.R"))
source(here::here("R/themes/base_theme.R"))

# ============================================================================
# REFERENCES & DATA PREPARATION DOCUMENTATION
# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ ORIGINAL VISUALIZATION                                                      │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# Title:    "What Jobs Are in Highest Demand in Each State?"
# Source:   Lightcast Job Posting Analytics
# Article:  "Job titles in highest-demand, state by state"
# URL:      https://lightcast.io/resources/blog/most-posted-for-jobs-in-each-us-state
# Date:     2025
#
# Original chart showed a choropleth map of the #3 most-posted job by state
# with 8 color categories, making patterns difficult to discern.

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ DATA SOURCES                                                                │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# PRIMARY DATA (Makeover Monday):
# ──────────────────────────────────────────────────────────────────────────────
# Source:     Lightcast Job Posting Analytics
# Dataset:    Top Job Posted for by State (Oct 2024 - Oct 2025)
# Variables:  State Name, 1st Place, 2nd Place, 3rd Place (job titles)
# Coverage:   50 US States
# URL:        https://data.world/makeovermonday/2025w04
# File:       LIGHTCAST_TOP JOB POSTED FOR BY STATE OCT 24-25.csv
#
# Note: Original data contains rankings only (no counts). Job posting counts
# are proprietary to Lightcast and not publicly available.
#
# ENHANCEMENT DATA #1 (Employment Context):
# ──────────────────────────────────────────────────────────────────────────────
# Source:     U.S. Bureau of Labor Statistics (BLS)
# Dataset:    Occupational Employment and Wage Statistics (OEWS)
# Release:    May 2024 State Estimates
# URL:        https://www.bls.gov/oes/current/oessrcst.htm
# Variables:  Employment counts, mean annual wages by occupation × state
#
# Occupations extracted (SOC codes):
#   - 29-1141: Registered Nurses
#   - 53-3032: Heavy and Tractor-Trailer Truck Drivers
#   - 41-2031: Retail Salespersons
#   - 15-1252: Software Developers
#   - 43-4051: Customer Service Representatives
#   - 29-2061: Licensed Practical and Licensed Vocational Nurses
#
# Citation:
#   U.S. Bureau of Labor Statistics. (2025). Occupational Employment and
#   Wage Statistics, May 2024. Retrieved from https://www.bls.gov/oes/
#
# ENHANCEMENT DATA #2 (Population Context):
# ──────────────────────────────────────────────────────────────────────────────
# Source:     U.S. Census Bureau
# Dataset:    Population Estimates Program
# Year:       2023 State Population Estimates
# URL:        https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
#
# Citation:
#   U.S. Census Bureau. (2023). Annual Estimates of the Resident Population
#   for the United States, Regions, States, District of Columbia, and
#   Puerto Rico: April 1, 2020 to July 1, 2023 (NST-EST2023-POP).

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ DATA PREPARATION STEPS                                                      │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# STEP 1: Load Lightcast Rankings Data
# ──────────────────────────────────────────────────────────────────────────────
# - Read CSV from data.world/makeovermonday
# - Clean column names (janitor::clean_names)
# - Reshape from wide to long format (1st, 2nd, 3rd → rank column)
# - Standardize job title strings to match BLS occupation names
#
# STEP 2: Compile BLS OEWS Employment Data
# ──────────────────────────────────────────────────────────────────────────────
# - Data extracted from individual BLS occupation pages (May 2024)
# - Employment counts (tot_emp) and mean annual wages (a_mean) by state
# - Six occupations matching Lightcast job categories
# - Note: "Physician" category not included due to multiple SOC codes
#         (29-1211 through 29-1229 for various specialties)
#
# STEP 3: Add State Population Data
# ──────────────────────────────────────────────────────────────────────────────
# - Census Bureau 2023 population estimates for all 50 states
# - Used for per-capita calculations
#
# STEP 4: Merge and Calculate Derived Metrics
# ──────────────────────────────────────────────────────────────────────────────
# - Join Lightcast rankings with BLS employment data on state + job category
# - Join with Census population data on state name
# - Calculate: emp_per_100k = (tot_emp / population) * 100,000
# - Calculate: national average RN per 100k for reference line
#
# STEP 5: Create Summary Datasets
# ──────────────────────────────────────────────────────────────────────────────
# - state_summary: One row per state with top job metrics
# - job_summary: Aggregated statistics by job category
# - gap_data: Software vs. Trucking comparison subset

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ KEY ANALYTICAL DECISIONS                                             │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# 1. WHY ENHANCE WITH BLS DATA?
#    The original Lightcast data provides rankings only. BLS OEWS data adds:
#    - Actual employment magnitudes for context
#    - Wage information for economic analysis
#    - Enables per-capita comparisons across states
#
# 2. WHY PER-CAPITA ADJUSTMENT?
#    Raw employment counts favor large population states (CA, TX, FL).
#    Per-capita rates reveal workforce concentration patterns that would
#    otherwise be hidden. Example: Massachusetts has highest RN concentration
#    despite ranking 7th in raw RN employment.
#
# 3. WHY FOCUS ON #2 JOB?
#    Registered Nurse is #1 in 49/50 states - minimal variation to visualize.
#    The #2 job reveals meaningful regional economic patterns:
#    - Software Dev: Tech hub states (VA, DE)
#    - Trucking: Logistics corridor (Midwest, South)
#    - Retail: Service economies (coasts, urban)
#    - Physician: States with aging/rural populations
#
# 4. DATA LIMITATIONS
#    - Lightcast rankings are based on job postings, not actual employment
#    - BLS OEWS is survey-based with sampling error
#    - Time period mismatch: Lightcast (Oct 2024-2025) vs BLS (May 2024)
#    - Physician and Retail Store Manager categories lack BLS match

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ OUTPUT FILES                                                         │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# data/2026/bls_oews_state_jobs_processed.csv
#   - BLS employment and wage data by state × occupation
#   - 300 rows (6 occupations × 50 states)
#   - Columns: state_name, job_category, tot_emp, a_mean
#
# data/2026/mm_wk04_state_summary.csv
#   - One row per state with job rankings and metrics
#   - 50 rows
#   - Key columns: state_name, population, top_job, second_job, third_job,
#                  top_job_emp, top_job_per_100k, second_job_emp, second_job_wage
#
# data/2026/mm_wk04_job_summary.csv
#   - Aggregated statistics by job category
#   - Columns: job_category, times_ranked_1/2/3, total_national_emp, avg_wage

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ REPRODUCIBILITY                                                      │
# └─────────────────────────────────────────────────────────────────────────────┘
#
# R Version:
#   sessionInfo() output captured at end of script
#
# Scripts (run in order):
#   1. mm_wk04_bls_data_pull_v2.R    - Compile BLS OEWS data
#   2. mm_wk04_data_merge_v2.R       - Merge all sources, create summaries
#   3. mm_wk04_phase2_dashboard_v3.R - Final visualization
#
# END REFERENCES & DATA PREPARATION DOCUMENTATION
# ============================================================================

## 2. READ IN THE DATA ----
state_summary <- read_csv("data/2026/mm_wk04_state_summary.csv")
job_summary <- read_csv("data/2026/mm_wk04_job_summary.csv")


## 3. EXAMINING THE DATA ----
glimpse(state_summary)
glimpse(job_summary)


## 4. TIDY DATA ----

# Note: Primary data wrangling was performed in data preparation scripts:
#   - mm_wk04_bls_data_pull.R (BLS OEWS extraction)
#   - mm_wk04_data_merge.R (merge all sources)
# 
# This script reads the processed summary files and performs
# visualization-specific transformations only.

### |- P2 data: #2 job distribution ----
p2_data <- state_summary |>
  count(second_job, name = "n_states") |>
  mutate(
    second_job = fct_reorder(second_job, n_states),
    label = ifelse(n_states == 1, "1 state", paste0(n_states, " states")),
    is_hero = second_job %in% c(
      "Software Developer / Engineer",
      "Tractor-Trailer Truck Driver"
    )
  )

### |- P3 data: Software vs Trucking comparison ----
p3_data <- state_summary |>
  filter(second_job %in% c(
    "Tractor-Trailer Truck Driver",
    "Software Developer / Engineer"
  )) |>
  group_by(second_job) |>
  summarise(
    n_states = n(),
    total_emp = sum(second_job_emp, na.rm = TRUE),
    avg_wage = mean(second_job_wage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    job_short = case_when(
      str_detect(second_job, "Truck") ~ "Trucking",
      str_detect(second_job, "Software") ~ "Software"
    ),
    job_short = factor(job_short, levels = c("Software", "Trucking"))
  )


# 5. VISUALIZATION ----

### |-  plot aesthetics ----
colors <- get_theme_colors(
  palette = list(
    software      = "#7B5E9A",
    trucking      = "#C4915E",
    neutral       = "#A0A4A8",
    text_headline = "#2C3E50",
    text_dark     = "#2C3E50",
    text_mid      = "#5D6D7E",
    text_light    = "#95A5A6"
  )
)

### |-  job color mapping ----
job_colors <- c(
  "Software Developer / Engineer"   = colors$palette$software,
  "Tractor-Trailer Truck Driver"    = colors$palette$trucking,
  "Retail Sales Associate"          = colors$palette$neutral,
  "Physician"                       = colors$palette$neutral,
  "Registered Nurse"                = colors$palette$neutral
)

### |-  Main titles ----
title_text <- "America's Most In-Demand Jobs: Beyond the Headlines"

subtitle_text <- str_glue(
  "Registered Nurses dominate job postings nationwide—but the #2 job reveals a stark divide between <span style='color:#7B5E9A;font-weight:bold;'>**Software**</span> and <span style='color:#C4915E;font-weight:bold;'>**Trucking**</span> states"
)

caption_text <- create_social_caption(
  mm_year = 2026, mm_week = 04,
  source_text = str_glue(
    "Lightcast Job Posting Analytics (Oct 2024-2025) | BLS OEWS May 2024 | Census Bureau 2023"
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
      size = rel(1.3), family = fonts$title, face = "bold",
      color = colors$title, lineheight = 1.1, hjust = 0,
      margin = margin(t = 5, b = 10)
    ),
    plot.subtitle = element_markdown(
      size = rel(0.8), family = fonts$subtitle, face = "italic",
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
    # axis.line.x  = element_line(color = "#252525", linewidth = .1),
    # axis.ticks.y = element_blank(),
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

### |-  P1 - THE BIG NUMBER ---- 
p1 <- ggplot() +
  # Annotate
  annotate(
    "text",
    x = 0.5, y = 0.75,
    label = "49",
    size = 40,
    fontface = "bold",
    color = colors$palette$text_headline
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.32,
    label = "out of 50 states have\nRegistered Nurse\nas the #1 most-posted job",
    size = 4.5,
    color = colors$palette$text_dark,
    lineheight = 1.2
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.08,
    label = "The one exception? One state has Retail Sales Associate in the top spot.",
    size = 3.5,
    color = colors$palette$text_mid,
    fontface = "italic"
  ) +
  # Limits
  xlim(0, 1) +
  ylim(0, 1) +
  # Theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = colors$background, color = colors$background),
    panel.background = element_rect(fill = colors$background, color = colors$background),
    plot.margin = margin(10, 10, 10, 10)
  )

### |-  P2 - #2 JOB DISTRIBUTION ----
p2 <- p2_data |>
  ggplot(aes(x = n_states, y = second_job, fill = second_job)) +
  # Geoms
  geom_col(width = 0.7) +
  geom_text(
    aes(
      label = label,
      fontface = ifelse(is_hero, "bold", "plain")
    ),
    hjust = -0.1,
    size = 4,
    color = colors$palette$text_dark
  ) +
  # Scales
  scale_fill_manual(values = job_colors, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  # Labs
  labs(
    title = "What's Your State's #2 Most-Posted Job?",
    subtitle = "Since RN dominates #1, the real variation is in the second spot",
    x = NULL,
    y = NULL
  ) +
  # Theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

### |-  P3 - THE OPPORTUNITY GAP ---

# Sub-panel A: Number of states
p3a <- p3_data |>
  ggplot(aes(x = job_short, y = n_states, fill = second_job)) +
  # Geoms
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(n_states, " states")),
    vjust = -0.5,
    fontface = "bold",
    size = 3.5,
    color = colors$palette$text_dark
  ) +
  # Scales
  scale_fill_manual(values = job_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  # Labs
  labs(title = "Where It's #2") +
  # Theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Sub-panel B: Average wage
p3b <- p3_data |>
  ggplot(aes(x = job_short, y = avg_wage, fill = second_job)) +
  # Geoms
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::dollar(avg_wage, accuracy = 1, scale = 0.001, suffix = "k")),
    vjust = -0.5,
    fontface = "bold",
    size = 3.5,
    color = colors$palette$text_dark
  ) +
  # Scales
  scale_fill_manual(values = job_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  # Labs
  labs(title = "Average Wage") +
  # Theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Sub-panel C: Total employment
p3c <- p3_data |>
  ggplot(aes(x = job_short, y = total_emp, fill = second_job)) +
  # Geoms
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::comma(total_emp)),
    vjust = -0.5,
    fontface = "bold",
    size = 3.5,
    color = colors$palette$text_dark
  ) +
  # Scales
  scale_fill_manual(values = job_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  # Labs
  labs(title = "Total Employment") +
  # Theme
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine P3
p3 <- (p3a | p3b | p3c)

### |-  combined plot ----
# combined_plot <- 
(p1 | p2) / p3 +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(
        size = rel(2.2),
        family = fonts$title,
        face = "bold",
        color = colors$title,
        lineheight = 1.15,
        margin = margin(t = 5, b = 10)
      ),
      plot.subtitle = element_markdown(
        size = rel(0.9),
        family = fonts$subtitle,
        color = alpha(colors$subtitle, 0.88),
        lineheight = 1.5,
        margin = margin(t = 5, b = 10)
      ),
      plot.caption = element_markdown(
        size = rel(0.65),
        family = fonts$subtitle,
        color = colors$caption,
        hjust = 0,
        lineheight = 1.4,
        margin = margin(t = 20, b = 5)
      ),
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

# ─ Session info ──────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 11 x64 (build 26100)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2026-01-26
# rstudio  2026.01.0+392 Apple Blossom (desktop)
# pandoc   NA
# 
# ─ Packages ──────────────────────────────────────────────────────────────────────
# ! package     * version  date (UTC) lib source
# V base        * 4.4.1    2024-04-24 [2] local (on disk 4.4.0)
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
# dplyr       * 1.1.4    2023-11-17 [1] RSPM (R 4.4.0)
# farver        2.1.2    2024-05-13 [1] RSPM (R 4.4.0)
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
# janitor     * 2.2.1    2024-12-22 [1] RSPM (R 4.4.0)
# jsonlite      1.8.9    2024-09-20 [1] RSPM (R 4.4.0)
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
# rlang         1.1.5    2025-01-17 [1] RSPM (R 4.4.0)
# rprojroot     2.0.4    2023-11-05 [1] RSPM (R 4.4.0)
# rstudioapi    0.17.1   2024-10-22 [1] RSPM (R 4.4.0)
# rsvg          2.6.1    2024-09-20 [1] RSPM (R 4.4.0)
# scales      * 1.3.0    2023-11-28 [1] RSPM (R 4.4.0)
# P sessioninfo   1.2.2    2021-12-06 [?] RSPM (R 4.4.0)
# showtext    * 0.9-7    2024-03-02 [1] RSPM (R 4.4.0)
# showtextdb  * 3.0      2020-06-04 [1] RSPM (R 4.4.0)
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
# vroom         1.6.5    2023-12-05 [1] RSPM (R 4.4.0)
# withr         3.0.2    2024-10-28 [1] RSPM (R 4.4.0)
# xfun          0.50     2025-01-07 [1] RSPM (R 4.4.0)
# xml2          1.3.6    2023-12-04 [1] RSPM (R 4.4.0)
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ─────────────────────────────────────────────────────────────────────────────────
# > 
