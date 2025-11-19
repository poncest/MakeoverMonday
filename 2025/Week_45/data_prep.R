## Challenge: #MakeoverMonday 2025 week 45
## Data:      Terrorism and Political Violence in the United States  
## Author:    Steven Ponce
## Date:      2025-11-19

## Article
# https://www.csis.org/analysis/left-wing-terrorism-and-political-violence-united-states-what-data-tells-us

## Data Sources
# Terrorism data: terrorism_by_ideology_1994-2025.csv
# Population data: US Census Bureau via tidycensus package

## Citation
# Center for Strategic and International Studies (CSIS). (2025). 
# Terrorism and Political Violence in the United States: What the Data Tells Us. 
# CSIS Warfare, Irregular Threats, and Terrorism Program.
#
# US Census Bureau. Population Estimates Program (PEP).
# - 1994-2009: Intercensal Population Estimates
#   https://www.census.gov/data/tables/time-series/demo/popest/intercensal-national.html
#   https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-national.html
# - 2010-2019: Vintage 2019 estimates via tidycensus R package
# - 2020-2023: Vintage 2023 estimates via tidycensus R package  
# - 2024-2025: Linear projection based on 2020-2023 average annual growth rate

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,     # Easily Install and Load the 'Tidyverse'
  janitor,       # Simple Tools for Examining and Cleaning Dirty Data
  skimr,         # Compact and Flexible Summaries of Data
  scales,        # Scale Functions for Visualization
  ggtext,        # Improved Text Rendering Support for 'ggplot2'
  showtext,      # Using Fonts More Easily in R Graphs
  glue,          # Interpreted String Literals
  patchwork,     # The Composer of Plots
  ggrepel,       # Automatically Position Non-Overlapping Text Labels
  tidycensus     # Load US Census Boundary and Attribute Data
)

## 2. READ IN TERRORISM DATA ----
terrorism_usa_raw <- read_csv("data/2025/terrorism_by_ideology_1994-2025.csv") |>
  clean_names()


## 3. TIDY TERRORISM DATA ----
### |-  pivot to long format ----
terrorism_usa_long <- terrorism_usa_raw |> 
  pivot_longer(
    cols = c(other, ethnonationalist, jihadist, left, right),
    names_to = "ideology",
    values_to = "attacks"
  ) |> 
  mutate(
    # Capitalize and clean ideology names
    ideology = case_when(
      ideology == "other" ~ "Other",
      ideology == "ethnonationalist" ~ "Ethnonationalist",
      ideology == "jihadist" ~ "Jihadist",
      ideology == "left" ~ "Left",
      ideology == "right" ~ "Right"
    ),
    # Create ordered factor for consistent plotting
    ideology = factor(
      ideology, 
      levels = c("Right", "Left", "Jihadist", "Ethnonationalist", "Other")
    )
  )

## 4. FETCH US CENSUS POPULATION DATA ----
### |-  1994-2009: Intercensal estimates ----
# Official Census Bureau intercensal estimates (gold standard)
# These reconcile postcensal estimates with actual decennial census counts
#
# VERIFIABLE SOURCES:
# 2000-2009: https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/national/us-est00int-01.csv
#            (See "BOTH SEXES" row for July 1 estimates)
# 1994-1999: https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-1990-2000-national.html
#            (Values from intercensal datasets, matching USAFacts and other authoritative sources)
us_pop_1994_2009 <- tibble(
  year = 1994:2009,
  population = c(
    263125821, 266278393, 269394284, 272657333, 275854104, 279040168,  # 1994-1999
    282162411, 284968955, 287625193, 290107933, 292805298, 295516599,  # 2000-2005
    298379912, 301231207, 304093966, 306771529                         # 2006-2009
  )
)

### |-  2010-2019: Via tidycensus API ----
us_pop_2010s <- get_estimates(
  geography = "us",
  product = "population",
  vintage = 2019,
  time_series = TRUE
) |>
  filter(variable == "POP") |>
  mutate(year = 2010 + DATE) |>
  select(year, population = value)

### |-  2020-2023: Via tidycensus flat files ----
us_pop_2020s <- map_dfr(2020:2023, function(yr) {
  get_estimates(
    geography = "us",
    variables = "POPESTIMATE",
    vintage = 2023,
    year = yr
  ) |>
    mutate(year = yr) |>
    select(year, population = value)
}) |>
  distinct(year, .keep_all = TRUE)

### |-  2024-2025: Linear projection ----
recent_growth_rate <- (
  tail(us_pop_2020s$population, 1) - head(us_pop_2020s$population, 1)
) / head(us_pop_2020s$population, 1) / 3

us_pop_projected <- tibble(
  year = 2024:2025,
  population = tail(us_pop_2020s$population, 1) * (1 + recent_growth_rate)^(1:2)
)

### |-  Combine all population data ----
us_population <- bind_rows(
  us_pop_1994_2009,
  us_pop_2010s,
  us_pop_2020s,
  us_pop_projected
) |>
  arrange(year) |>
  distinct(year, .keep_all = TRUE)


## 5. CALCULATE ANALYTICAL METRICS ----
terrorism_enriched <- terrorism_usa_long |>
  left_join(us_population, by = "year") |>
  group_by(year) |>
  mutate(
    # Per capita rate
    attacks_per_million = (attacks / population) * 1000000,
    # Percentage composition
    pct_of_year = (attacks / sum(attacks)) * 100
  ) |>
  ungroup() |>
  # Cumulative totals
  arrange(ideology, year) |>
  group_by(ideology) |>
  mutate(cumulative_attacks = cumsum(attacks)) |>
  ungroup() |>
  # Period classifications
  mutate(
    period = case_when(
      year <= 2001 ~ "Pre-9/11 (1994-2001)",
      year <= 2013 ~ "Post-9/11 (2002-2013)",
      year <= 2024 ~ "Recent Era (2014-2024)",
      TRUE ~ "2025 (Partial)"
    )
  )


## 6. CREATE REFERENCE DATA ----

### |-  Period means by ideology ----
period_means <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(period, ideology) |>
  summarize(
    mean_attacks = mean(attacks),
    mean_per_capita = mean(attacks_per_million),
    total_attacks = sum(attacks),
    .groups = "drop"
  )

### |-  Overall period statistics ----
period_totals <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(period, year) |>
  summarize(total = sum(attacks), .groups = "drop") |>
  group_by(period) |>
  summarize(
    period_mean = mean(total),
    period_start = min(year),
    period_end = max(year),
    .groups = "drop"
  )

### |-  Key event markers ----
key_events <- tibble(
  event = c("9/11", "Trump Era", "COVID-19 + Floyd", "Jan 6"),
  year = c(2001, 2017, 2020, 2021),
  label = c("9/11\n(2001)", "Trump\nEra", "COVID-19\nFloyd", "Jan 6\n(2021)")
)

### |-  Overall baseline ----
overall_mean <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(year) |>
  summarize(total = sum(attacks)) |>
  pull(total) |>
  mean()


## 7. CREATE SUMMARY DATASETS ----

### |-  Yearly totals ----
terrorism_yearly <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(year) |>
  summarize(
    total_attacks = sum(attacks),
    population = first(population),
    attacks_per_million = (total_attacks / population) * 1000000
  )

### |-  Ideology totals ----
terrorism_by_ideology <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(ideology) |>
  summarize(
    total_attacks = sum(attacks),
    mean_attacks = mean(attacks),
    median_attacks = median(attacks),
    pct_of_all = (total_attacks / sum(terrorism_yearly$total_attacks)) * 100
  ) |>
  arrange(desc(total_attacks))


## 8. KEY INSIGHT: RAW vs PER CAPITA ----

comparison <- terrorism_yearly |>
  filter(year %in% c(1994:2001, 2020:2024)) |>
  mutate(period = ifelse(year <= 2001, "Early", "Recent")) |>
  group_by(period) |>
  summarize(
    mean_raw = mean(total_attacks),
    mean_per_capita = mean(attacks_per_million)
  ) |>
  mutate(
    raw_change_pct = ((mean_raw - first(mean_raw)) / first(mean_raw)) * 100,
    per_capita_change_pct = ((mean_per_capita - first(mean_per_capita)) / first(mean_per_capita)) * 100
  )


## 9. SAVE PROCESSED DATA ----

write_csv(terrorism_enriched, "2025/Week_45/terrorism_enriched.csv")
write_csv(period_means, "2025/Week_45/period_means.csv")
write_csv(period_totals, "2025/Week_45/period_totals.csv")
write_csv(key_events, "2025/Week_45/key_events.csv")
write_csv(terrorism_yearly, "2025/Week_45/terrorism_yearly.csv")
write_csv(terrorism_by_ideology, "2025/Week_45/terrorism_by_ideology.csv")

