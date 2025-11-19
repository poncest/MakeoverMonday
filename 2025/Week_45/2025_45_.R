## Challenge: #MakeoverMonday 2025 week 45
## Data:      Terrorism and Political Violence in the United States  
## Author:    Steven Ponce
## Date:      2025-11-18

## Article
# https://www.csis.org/analysis/left-wing-terrorism-and-political-violence-united-states-what-data-tells-us

## Data
# https://data.world/makeovermonday/2025w45-terrorism-and-political-violence-in-the-usa
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
# Retrieved via tidycensus R package.
## NOTE: This script uses custom helper functions for theming and formatting.
##       See "HELPER FUNCTIONS DOCUMENTATION" section at the end for details

## 1. LOAD PACKAGES & SETUP ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Easily Install and Load the 'Tidyverse'
  janitor, # Simple Tools for Examining and Cleaning Dirty Data
  skimr, # Compact and Flexible Summaries of Data
  scales, # Scale Functions for Visualization
  ggtext, # Improved Text Rendering Support for 'ggplot2'
  showtext, # Using Fonts More Easily in R Graphs
  glue, # Interpreted String Literals
  patchwork, # The Composer of Plots
  ggrepel # Automatically Position Non-Overlapping Text Labels
)

### |- figure size ----
camcorder::gg_record(
  dir    = here::here("temp_plots"),
  device = "png",
  width  = 10,
  height = 7,
  units  = "in",
  dpi    = 320
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

### |-  METHOD: Using tidycensus package to fetch official Census data ----
# The Population Estimates Program (PEP) provides annual population estimates
# For reproducibility: Census API key can be obtained free at:
# https://api.census.gov/data/key_signup.html
# Store your key with: census_api_key("YOUR_KEY_HERE", install = TRUE)

cat("\n=== Fetching US Census Population Data via tidycensus ===\n")

### |-  NOTE on get_estimates() behavior ----
# - For years 2019 and earlier: Uses Census API
# - For years 2020 and later: Reads flat files from Census website
# - Column names and structure differ between the two methods
# - time_series = TRUE returns full time series with DATE column

###  |-  Get 1990s-2000s population estimates (1994-2009) ----  
# API not available before 2015 via tidycensus
# Using official Census Bureau intercensal estimates
# These are the final, authoritative population estimates that reconcile
# postcensal estimates with actual decennial census counts (1990 and 2000)
#
# Source: US Census Bureau, Population Division
# - 1990-2000: Intercensal Estimates
#   https://www.census.gov/data/tables/time-series/demo/popest/intercensal-national.html
# - 2000-2010: Intercensal Estimates  
#   https://www.census.gov/data/tables/time-series/demo/popest/intercensal-2000-2010-national.html
#
# These values match those used by USAFacts and other authoritative sources
# Citation: US Census Bureau, Population Division, Intercensal Estimates

cat("  Using 1994-2009 intercensal estimates...\n")

us_pop_1994_2009 <- tibble(
  year = 1994:2009,
  population = c(
    # 1994-1999: From 1990-2000 Intercensal Estimates (July 1)
    263125821,  # 1994
    266278393,  # 1995
    269394284,  # 1996  
    272657333,  # 1997
    275854104,  # 1998
    279040168,  # 1999
    # 2000-2009: From 2000-2010 Intercensal Estimates (July 1)
    282162411,  # 2000 (April 1, 2000 Census: 281,421,906; July 1: 282,162,411)
    284968955,  # 2001
    287625193,  # 2002
    290107933,  # 2003
    292805298,  # 2004
    295516599,  # 2005
    298379912,  # 2006
    301231207,  # 2007
    304093966,  # 2008
    306771529   # 2009
  )
)

cat(glue("  Loaded 1994-2009: {nrow(us_pop_1994_2009)} years (intercensal estimates)"), "\n")

### |-  Get 2010s population estimates (2010-2019) ----
# Using vintage 2019 time series - this works via API
us_pop_2010s <- get_estimates(
  geography = "us",
  product = "population",
  vintage = 2019,
  time_series = TRUE
) |>
  filter(variable == "POP") |>
  mutate(year = 2010 + DATE) |>
  select(year, population = value)

cat(glue("  Fetched 2010-2019 via API: {nrow(us_pop_2010s)} years"), "\n")

### |-  Get 2020s population estimates (2020-2023) ----
# For 2020+, get_estimates() reads flat files (no API)
# Variables change: use "POPESTIMATE" instead of "POP"
us_pop_2020s_list <- map_dfr(2020:2023, function(yr) {
  get_estimates(
    geography = "us",
    variables = "POPESTIMATE",  # For 2020+, use variables (no product)
    vintage = 2023,
    year = yr
  ) |>
    mutate(year = yr) |>
    select(year, population = value)
})

us_pop_2020s <- us_pop_2020s_list |>
  distinct(year, .keep_all = TRUE)

cat(glue("  Fetched 2020-2023 via flat file: {nrow(us_pop_2020s)} years"), "\n")

### |-  Combine all years ----
us_population <- bind_rows(
  us_pop_1994_2009,
  us_pop_2010s,
  us_pop_2020s
) |>
  arrange(year) |>
  distinct(year, .keep_all = TRUE) |>
  mutate(source = case_when(
    year <= 2009 ~ "Intercensal Estimates",
    year <= 2019 ~ "PEP via API",
    year <= 2023 ~ "PEP Flat File",
    TRUE ~ "Projected"
  ))

### |-  Estimate 2024-2025 (not yet available from Census) ----
# Use linear projection based on recent growth rate (2020-2023)
recent_growth_rate <- (
  us_population$population[us_population$year == 2023] - 
    us_population$population[us_population$year == 2020]
) / us_population$population[us_population$year == 2020] / 3

pop_2023 <- us_population$population[us_population$year == 2023]
pop_2024_proj <- pop_2023 * (1 + recent_growth_rate)
pop_2025_proj <- pop_2024_proj * (1 + recent_growth_rate)

us_pop_projected <- tibble(
  year = c(2024, 2025),
  population = c(pop_2024_proj, pop_2025_proj),
  source = "Projected"
)

### |-  Final combined population dataset ----
us_population <- us_population |>
  bind_rows(us_pop_projected) |>
  arrange(year)

cat("\n=== Population Data Summary ===\n")
cat(glue("  1994: {comma(us_population$population[us_population$year == 1994])}"), "\n")
cat(glue("  2023: {comma(us_population$population[us_population$year == 2023])}"), "\n")
cat(glue("  2025: {comma(us_population$population[us_population$year == 2025], accuracy = 1)} (projected)"), "\n")
pop_growth <- (us_population$population[us_population$year == 2025] / 
                 us_population$population[us_population$year == 1994]) - 1
cat(glue("  Growth 1994-2025: {percent(pop_growth, accuracy = 0.1)}"), "\n\n")

## 5. CALCULATE ANALYTICAL METRICS ----

### |-  Join population data ----
terrorism_enriched <- terrorism_usa_long |>
  left_join(us_population, by = "year")

### |-  Calculate per capita rates ----
terrorism_enriched <- terrorism_enriched |>
  mutate(attacks_per_million = (attacks / population) * 1000000)

### |-  Calculate percentage of year total ----
terrorism_enriched <- terrorism_enriched |>
  group_by(year) |>
  mutate(pct_of_year = (attacks / sum(attacks)) * 100) |>
  ungroup()

### |-  Calculate cumulative totals ----
terrorism_enriched <- terrorism_enriched |>
  arrange(ideology, year) |>
  group_by(ideology) |>
  mutate(cumulative_attacks = cumsum(attacks)) |>
  ungroup()

### |-  Add period classifications ----
terrorism_enriched <- terrorism_enriched |>
  mutate(
    period = case_when(
      year <= 2001 ~ "Pre-9/11 (1994-2001)",
      year <= 2013 ~ "Post-9/11 (2002-2013)",
      year <= 2024 ~ "Recent Era (2014-2024)",
      TRUE ~ "2025 (Partial)"
    )
  )

cat("✓ Calculated analytical metrics\n")
cat("  • Attacks per million population\n")
cat("  • Percentage composition by year\n")
cat("  • Cumulative attack totals\n")
cat("  • Period classifications\n\n")

## 6. CREATE REFERENCE DATA ----

### |-  Period means by ideology ----
period_means <- terrorism_enriched |>
  filter(year <= 2024) |>  # Exclude partial 2025
  group_by(period, ideology) |>
  summarize(
    mean_attacks = mean(attacks, na.rm = TRUE),
    mean_per_capita = mean(attacks_per_million, na.rm = TRUE),
    total_attacks = sum(attacks, na.rm = TRUE),
    .groups = "drop"
  )

### |-  Overall period statistics ----
period_totals <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(period, year) |>
  summarize(total = sum(attacks, na.rm = TRUE), .groups = "drop") |>
  group_by(period) |>
  summarize(
    period_mean = mean(total, na.rm = TRUE),
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
  summarize(mean = mean(total)) |>
  pull(mean)

cat("=== Reference Statistics ===\n")
cat(glue("Overall mean (1994-2024): {round(overall_mean, 1)} attacks/year\n\n"))
print(period_totals)

## 7. CREATE SUMMARY DATASETS ----

### |-  Yearly totals with per capita ----
terrorism_yearly <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(year) |>
  summarize(
    total_attacks = sum(attacks),
    population = first(population),
    attacks_per_million = (total_attacks / population) * 1000000,
    .groups = "drop"
  )

### |-  Ideology totals ----
terrorism_by_ideology <- terrorism_enriched |>
  filter(year <= 2024) |>
  group_by(ideology) |>
  summarize(
    total_attacks = sum(attacks),
    mean_attacks = mean(attacks),
    median_attacks = median(attacks),
    pct_of_all = (total_attacks / sum(terrorism_yearly$total_attacks)) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(total_attacks))

cat("\n=== Summary by Ideology (1994-2024) ===\n")
print(terrorism_by_ideology)

## 8. KEY INSIGHT: RAW vs PER CAPITA ----

cat("\n=== Raw Counts vs Per Capita Comparison ===\n")

comparison <- terrorism_yearly |>
  summarize(
    period = c("Early (1994-2001)", "Recent (2020-2024)"),
    mean_raw = c(
      mean(total_attacks[year >= 1994 & year <= 2001]),
      mean(total_attacks[year >= 2020 & year <= 2024])
    ),
    mean_per_capita = c(
      mean(attacks_per_million[year >= 1994 & year <= 2001]),
      mean(attacks_per_million[year >= 2020 & year <= 2024])
    )
  ) |>
  mutate(
    raw_change_pct = ((mean_raw - first(mean_raw)) / first(mean_raw)) * 100,
    per_capita_change_pct = ((mean_per_capita - first(mean_per_capita)) / first(mean_per_capita)) * 100
  )

print(comparison)

cat("\n*** KEY INSIGHT ***\n")
cat(glue("Raw count increase: {percent(comparison$raw_change_pct[2]/100, accuracy = 0.1)}"), "\n")
cat(glue("Per capita increase: {percent(comparison$per_capita_change_pct[2]/100, accuracy = 0.1)}"), "\n")
pop_effect_pct <- (comparison$raw_change_pct[2] - comparison$per_capita_change_pct[2]) / comparison$raw_change_pct[2]
cat(glue("Population growth explains {percent(pop_effect_pct, accuracy = 1)} of the raw count increase"), "\n\n")

## 9. SAVE PROCESSED DATA ----

write_csv(terrorism_enriched, "terrorism_enriched.csv")
write_csv(period_means, "period_means.csv")
write_csv(period_totals, "period_totals.csv")
write_csv(key_events, "key_events.csv")
write_csv(terrorism_yearly, "terrorism_yearly.csv")
write_csv(terrorism_by_ideology, "terrorism_by_ideology.csv")
write_csv(us_population, "us_population.csv")  # Save for reference

cat("=== Files Saved ===\n")
cat("✓ terrorism_enriched.csv\n")
cat("✓ period_means.csv\n")
cat("✓ period_totals.csv\n")
cat("✓ key_events.csv\n")
cat("✓ terrorism_yearly.csv\n")
cat("✓ terrorism_by_ideology.csv\n")
cat("✓ us_population.csv\n\n")

cat("=== DATA PREPARATION COMPLETE ===\n")
cat("Ready for visualization exploration!\n")

## 10. SESSION INFO ----
sessionInfo()