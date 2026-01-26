# ============================================================================
# Makeover Monday 2025 Week 04 - Data Merge & Enhancement
# Combine: Lightcast Rankings + BLS OEWS Employment + State Population
# ============================================================================

## 1. LOAD PACKAGES ----
if (!require("pacman")) install.packages("pacman
")
pacman::p_load(
  tidyverse,
  janitor
)

## 2. READ LIGHTCAST DATA ----

top_jobs_raw <- read_csv("data/2026/LIGHTCAST_TOP JOB POSTED FOR BY STATE OCT 24-25.csv") |>
  clean_names()

# Reshape to long format
lightcast_long <- top_jobs_raw |>
  pivot_longer(
    cols = c(x1st_place, x2nd_place, x3rd_place),
    names_to = "rank",
    values_to = "job_title"
  ) |>
  mutate(
    rank = case_when(
      rank == "x1st_place" ~ 1L,
      rank == "x2nd_place" ~ 2L,
      rank == "x3rd_place" ~ 3L
    ),
    # Standardize job titles to match BLS categories
    job_category = case_when(
      job_title == "Registered Nurse" ~ "Registered Nurse",
      job_title == "Retail Sales Associate" ~ "Retail Sales Associate",
      job_title == "Tractor-Trailer Truck Driver" ~ "Tractor-Trailer Truck Driver",
      job_title == "Physician" ~ "Physician",
      job_title == "Software Developer / Engineer" ~ "Software Developer / Engineer",
      job_title == "Customer Service Representative" ~ "Customer Service Representative",
      job_title == "Licensed Practical / Vocational Nurse" ~ "Licensed Practical / Vocational Nurse",
      job_title == "Retail Store Manager / Supervisor" ~ "Retail Store Manager / Supervisor",
      TRUE ~ job_title
    )
  )

# Quick check
cat("\n=== LIGHTCAST JOB TITLES ===\n")
lightcast_long |> count(job_category, sort = TRUE) |> print()

## 3. READ BLS OEWS DATA ----
oews_state_jobs <- read_csv("data/2026/bls_oews_state_jobs_processed.csv")

cat("\n=== BLS OEWS JOB CATEGORIES ===\n")
oews_state_jobs |> count(job_category, sort = TRUE) |> print()

## 4. STATE POPULATION DATA ----
# US Census Bureau, 2023 Population Estimates
# Source: https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html

state_pop <- tribble(
  ~state_name, ~population,
  "Alabama", 5108468,
  "Alaska", 733406,
  "Arizona", 7431344,
  "Arkansas", 3067732,
  "California", 38965193,
  "Colorado", 5877610,
  "Connecticut", 3617176,
  "Delaware", 1031890,
  "Florida", 23372215,
  "Georgia", 11029227,
  "Hawaii", 1435138,
  "Idaho", 1964726,
  "Illinois", 12549689,
  "Indiana", 6862199,
  "Iowa", 3207004,
  "Kansas", 2940546,
  "Kentucky", 4526154,
  "Louisiana", 4573749,
  "Maine", 1395722,
  "Maryland", 6180253,
  "Massachusetts", 7001399,
  "Michigan", 10037261,
  "Minnesota", 5737915,
  "Mississippi", 2939690,
  "Missouri", 6196156,
  "Montana", 1132812,
  "Nebraska", 1978379,
  "Nevada", 3194176,
  "New Hampshire", 1402054,
  "New Jersey", 9290841,
  "New Mexico", 2114371,
  "New York", 19571216,
  "North Carolina", 10835491,
  "North Dakota", 783926,
  "Ohio", 11785935,
  "Oklahoma", 4053824,
  "Oregon", 4233358,
  "Pennsylvania", 12961683,
  "Rhode Island", 1095962,
  "South Carolina", 5373555,
  "South Dakota", 919318,
  "Tennessee", 7126489,
  "Texas", 30503301,
  "Utah", 3417734,
  "Vermont", 647464,
  "Virginia", 8683619,
  "Washington", 7812880,
  "West Virginia", 1770071,
  "Wisconsin", 5910955,
  "Wyoming", 584057
)

## 5. MERGE ALL DATA SOURCES ----

# Join OEWS employment data to Lightcast rankings
enhanced_data <- lightcast_long |>
  left_join(
    oews_state_jobs,
    by = c("state_name", "job_category")
  ) |>
  left_join(
    state_pop,
    by = "state_name"
  ) |>
  mutate(
    # Calculate per capita metrics (per 100,000 population)
    emp_per_100k = (tot_emp / population) * 100000
  )

# Check for missing BLS data (Physician, Retail Store Manager not in BLS pull)
cat("\n=== MISSING BLS DATA ===\n")
enhanced_data |>
  filter(is.na(tot_emp)) |>
  count(job_category) |>
  print()

## 6. CREATE SUMMARY DATASETS ----

# Dataset 1: State-level summary (focus on what we have data for)
state_summary <- enhanced_data |>
  group_by(state_name, population) |>
  summarise(
    top_job = first(job_category[rank == 1]),
    second_job = first(job_category[rank == 2]),
    third_job = first(job_category[rank == 3]),
    top_job_emp = first(tot_emp[rank == 1]),
    second_job_emp = first(tot_emp[rank == 2]),
    third_job_emp = first(tot_emp[rank == 3]),
    top_job_wage = first(a_mean[rank == 1]),
    second_job_wage = first(a_mean[rank == 2]),
    .groups = "drop"
  ) |>
  mutate(
    # Per capita top job (RN for all states)
    top_job_per_100k = (top_job_emp / population) * 100000,
    # Gap analysis (where we have data for #2)
    emp_gap_1_2 = top_job_emp - second_job_emp,
    emp_ratio_1_2 = top_job_emp / second_job_emp
  )

# Dataset 2: Job category national summary
job_summary <- enhanced_data |>
  filter(!is.na(tot_emp)) |>
  group_by(job_category) |>
  summarise(
    times_ranked_1 = sum(rank == 1),
    times_ranked_2 = sum(rank == 2),
    times_ranked_3 = sum(rank == 3),
    times_in_top3 = n(),
    total_national_emp = sum(tot_emp, na.rm = TRUE),
    avg_state_emp = mean(tot_emp, na.rm = TRUE),
    avg_wage = weighted.mean(a_mean, tot_emp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(times_ranked_1), desc(total_national_emp))

# Dataset 3: Wide format for easy state comparison
state_wide <- state_summary |>
  select(
    state_name, population,
    top_job, top_job_emp, top_job_wage, top_job_per_100k,
    second_job, second_job_emp, second_job_wage,
    third_job, third_job_emp,
    emp_ratio_1_2
  )

## 7. EXPLORATION OUTPUT ----
cat("\n" , strrep("=", 60), "\n")
cat("KEY FINDINGS\n")
cat(strrep("=", 60), "\n")

cat("\n=== JOB CATEGORY SUMMARY ===\n")
print(job_summary, n = 10)

cat("\n=== TOP 10 STATES BY RN EMPLOYMENT (raw) ===\n")
state_summary |>
  arrange(desc(top_job_emp)) |>
  select(state_name, top_job_emp, population, top_job_per_100k) |>
  head(10) |>
  print()

cat("\n=== TOP 10 STATES BY RN CONCENTRATION (per 100k) ===\n")
state_summary |>
  arrange(desc(top_job_per_100k)) |>
  select(state_name, top_job_per_100k, top_job_emp, population) |>
  head(10) |>
  print()

cat("\n=== BOTTOM 10 STATES BY RN CONCENTRATION (per 100k) ===\n")
state_summary |>
  arrange(top_job_per_100k) |>
  select(state_name, top_job_per_100k, top_job_emp, population) |>
  head(10) |>
  print()

cat("\n=== #2 JOB DISTRIBUTION ===\n")
state_summary |>
  count(second_job, sort = TRUE) |>
  print()

cat("\n=== TECH HUB STATES (Software Dev as #2) ===\n")
state_summary |>
  filter(second_job == "Software Developer / Engineer") |>
  select(state_name, second_job_emp, second_job_wage, population) |>
  arrange(desc(second_job_emp)) |>
  print()

cat("\n=== TRUCKING STATES (Truck Driver as #2) ===\n")
state_summary |>
  filter(second_job == "Tractor-Trailer Truck Driver") |>
  select(state_name, second_job_emp, second_job_wage, population) |>
  arrange(desc(second_job_emp)) |>
  head(10) |>
  print()

## 8. SAVE ENHANCED DATASETS ----

write_csv(enhanced_data, "data/2026/mm_wk04_enhanced_long.csv")
write_csv(state_summary, "data/2026/mm_wk04_state_summary.csv")
write_csv(state_wide, "data/2026/mm_wk04_state_wide.csv")
write_csv(job_summary, "data/2026/mm_wk04_job_summary.csv")


## 9. DATA SOURCES REFERENCE ----
cat("\n
============================================================
DATA SOURCES REFERENCE
============================================================

1. LIGHTCAST JOB POSTINGS (Primary - Makeover Monday)
   Source: Lightcast Job Posting Analytics
   Article: 'Job titles in highest-demand, state by state'
   URL: https://lightcast.io/resources/blog/most-posted-for-jobs-in-each-us-state
   Data: data.world/makeovermonday/2025w04
   Period: October 2024 - October 2025
   Note: Rankings only (1st, 2nd, 3rd most posted jobs by state)

2. BLS OCCUPATIONAL EMPLOYMENT & WAGE STATISTICS (Enhancement)
   Source: U.S. Bureau of Labor Statistics
   Dataset: May 2024 State Occupational Employment and Wage Estimates
   URL: https://www.bls.gov/oes/current/oessrcst.htm
   Occupations: RN, Truck Driver, Retail Sales, Software Dev, CSR, LPN
   Note: Actual employment counts and mean wages by state

3. U.S. CENSUS BUREAU POPULATION (Enhancement)
   Source: Population Estimates Program
   Year: 2023 estimates (released Dec 2023)
   URL: https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
   Note: Used for per capita calculations

============================================================
")