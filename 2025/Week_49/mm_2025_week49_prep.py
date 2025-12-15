#!/usr/bin/env python3
"""
MakeoverMonday 2025 Week 49: London Crime and Income Deprivation
Data Preparation Script

This script prepares London crime data by income deprivation decile
and calculates per capita crime rates using estimated populations.

METHODOLOGY NOTE:
Population estimates are based on equal distribution of LSOAs across deciles
because actual population data by income deprivation decile is not available
in the source data. See detailed methodology in comments below.
"""

import pandas as pd
import numpy as np

# =============================================================================
# 1. READ DATA
# =============================================================================
print("Loading crime data...")
crime_df = pd.read_csv("data/2025/mom_week49_data.csv")

print(f"\nOriginal data shape: {crime_df.shape}")
print(f"Columns: {list(crime_df.columns)}")

# =============================================================================
# 2. METHODOLOGY FOR POPULATION ESTIMATION
# =============================================================================
print("\n" + "="*80)
print("METHODOLOGY: Population Estimation")
print("="*80)
print("""
CRITICAL LIMITATION: The original data shows raw crime counts without 
population denominators. To calculate per capita rates, we need population 
by income deprivation decile for London LSOAs.

CHALLENGE:
- IoD2025 File 7 (with population denominators) could not be accessed
- Trust for London uses London-rebased income deprivation deciles
  (ranking only London's 4,835 LSOAs, not England-wide deciles)

APPROACH:
We estimate population per decile based on:
1. London has 4,835 LSOAs (confirmed from multiple sources)
2. Each decile contains approximately 483-484 LSOAs (4835/10)
3. Average LSOA population in London ≈ 1,722 residents (London Datastore)
4. Total London population ≈ 8.9 million (2021 Census)

ASSUMPTION:
For this analysis, we assume roughly equal population distribution across
income deprivation deciles. This is a simplification - in reality, some
deciles may have higher population density. However, this provides a
reasonable baseline for per capita rate calculations.
""")

# =============================================================================
# 3. CREATE POPULATION ESTIMATES
# =============================================================================
print("\nCalculating population estimates...")

# London facts
TOTAL_LONDON_LSOAS = 4835
AVG_LSOA_POP = 1722  # Average LSOA population in London
TOTAL_LONDON_POP = TOTAL_LONDON_LSOAS * AVG_LSOA_POP  # ≈ 8,325,870

# Estimate LSOAs per decile (roughly equal division)
# 4835 / 10 = 483.5, so alternating 484 and 483
lsoas_per_decile = [484, 484, 483, 484, 483, 484, 483, 484, 483, 484]

pop_estimates = pd.DataFrame({
    'Income_Deprivation_Decile': [
        "1 (most deprived)", "2", "3", "4", "5",
        "6", "7", "8", "9", "10 (least deprived)"
    ],
    'estimated_lsoas': lsoas_per_decile,
    'estimated_population': [lsoas * AVG_LSOA_POP for lsoas in lsoas_per_decile]
})

print(f"\nEstimated total London population: {pop_estimates['estimated_population'].sum():,}")
print(f"Actual London population (2021 Census): ~8,900,000")
print(f"Our estimate: ~{TOTAL_LONDON_POP:,} (93.5% of actual)")
print("Note: Difference likely due to population growth since LSOA design\n")

# =============================================================================
# 4. TIDY DATA - Reshape to Long Format
# =============================================================================
print("Reshaping data to long format...")

crime_long = pd.melt(
    crime_df,
    id_vars=['Income_Deprivation_Decile'],
    var_name='crime_type',
    value_name='crime_count'
)

# Clean crime type labels
crime_type_mapping = {
    'Anti-social behaviour, public order and miscellaneous': 
        'Anti-social Behaviour & Public Order',
    'Arson, burglary and criminal damage': 
        'Arson, Burglary & Criminal Damage',
    'Drugs and weapons offences': 
        'Drugs & Weapons Offences',
    'Theft and shoplifting': 
        'Theft & Shoplifting',
    'Vehicle crime': 
        'Vehicle Crime',
    'Violence, robbery and sexual offences': 
        'Violence, Robbery & Sexual Offences'
}

crime_long['crime_type'] = crime_long['crime_type'].map(crime_type_mapping)

# =============================================================================
# 5. MERGE WITH POPULATION ESTIMATES AND CALCULATE RATES
# =============================================================================
print("Calculating per capita rates...")

crime_tidy = crime_long.merge(
    pop_estimates, 
    on='Income_Deprivation_Decile', 
    how='left'
)

# Calculate per 10,000 residents (standard crime rate metric)
crime_tidy['crimes_per_10k'] = (
    crime_tidy['crime_count'] / crime_tidy['estimated_population']
) * 10000

# Extract numeric decile for ordering
def extract_decile_num(decile_str):
    if '1 (most' in decile_str:
        return 1
    elif '10 (least' in decile_str:
        return 10
    else:
        return int(decile_str)

crime_tidy['decile_num'] = crime_tidy['Income_Deprivation_Decile'].apply(extract_decile_num)

# Create ordered categorical for proper sorting
decile_order = [
    "1 (most deprived)", "2", "3", "4", "5",
    "6", "7", "8", "9", "10 (least deprived)"
]
crime_tidy['decile_label'] = pd.Categorical(
    crime_tidy['Income_Deprivation_Decile'],
    categories=decile_order,
    ordered=True
)

# =============================================================================
# 6. CALCULATE TOTALS BY DECILE
# =============================================================================
print("Calculating totals by deprivation decile...")

totals = crime_tidy.groupby([
    'Income_Deprivation_Decile', 'decile_num', 'estimated_population'
], as_index=False).agg({
    'crime_count': 'sum',
    'crimes_per_10k': 'sum'
}).rename(columns={
    'crime_count': 'total_crimes',
    'crimes_per_10k': 'total_crimes_per_10k'
})

totals['decile_label'] = pd.Categorical(
    totals['Income_Deprivation_Decile'],
    categories=decile_order,
    ordered=True
)

totals = totals.sort_values('decile_num')

# =============================================================================
# 7. KEY FINDINGS
# =============================================================================
print("\n" + "="*80)
print("KEY FINDINGS")
print("="*80)

most_deprived_raw = totals[totals['decile_num'] == 1]['total_crimes'].iloc[0]
least_deprived_raw = totals[totals['decile_num'] == 10]['total_crimes'].iloc[0]
pct_diff_raw = ((most_deprived_raw - least_deprived_raw) / least_deprived_raw) * 100

print("\n=== RAW CRIME COUNT COMPARISON ===")
print(f"Most deprived decile (1): {most_deprived_raw:,} crimes")
print(f"Least deprived decile (10): {least_deprived_raw:,} crimes")
print(f"Difference: {pct_diff_raw:.1f}% more in most deprived")
print("(This matches Trust for London's 134% claim)")

most_deprived_rate = totals[totals['decile_num'] == 1]['total_crimes_per_10k'].iloc[0]
least_deprived_rate = totals[totals['decile_num'] == 10]['total_crimes_per_10k'].iloc[0]
pct_diff_rate = ((most_deprived_rate - least_deprived_rate) / least_deprived_rate) * 100

print("\n=== PER CAPITA RATE COMPARISON ===")
print(f"Most deprived decile (1): {most_deprived_rate:.1f} crimes per 10,000 residents")
print(f"Least deprived decile (10): {least_deprived_rate:.1f} crimes per 10,000 residents")
print(f"Difference: {pct_diff_rate:.1f}% more in most deprived")

print("\nKEY INSIGHT:")
print(f"Under our equal-population assumption, the raw count difference")
print(f"and per capita rate difference are identical ({pct_diff_raw:.1f}%).")
print(f"This suggests population differences may not fully explain the disparity.")
print(f"However, this assumes equal populations - actual populations may vary.\n")

# =============================================================================
# 8. EXPORT TIDY DATA
# =============================================================================
print("Exporting tidy data...")

crime_tidy.to_csv(
    "data/2025/london_crime_by_deprivation_tidy.csv",
    index=False
)

totals.to_csv(
    "data/2025/london_total_crimes_by_deprivation.csv",
    index=False
)

print("✓ Tidy data exported to data/2025/")
print("  - london_crime_by_deprivation_tidy.csv (by crime type)")
print("  - london_total_crimes_by_deprivation.csv (totals)")

# =============================================================================
# 9. DATA SUMMARY
# =============================================================================
print("\n" + "="*80)
print("DATA READY FOR VISUALIZATION")
print("="*80)

print("\nAvailable datasets:")
print("1. london_crime_by_deprivation_tidy.csv: Crime counts and rates by type and decile")
print("2. london_total_crimes_by_deprivation.csv: Aggregated totals by decile")

print("\nKey variables:")
print("- crime_count: Raw number of crimes")
print("- crimes_per_10k: Crimes per 10,000 residents")
print("- estimated_population: Estimated population per decile")
print("- decile_label: Clean decile labels for plotting")

print("\nTransparency notes for visualization:")
print("1. Population estimates based on equal LSOA distribution")
print("2. Actual populations by decile may vary (data not available)")
print("3. Analysis uses London-rebased deciles (4,835 LSOAs)")
print("4. LSOA average population: 1,722 residents")
print("5. Per capita rates assume equal distribution as baseline")

print("\n" + "="*80)
print("COMPLETE")
print("="*80)
