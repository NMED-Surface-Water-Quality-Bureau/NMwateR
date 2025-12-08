# Analysis of pH data against PCR standards

This function compares pH data against water quality standards for
primary contact recreation (PCR) use. For more information, see the NMED
Consolidated Assessment and Listing Methodology (CALM) guidance manual.

## Usage

``` r
pH_PCR(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed pH data compared to PCR water quality
criteria.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed

df_pH_PCR <- pH_PCR(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)
#> [1] "W_NM-212-02"
#> [1] 1648
#> [1] "W_NM-209-05"
#> [1] 1648
#> [1] "W_NM-210-05"
#> [1] 1648
#> [1] "W_NM-202-06"
#> [1] 1648
#> [1] "W_NM-208-02"
#> [1] 1648
#> [1] "W_NM-211-03"
#> [1] 1648
#> [1] "W_NM-202-04.B"
#> [1] 1648
#> [1] "W_NM-208-01"
#> [1] 1648
#> [1] "W_NM-210-04"
#> [1] 1648
#> [1] "W_NM-206-01"
#> [1] 1648
#> [1] "W_NM-208-04.A"
#> [1] 1648
#> [1] "W_NM-206-08"
#> [1] 1648
#> [1] "W_NM-206-03"
#> [1] 1648
#> [1] "W_NM-207-04"
#> [1] 1648
#> [1] "W_NM-207-01"
#> [1] 1648
#> [1] "W_NM-215-02"
#> [1] 1648
#> [1] "W_NM-208-04.B"
#> [1] 1648
#> [1] "W_NM-208-03"
#> [1] 1648
#> [1] "W_NM-204-01"
#> [1] 1648
#> [1] "W_NM-213-05"
#> [1] 1648
#> [1] "W_NM-214-04"
#> [1] 1648
#> [1] "W_NM-214-02"
#> [1] 1648
#> [1] "W_NM-216-02.B"
#> [1] 1648
#> [1] "W_NM-216-02.A"
#> [1] 1648
#> [1] "NM-2114_00"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2117_10"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.B_00"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_030"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_010"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_070"
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
```
