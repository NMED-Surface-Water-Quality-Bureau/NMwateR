# Analysis of conventionals data against ALU standards

This function compares conventional water chemistry data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Conventionals_ALU(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed conventionals data compared to ALU water
quality criteria.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed
df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)
#> [1] "W_NM-212-02"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-209-05"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-210-05"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-202-06"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-208-02"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-211-03"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-202-04.B"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-208-01"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-210-04"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-206-01"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-208-04.A"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-206-08"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-206-03"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-207-04"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-207-01"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-215-02"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-208-04.B"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-208-03"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-204-01"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-213-05"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-214-04"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-214-02"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-216-02.B"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "W_NM-216-02.A"
#> [1] 1648
#> [1] 1815
#> [1] 2849
#> [1] 985
#> [1] "NM-2114_00"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2117_10"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.B_00"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_50"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_030"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_100"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_010"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.A_20"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_40"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.A_02"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.A_03"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_023"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_011"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_080"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_070"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1815"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648b"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "1648a"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_041"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_001"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2115_00"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_003"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_00"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_002"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_01"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_000"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_110"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_040"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2115_10"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_060"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_10"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2115_20"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_020"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2113_30"
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2112.A_00"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "985"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_072"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
#> [1] "NM-2116.A_112"
#> [1] "2849"
#> Joining with `by = join_by(WATER_ID, CHR_UID, CHR_UID_Unique)`
```
