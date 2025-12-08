# Analysis of bacteria data against PCR and SCR standards

This function compares bacteria data against water quality standards for
primary contact recreation (PCR) and secondary contact recreation (SCR)
uses. For more information, see the NMED Consolidated Assessment and
Listing Methodology (CALM) guidance manual.

## Usage

``` r
Bacteria_PCR_SCR(Chem_table, DU_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

## Value

A dataframe containing analyzed bacteria data compared to PCR and SCR
water quality criteria.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed

df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
, DU_table = df_DU_processed)
#> [1] "W_NM-212-02"
#> [1] 2287
#> [1] "NM-2114_00"
#> [1] "2287a"
#> [1] "2287b"
#> [1] "W_NM-209-05"
#> [1] 2287
#> [1] "W_NM-210-05"
#> [1] 2287
#> [1] "W_NM-202-06"
#> [1] 2287
#> [1] "W_NM-208-02"
#> [1] 2287
#> [1] "W_NM-211-03"
#> [1] 2287
#> [1] "NM-2117_10"
#> [1] "2287a"
#> [1] "2287b"
#> [1] "NM-2112.B_00"
#> [1] "2287a"
#> [1] "2287b"
#> [1] "W_NM-202-04.B"
#> [1] 2287
#> [1] "W_NM-208-01"
#> [1] 2287
#> [1] "W_NM-210-04"
#> [1] 2287
#> [1] "W_NM-206-01"
#> [1] 2287
#> [1] "W_NM-208-04.A"
#> [1] 2287
#> [1] "W_NM-206-03"
#> [1] 2287
#> [1] "W_NM-207-04"
#> [1] 2287
#> [1] "W_NM-207-01"
#> [1] 2287
#> [1] "W_NM-215-02"
#> [1] 2287
#> [1] "W_NM-208-04.B"
#> [1] 2287
#> [1] "W_NM-208-03"
#> [1] 2287
#> [1] "W_NM-204-01"
#> [1] 2287
#> [1] "W_NM-213-05"
#> [1] 2287
#> [1] "W_NM-214-04"
#> [1] 2287
#> [1] "W_NM-214-02"
#> [1] 2287
#> [1] "W_NM-216-02.B"
#> [1] 2287
#> [1] "W_NM-216-02.A"
#> [1] 2287
```
