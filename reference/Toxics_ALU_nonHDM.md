# Analysis of non hardness dependent toxics data against ALU standards

This function compares non hardness dependent toxics data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Toxics_ALU_nonHDM(Chem_table, DU_table, Criteria_table)
```

## Arguments

- Chem_table:

  Compiled water chemistry data from Data_Prep function. Usually
  contains a combination of grab, LTD, and lake profile data.

- DU_table:

  Quality controlled Stations DU table from Data_Prep function.

- Criteria_table:

  Quality controlled criteria table from Data_Prep function.

## Value

A dataframe containing analyzed non hardness dependent toxics data
compared to ALU water quality criteria.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed
df_Criteria <- NMwateR::example_criteria_processed

df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
, DU_table = df_DU_processed
, Criteria_table = df_Criteria)
#> [1] "W_NM-212-02"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "NM-2114_00"
#> [1] 1266001
#> [1] 1633
#> [1] 1783002
#> [1] 2414
#> [1] 518
#> [1] 549001
#> [1] 591
#> [1] "W_NM-209-05"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-210-05"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-202-06"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-208-02"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-211-03"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "NM-2117_10"
#> [1] 1266001
#> [1] 1633
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] 518
#> [1] "NM-2112.B_00"
#> [1] 1266001
#> [1] 1633
#> [1] 1783002
#> [1] 2414
#> [1] 518
#> [1] 549001
#> [1] 591
#> [1] "W_NM-202-04.B"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-208-01"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-210-04"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-206-01"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-208-04.A"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-206-08"
#> [1] 1783002
#> [1] 2414
#> [1] "W_NM-206-03"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-207-04"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-207-01"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-215-02"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-208-04.B"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-208-03"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-204-01"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-213-05"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-214-04"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-214-02"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-216-02.B"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
#> [1] "W_NM-216-02.A"
#> [1] 1266001
#> [1] 1783002
#> [1] 2414
#> [1] 549001
#> [1] 591
```
