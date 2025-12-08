# Analysis of hardness dependent toxics data against ALU standards

This function compares hardness dependent toxics data against water
quality standards for aquatic life use (ALU). For more information, see
the NMED Consolidated Assessment and Listing Methodology (CALM) guidance
manual.

## Usage

``` r
Toxics_ALU_HDM(Chem_table, DU_table, Criteria_table)
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

A list of two dataframes. The first contains analyzed hardness dependent
toxics data compared to LW water quality criteria. The second, labeled
"Indiv_Res" is an intermediate file used for QA/QC purposes.

## Examples

``` r
df_Chem_combined <- NMwateR::example_chemistry_processed
df_DU_processed <- NMwateR::example_DU_processed
df_Criteria <- NMwateR::example_criteria_processed

Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
, DU_table = df_DU_processed
, Criteria_table = df_Criteria)
#> [1] "W_NM-212-02"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "NM-2114_00"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-209-05"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-210-05"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-202-06"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-208-02"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-211-03"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "NM-2117_10"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "NM-2112.B_00"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-202-04.B"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-208-01"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-210-04"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-206-01"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-208-04.A"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-206-03"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-207-04"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-207-01"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-215-02"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-208-04.B"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-208-03"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-204-01"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-213-05"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-214-04"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-214-02"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-216-02.B"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832
#> [1] "W_NM-216-02.A"
#> [1] 1215
#> [1] 1249
#> [1] 1395
#> [1] 1793
#> [1] 2005
#> [1] 549002
#> [1] 725
#> [1] 832

df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM
df_Toxics_ALU_HDM_Indiv_Res <- Toxics_ALU_HDM_list$Toxics_ALU_HDM_Indiv_Res
```
