# Analysis of copper data from LANL against site-specific ALU standards

This function compares copper data from Los Alamos National Laboratory
(LANL) against site-specific water quality standards for certain aquatic
life uses (ALU). For more information, see the NMED Consolidated
Assessment and Listing Methodology (CALM) guidance manual and New Mexico
SQWS.

## Usage

``` r
SS_Copper_ALU(DU_LANL_Stations_table, LANL_WQ_data)
```

## Arguments

- DU_LANL_Stations_table:

  Stations DU table for LANL monitoring locations.

- LANL_WQ_data:

  Water quality data table provided by LANL to NMED.

## Value

A list of three dataframes. The first contains analyzed LANL copper data
compared to site-specific water quality criteria. The second, labeled
"Indiv_Res" is an intermediate file used for QA/QC purposes. The third,
labeled "Insuff_Res" is a dataset of copper criteria with missing
predictor parameters for copper calculations.

## Examples

``` r
example_LANL_DU_table <- NMwateR::example_LANL_DU_table
example_LANL_WQ_table <- NMwateR::example_LANL_WQ_table

SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_LANL_DU_table
, LANL_WQ_data = example_LANL_WQ_table)
#> [1] "NM-9000.A_054"
#> [1] "NM-9000.A_055"
#> [1] "NM-9000.A_046"
#> [1] "NM-128.A_03"
#> [1] "NM-9000.A_005"
#> [1] "NM-2118.A_70"
#> [1] "NM-126.A_03"
#> [1] "NM-97.A_002"
#> [1] "NM-97.A_007"
#> [1] "NM-128.A_14"
#> [1] "NM-128.A_10"
#> [1] "NM-97.A_005"
#> [1] "NM-97.A_003"
#> [1] "NM-9000.A_063"
#> [1] "NM-127.A_00"
#> [1] "NM-9000.A_006"
#> [1] "NM-9000.A_000"
#> [1] "NM-9000.A_049"
#> [1] "NM-9000.A_043"
#> [1] "NM-99.A_001"
#> [1] "NM-97.A_006"
#> [1] "NM-9000.A_045"
#> [1] "NM-97.A_029"
#> [1] "NM-97.A_004"
#> [1] "NM-128.A_00"
#> [1] "NM-128.A_17"
#> [1] "NM-128.A_16"
#> [1] "NM-126.A_01"
#> [1] "NM-128.A_08"
#> [1] "NM-9000.A_040"
#> [1] "NM-128.A_06"
#> [1] "NM-9000.A_048"
#> [1] "NM-128.A_07"
#> [1] "NM-9000.A_091"
#> [1] "NM-128.A_15"
#> [1] "NM-9000.A_053"
#> [1] "NM-9000.A_042"
#> [1] "NM-9000.A_047"
#> [1] "NM-128.A_11"
#> [1] "NM-128.A_01"
#> [1] "NM-126.A_00"
#> [1] "NM-9000.A_051"
#> [1] "NM-128.A_02"
#> [1] "NM-128.A_04"
#> [1] "NM-128.A_05"
#> [1] "NM-128.A_09"
#> [1] "NM-9000.A_044"
#> [1] "NM-9000.A_052"
#> [1] "NM-128.A_12"
#> [1] "NM-128.A_13"
```
