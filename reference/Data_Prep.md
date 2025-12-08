# Compile data from SQUID for water quality analyses

This function compiles multiple tables from SQUID into a single output
used for subsequent water quality analysis functions. The LTD and lake
profile datasets are optional, whereas the RStudio, stations DU,
parameter, and criteria tables are required for the function to run.

## Usage

``` r
Data_Prep(
  criteria_table,
  parameter_table,
  SQUID_RStudio_table,
  SQUID_DU_table,
  SQUID_LTD_table = NULL,
  SQUID_LakeProfile_table = NULL
)
```

## Arguments

- criteria_table:

  Criteria table is output from SQUID. Filters water quality data to
  those in the criteria table. Has criteria to compare data against.

- parameter_table:

  Parameter table is output from SQUID. Filters water quality data to
  those in the parameter table.

- SQUID_RStudio_table:

  RStudio query from SQUID. Entirely grab data from SQUID projects.

- SQUID_DU_table:

  Stations DU table from SQUID. Has DU and AU specific water quality
  criteria which are used in some cases.

- SQUID_LTD_table:

  Optional long-term deployment summary table from SQUID. Not raw data.

- SQUID_LakeProfile_table:

  Optional lake profile table from SQUID.

## Value

A dataframe containing compiled and quality controlled water quality
necessary for subsequent analyses.

## Examples

``` r
example_criteria_table <- NMwateR::example_criteria_table
example_parameter_table <- NMwateR::example_parameter_table
example_SQUID_RStudio_table <- NMwateR::example_SQUID_RStudio_table
example_SQUID_DU_table <- NMwateR::example_SQUID_DU_table
example_SQUID_LTD_table <- NMwateR::example_SQUID_LTD_table
example_SQUID_LakeProfile_table <- NMwateR::example_SQUID_LakeProfile_table

my_data_list <- Data_Prep(criteria_table = example_criteria_table
                          , parameter_table = example_parameter_table
                          , SQUID_RStudio_table = example_SQUID_RStudio_table
                          , SQUID_DU_table = example_SQUID_DU_table
                          , SQUID_LTD_table = example_SQUID_LTD_table
                          , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table)
#> Joining with `by = join_by(WATER_ID, PROJECT_NAME, MLOC_NAME, MLOC_ID,
#> SE_START_DATE_TIME, PRJ_UID, MLOC_UID)`
# cleanup
rm(example_criteria_table, example_parameter_table, example_SQUID_RStudio_table
 , example_SQUID_DU_table, example_SQUID_LTD_table, example_SQUID_LakeProfile_table)

df_Chem_combined <- my_data_list$Chem_Combined
df_DU_processed <- my_data_list$DU_Processed
df_Criteria <- my_data_list$Criteria_Formatted
```
