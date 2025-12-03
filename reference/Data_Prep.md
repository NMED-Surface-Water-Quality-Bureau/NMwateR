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
