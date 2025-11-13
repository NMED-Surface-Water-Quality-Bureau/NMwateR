#### R script used to test NMED assessment workflow
# Developed by Ben Block, Tetra Tech; Ben.Block@tetratech.com
## Hannah Ferriby, Tetra Tech; Hannah.Ferriby@tetratech.com
### and Kateri Salk, Tetra Tech; Kateri.SalkGundersen@tetratech.com
# Date created: 11/07/2025
# Date last updated: 11/13/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"

# Libraries needed
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(lubridate)
# library(rLakeAnalyzer)
library(devtools)

# specify input files ####
data(example_criteria_table)
data(example_parameter_table)
data(example_SQUID_RStudio_table)
data(example_SQUID_DU_table)
data(example_SQUID_LTD_table)
data(example_SQUID_LakeProfile_table)

# load all
devtools::load_all()

# Data Prep ####
my_data_list <- Data_Prep(criteria_table = example_criteria_table
                          , parameter_table = example_parameter_table
                          , SQUID_RStudio_table = example_SQUID_RStudio_table
                          , SQUID_DU_table = example_SQUID_DU_table
                          , SQUID_LTD_table = example_SQUID_LTD_table
                          , SQUID_LakeProfile_table = example_SQUID_LakeProfile_table)

## Pull out dataframes ####
df_Chem_combined <- my_data_list$Chem_Combined
df_DU_processed <- my_data_list$DU_Processed
df_Criteria <- my_data_list$Criteria_Formatted

# cleanup
rm(example_criteria_table, example_parameter_table, example_SQUID_RStudio_table
   , example_SQUID_DU_table, example_SQUID_LTD_table
   , example_SQUID_LakeProfile_table, my_data_list)

# Conventionals ALU ####
df_Conv_ALU <- Conventionals_ALU(Chem_table = df_Chem_combined
                                 , DU_table = df_DU_processed)




