#### R script used to load example files and save as RDA
# Developed by Ben Block, Tetra Tech; Ben.Block@tetratech.com
# Date created: 11/07/2025
# Date last updated: 11/17/2025
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"

# Libraries needed
library(readxl)
library(readr)

# Declare directories ####
(wd <- getwd())
input.dir <- "data-raw/Input_Files"

# specify input files
fn.data1 <- "CriteriaNM_FINAL_2025 SCB LG_MZ.csv"
fn.data2 <- "ParameterYearAssessNM_ALL.csv"
fn.data3 <- "RStudio_Data_File_Sacramentos_rawSQUID.xlsx"
fn.data4 <- "Stations DU Analysis_07-29-25_09_14_24.xlsx"
fn.data5 <- "LTD_Assessment_Report_Sacramentos_rawSQUID.xlsx"
fn.data6 <- "LakeDepthProfile_07-29-25_09_16_39.xlsx"
fn.data7 <- "DU_LANL_Stations_2025.csv"
fn.data8 <- "N3B Data received.xlsx"

# Read data files ####
example_criteria_table <- readr::read_csv(file.path(wd, input.dir, fn.data1)
                               , na = c("NA",""), trim_ws = TRUE, skip = 0
                               , col_names = TRUE, guess_max = 100000)

example_parameter_table <- readr::read_csv(file.path(wd, input.dir, fn.data2)
                                 , na = c("NA",""), trim_ws = TRUE, skip = 0
                                 , col_names = TRUE, guess_max = 100000)

example_SQUID_RStudio_table <- readxl::read_excel(file.path(wd, input.dir, fn.data3)
                              , na = c("NA",""), trim_ws = TRUE, skip = 0
                              , col_names = TRUE, guess_max = 100000)

example_SQUID_DU_table <- readxl::read_excel(file.path(wd, input.dir, fn.data4)
                            , na = c("NA",""), trim_ws = TRUE, skip = 0
                            , col_names = TRUE, guess_max = 100000)

example_SQUID_LTD_table <- readxl::read_excel(file.path(wd, input.dir, fn.data5)
                             , na = c("NA",""), trim_ws = TRUE, skip = 0
                             , col_names = TRUE, guess_max = 100000)

example_SQUID_LakeProfile_table <- readxl::read_excel(file.path(wd, input.dir
                                                                , fn.data6)
                                 , na = c("NA",""), trim_ws = TRUE, skip = 0
                                 , col_names = TRUE, guess_max = 100000)

example_LANL_DU_table <- readr::read_csv(file.path(wd, input.dir, fn.data7)
                                                      , na = c("NA","")
                                            , trim_ws = TRUE, skip = 0
                                            , col_names = TRUE, guess_max = 100000)

example_LANL_WQ_table <- readxl::read_excel(file.path(wd, input.dir, fn.data8)
                                              , na = c("NA",""), trim_ws = TRUE, skip = 0
                                              , col_names = TRUE, guess_max = 100000)

# cleanup
rm(fn.data1, fn.data2, fn.data3, fn.data4, fn.data5, fn.data6, fn.data7
   , fn.data8, input.dir)

# Save in package ####
usethis::use_data(example_criteria_table, overwrite = TRUE)
usethis::use_data(example_parameter_table, overwrite = TRUE)
usethis::use_data(example_SQUID_RStudio_table, overwrite = TRUE)
usethis::use_data(example_SQUID_DU_table, overwrite = TRUE)
usethis::use_data(example_SQUID_LTD_table, overwrite = TRUE)
usethis::use_data(example_SQUID_LakeProfile_table, overwrite = TRUE)
usethis::use_data(example_LANL_DU_table, overwrite = TRUE)
usethis::use_data(example_LANL_WQ_table, overwrite = TRUE)
