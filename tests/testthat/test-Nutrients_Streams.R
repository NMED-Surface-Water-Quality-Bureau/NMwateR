test_that("Nutrients Streams calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed

  ## Nutrients (Streams) ####
  Nutrients_Streams_list <- Nutrients_Streams(Chem_table = df_Chem_combined
                                              , DU_table = df_DU_processed)

  df_Nutrients_Streams <- Nutrients_Streams_list$Nutrients_Streams

  n_row_code <- nrow(df_Nutrients_Streams)
  n_unique_WATER_ID_code <- length(unique(df_Nutrients_Streams$WATER_ID))
  n_unique_Category_code <- length(unique(df_Nutrients_Streams$Category))

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 46
  n_unique_WATER_ID_dev <- 46
  n_unique_Category_dev <- 3

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(n_unique_WATER_ID_code, n_unique_WATER_ID_dev)
  testthat::expect_equal(n_unique_Category_code, n_unique_Category_dev)

})
