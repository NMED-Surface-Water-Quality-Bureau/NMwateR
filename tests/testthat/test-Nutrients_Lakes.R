test_that("Nutrients Lakes calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed

  ## Nutrients (Lakes) ####
  Nutrients_Lakes_list <- Nutrients_Lakes(Chem_table = df_Chem_combined
                                          , DU_table = df_DU_processed)

  df_Nutrients_Lakes <- Nutrients_Lakes_list$Nutrients_Lakes

  n_row_code <- nrow(df_Nutrients_Lakes)
  n_unique_WATER_ID_code <- length(unique(df_Nutrients_Lakes$WATER_ID))
  n_unique_Category_code <- length(unique(df_Nutrients_Lakes$Category))

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 4
  n_unique_WATER_ID_dev <- 3
  n_unique_Category_dev <- 2

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(n_unique_WATER_ID_code, n_unique_WATER_ID_dev)
  testthat::expect_equal(n_unique_Category_code, n_unique_Category_dev)

})
