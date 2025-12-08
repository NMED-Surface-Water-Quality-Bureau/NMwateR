test_that("Salinity calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed

  ## Salinity IRR ####
  df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
                                  , DU_table = df_DU_processed)

  n_row_code <- nrow(df_Salinity_IRR)
  sum_n_Samps_code <- sum(df_Salinity_IRR$n_Samples, na.rm = TRUE)
  sum_n_Exceed_code <- sum(df_Salinity_IRR$n_Exceed, na.rm = TRUE)

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 0
  sum_n_Samps_dev <- 0
  sum_n_Exceed_dev <- 0

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(sum_n_Samps_code, sum_n_Samps_dev)
  testthat::expect_equal(sum_n_Exceed_code, sum_n_Exceed_dev)

})
