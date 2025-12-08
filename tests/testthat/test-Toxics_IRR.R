test_that("Toxics IRR calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_Criteria <- NMwateR::example_criteria_processed

  ## Toxics IRR ####
  df_Toxics_IRR <- Toxics_IRR(Chem_table = df_Chem_combined
                              , Criteria_table = df_Criteria)

  n_row_code <- nrow(df_Toxics_IRR)
  sum_n_Samps_assessable_code <- sum(df_Toxics_IRR$n_Samples_assessable, na.rm = TRUE)
  sum_n_Exceed_code <- sum(df_Toxics_IRR$n_Exceed, na.rm = TRUE)

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 336
  sum_n_Samps_assessable_dev <- 1488
  sum_n_Exceed_dev <- 0

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(sum_n_Samps_assessable_code, sum_n_Samps_assessable_dev)
  testthat::expect_equal(sum_n_Exceed_code, sum_n_Exceed_dev)

})
