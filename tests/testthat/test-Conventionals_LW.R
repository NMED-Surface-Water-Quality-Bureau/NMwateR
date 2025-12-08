test_that("Conv LW calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed
  df_Criteria <- NMwateR::example_criteria_processed

  ## Conventionals LW ####
  df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
                                          , DU_table = df_DU_processed
                                          , Criteria_table = df_Criteria)

  n_row_code <- nrow(df_Conventionals_LW)
  sum_n_Samps_code <- sum(df_Conventionals_LW$n_Samples, na.rm = TRUE)
  sum_n_Exceed_code <- sum(df_Conventionals_LW$n_Exceed, na.rm = TRUE)

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 29
  sum_n_Samps_dev <- 127
  sum_n_Exceed_dev <- 0

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(sum_n_Samps_code, sum_n_Samps_dev)
  testthat::expect_equal(sum_n_Exceed_code, sum_n_Exceed_dev)

})
