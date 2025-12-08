test_that("Toxics (HDM) ALU calc works", {
  df_Chem_combined <- NMwateR::example_chemistry_processed
  df_DU_processed <- NMwateR::example_DU_processed
  df_Criteria <- NMwateR::example_criteria_processed

  ## Toxics ALU (HDM) ####
  # Hardness-dependent metals only
  Toxics_ALU_HDM_list <- Toxics_ALU_HDM(Chem_table = df_Chem_combined
                                        , DU_table = df_DU_processed
                                        , Criteria_table = df_Criteria)


  df_Toxics_ALU_HDM <- Toxics_ALU_HDM_list$Toxics_ALU_HDM

  n_row_code <- nrow(df_Toxics_ALU_HDM)
  sum_n_Samps_assessable_code <- sum(df_Toxics_ALU_HDM$n_Samples_assessable, na.rm = TRUE)
  sum_n_Exceed_acute_code <- sum(df_Toxics_ALU_HDM$n_Exceed_Acute, na.rm = TRUE)
  sum_n_Exceed_chron_code <- sum(df_Toxics_ALU_HDM$n_Exceed_Chronic, na.rm = TRUE)

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 224
  sum_n_Samps_assessable_dev <- 753
  sum_n_Exceed_acute_dev <- 20
  sum_n_Exceed_chron_code <- 35

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(sum_n_Samps_assessable_code, sum_n_Samps_assessable_dev)
  testthat::expect_equal(sum_n_Exceed_acute_code, sum_n_Exceed_acute_dev)
  testthat::expect_equal(sum_n_Exceed_chron_code, sum_n_Exceed_chron_code)

})
