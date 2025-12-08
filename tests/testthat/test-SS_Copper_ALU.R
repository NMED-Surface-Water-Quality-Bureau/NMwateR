test_that("SS Copper ALU calc works", {
  example_LANL_DU_table <- NMwateR::example_LANL_DU_table
  example_LANL_WQ_table <- NMwateR::example_LANL_WQ_table

  ## Site-specific copper ####
  #Only used for LANL data
  SS_Copper_ALU_list <- SS_Copper_ALU(DU_LANL_Stations_table = example_LANL_DU_table
                                      , LANL_WQ_data = example_LANL_WQ_table)

  df_SS_Copper_ALU <- SS_Copper_ALU_list$df_SS_Copper_ALU

  rm(SS_Copper_ALU_list, example_LANL_DU_table, example_LANL_WQ_table)

  n_row_code <- nrow(df_SS_Copper_ALU)
  sum_n_Samps_assessable_code <- sum(df_SS_Copper_ALU$n_Samples_assessable, na.rm = TRUE)
  sum_n_Exceed_acute_code <- sum(df_SS_Copper_ALU$n_Exceed_Acute, na.rm = TRUE)
  sum_n_Exceed_chron_code <- sum(df_SS_Copper_ALU$n_Exceed_Chronic, na.rm = TRUE)

  # dev values taken from run of R code that was developed by Tetra Tech
  # dev code ran on 12/05/2025 using Chama project
  n_row_dev <- 9
  sum_n_Samps_assessable_dev <- 44
  sum_n_Exceed_acute_dev <- 1
  sum_n_Exceed_chron_code <- 0

  testthat::expect_equal(n_row_code, n_row_dev)
  testthat::expect_equal(sum_n_Samps_assessable_code, sum_n_Samps_assessable_dev)
  testthat::expect_equal(sum_n_Exceed_acute_code, sum_n_Exceed_acute_dev)
  testthat::expect_equal(sum_n_Exceed_chron_code, sum_n_Exceed_chron_code)

})
