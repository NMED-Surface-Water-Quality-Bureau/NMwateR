#' Analysis of salinity data against IRR standards
#'
#' This function compares salinity data against water quality
#' standards for irrigation (IRR) use. For more information, see the NMED
#' Consolidated Assessment and Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed salinity data compared to IRR
#' water quality criteria.
#'
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_DU_processed <- NMwateR::example_DU_processed
#'
#' df_Salinity_IRR <- Salinity_IRR(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed)
#'
#' @export
Salinity_IRR <- function(Chem_table
                       , DU_table){

  # QC ####
  # QC messages for required files
  if (missing(Chem_table)) {
    stop(paste0("Error: 'Chem_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  if (missing(DU_table)) {
    stop(paste0("Error: 'DU_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  # Salinity Irrigation ####
  RFunctionName <- "Salinity_IRR"

  # Only relevant criteria for TDS in DU table
  df_DU_v2 <- DU_table %>%
    dplyr::filter(DU == "IRR" & (Criteria_Name == "TDS_WQC"
                                 | Criteria_Name == "SO4_WQC"
                                 | Criteria_Name == "CHL_WQC")) %>%
    dplyr::select(-c(TP_SITE_CLASS, TN_SITE_CLASS)) %>%
    dplyr::distinct()

  CHR_UID_SalinityIRR <- unique(df_DU_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v2 <- Chem_table %>%
    dplyr::filter(CHR_UID %in% CHR_UID_SalinityIRR)

  ## AU Loop ####
  Unique_AUIDs <- unique(df_Chem_v2$WATER_ID) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  for(i in Unique_AUIDs){
    print(i) # print name of current WATER_ID

    # subset chem data by WATER_ID
    df_subset <- df_Chem_v2 %>%
      dplyr::filter(WATER_ID == i)

    #If no relevant samples, skip WATER_ID
    if(nrow(df_subset)==0){
      next
    }

    # subset DU table by WATER_ID
    df_DU_v3 <- df_DU_v2 %>%
      dplyr::filter(WATER_ID == i)

    # obtain unique constituents from WQ dataset for the WATER_ID
    my_constituents <- unique(df_subset$CHR_UID)

    #Cycle through each parameter to apply logic
    for(j in my_constituents) {
      print(j)
      counter <- counter + 1

      # Specify CHR_UID
      filter_by <- j

      # subset chem data by CHR_UID
      df_subset_v2 <- df_subset %>%
        dplyr::filter(CHR_UID %in% filter_by)

      n_Samples <- nrow(df_subset_v2)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # subset DU table by WATER_ID
      df_DU_v4 <- df_DU_v3 %>%
        dplyr::filter(CHR_UID %in% filter_by)

      #If samples exist, but the AU doesn't have criteria for it - skip
      if(nrow(df_DU_v4) == 0) {
        next
      }

      # create results table
      df_results <- df_subset_v2 %>%
        dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, SAMPLING_EVENT_TYPE
                      , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME
                      , UNITS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(R_Script_Name = RFunctionName
                      , DU = df_DU_v4$DU
                      , n_Samples = n_Samples
                      , Criteria_Value = unique(df_DU_v4$Criteria_Value)
        )

      # Apply method based on n_Samples

      if(n_Samples == 1){
        results <- df_subset_v2 %>%
          dplyr::left_join(., df_DU_v4) %>%
          dplyr::mutate(bad_samp = ifelse(MEASUREMENT_num
                                          >= df_DU_v4$Criteria_Value, 1, 0))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- "Data Insufficient"
        df_results$Exceed <- ifelse(bad_tot == 1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else if (n_Samples == 2 | n_Samples == 3){
        results <- df_subset_v2 %>%
          dplyr::left_join(., df_DU_v4) %>%
          dplyr::mutate(bad_samp = ifelse(MEASUREMENT_num
                                          >= df_DU_v4$Criteria_Value, 1, 0))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- ifelse(bad_tot >1, "Data Sufficient; Low N Samples"
                                       , "Data Insufficient")
        df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else if (n_Samples >= 4 & n_Samples < 10) {
        # no more than one exceedance (≤1) of the criterion
        results <- df_subset_v2 %>%
          dplyr::left_join(., df_DU_v4) %>%
          dplyr::mutate(bad_samp = ifelse(MEASUREMENT_num
                                          >= df_DU_v4$Criteria_Value, 1, 0))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- "Data Sufficient"
        df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else {
        # not to exceed 10% (<10%) of measurements
        results <- df_subset_v2 %>%
          dplyr::mutate(bad_samp = ifelse(MEASUREMENT_num
                                          >= df_DU_v4$Criteria_Value, 1, 0))

        bad_tot <- sum(results$bad_samp)
        bad_pct <- 100*(bad_tot/n_Samples) #make into percent

        df_results$Method <- "Not to Exceed 10% of Measurements"
        df_results$Rationale <- "Data Sufficient"
        df_results$Exceed <- ifelse(bad_pct >= 10, 'Yes', 'No')
        df_results$n_Exceed <- NA
        df_results$pct_Exceed <- round(bad_pct,2)

      } # END ~ method if/else
      result_list[[counter]] <- df_results
    } # END ~ Parameter for loop
  } # END ~ AU for loop

  # combine results from for loop
  df_Salinity_IRR <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_Salinity_IRR)

} # END ~ Function
