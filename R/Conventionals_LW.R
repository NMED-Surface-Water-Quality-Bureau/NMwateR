#' Analysis of conventionals data against LW standards
#'
#' This function compares conventional water chemistry data against water quality
#' standards for livestock watering (LW) use. For more information, see the NMED
#' Consolidated Assessment and Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#' @param Criteria_table Quality controlled criteria table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed conventionals data compared to LW
#' water quality criteria.
#'
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_DU_processed <- NMwateR::example_DU_processed
#' df_Criteria <- NMwateR::example_criteria_processed
#'
#' df_Conventionals_LW <- Conventionals_LW(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed
#' , Criteria_table = df_Criteria)
#' @export
#'
Conventionals_LW <- function(Chem_table
                       , DU_table
                       , Criteria_table){

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

  if (missing(Criteria_table)) {
    stop(paste0("Error: 'Criteria_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  # Conventionals LW ####
  RFunctionName <- "Conventionals_LW"

  # Only relevant criteria in DU table
  df_DU_v2 <- DU_table %>%
    dplyr::filter(DU == "LW") %>%
    dplyr::select(-c(TN_SITE_CLASS, TP_SITE_CLASS, ECOREGION, ELEVATION)) %>%
    dplyr::distinct()

  df_DU_v2$Criteria_Value <- as.numeric(df_DU_v2$Criteria_Value)

  my_DU_CHR_UID <- unique(df_DU_v2$CHR_UID)

  # Only relevant criteria in criteria table
  df_Crit_v2 <- Criteria_table %>%
    dplyr::filter(DU == "LW" & TOXIC == "N")

  my_Crit_CHR_UID <- unique(df_Crit_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v2 <- Chem_table %>%
    dplyr::filter(CHR_UID %in% my_DU_CHR_UID
                  | CHR_UID %in% my_Crit_CHR_UID)

  ## DU plus criteria ####
  # WATER_IDs
  df_WATER_ID <- tibble::tibble(WATER_ID = unique(df_Chem_v2$WATER_ID))

  # dummy table for appending criteria to DU table for simplicity
  df_Crit_dummy <- df_Crit_v2 %>%
    dplyr::select(DU, Waterbody, CHR_NAME, Magnitude_Numeric, CHR_UID) %>%
    dplyr::mutate(CHR_UID_Unique = CHR_UID) %>%
    dplyr::rename(Criteria_Name = CHR_NAME
                  , Criteria_Value = Magnitude_Numeric)

  df_Crit_dummy_v2 <- df_WATER_ID %>%
    tidyr::crossing(df_Crit_dummy)

  df_DU_v3 <- rbind(df_DU_v2, df_Crit_dummy_v2)

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
    df_DU_v4 <- df_DU_v3 %>%
      dplyr::filter(WATER_ID == i)

    # join CHR_UID_Unique to data samples
    df_subset_v2 <- df_subset %>%
      dplyr::left_join(unique(dplyr::select(df_DU_v3, WATER_ID, CHR_UID
                                            , CHR_UID_Unique))
                       , by = c("CHR_UID", "WATER_ID")
                       , relationship = 'many-to-many') %>%
      dplyr::mutate(CHR_UID_Unique = ifelse(is.na(CHR_UID_Unique)
                                            , CHR_UID
                                            , CHR_UID_Unique))

    # obtain unique constituents from WQ dataset for the WATER_ID
    my_constituents <- unique(df_subset_v2$CHR_UID_Unique)

    #Cycle through each parameter to apply logic
    for(j in my_constituents) {
      print(j)
      counter <- counter + 1

      # Specify CHR_UID
      filter_by <- j

      # subset chem data by CHR_UID
      df_subset_v3 <- df_subset_v2 %>%
        dplyr::filter(CHR_UID_Unique %in% filter_by)

      n_Samples <- nrow(df_subset_v3)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # subset DU table by WATER_ID
      df_DU_v5 <- df_DU_v4 %>%
        dplyr::filter(CHR_UID_Unique %in% filter_by)

      #If samples exist, but the AU doesn't have criteria for it - skip
      if(nrow(df_DU_v5) == 0) {
        next
      }

      # create results table
      df_results <- df_subset_v3 %>%
        dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, SAMPLING_EVENT_TYPE
                      , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHR_UID_Unique
                      , CHARACTERISTIC_NAME, UNITS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(R_Script_Name = RFunctionName
                      , DU = df_DU_v5$DU
                      , n_Samples = n_Samples
                      , Criteria_Value = df_DU_v5$Criteria_Value)

      # Apply method based on n_Samples

      if(n_Samples == 1){
        results <- df_subset_v3 %>%
          dplyr::left_join(., df_DU_v5) %>%
          #If DO or pH low, analyze as minimum, if not analyze as maximum
          dplyr::mutate(bad_samp = ifelse(CHR_UID_Unique %in% c('1648b', '985')
                                          , ifelse(MEASUREMENT_num
                                                   < df_DU_v5$Criteria_Value, 1, 0)
                                          , ifelse(MEASUREMENT_num
                                                   >= df_DU_v5$Criteria_Value, 1, 0)))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- "Data Insufficient"
        df_results$Exceed <- ifelse(bad_tot ==1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else if (n_Samples == 2 | n_Samples == 3){
        results <- df_subset_v3 %>%
          dplyr::left_join(., df_DU_v5) %>%
          #If DO or pH low, analyze as minimum, if not analyze as maximum
          dplyr::mutate(bad_samp = ifelse(CHR_UID_Unique %in% c('1648b', '985')
                                          , ifelse(MEASUREMENT_num
                                                   < df_DU_v5$Criteria_Value, 1, 0)
                                          , ifelse(MEASUREMENT_num
                                                   >= df_DU_v5$Criteria_Value, 1, 0)))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- ifelse(bad_tot >1, "Data Sufficient; Low N Samples"
                                       , "Data Insufficient")
        df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else if (n_Samples >= 4 & n_Samples < 10) {
        # no more than one exceedance (≤1) of the criterion
        results <- df_subset_v3 %>%
          dplyr::left_join(., df_DU_v5) %>%
          #If DO or pH low, analyze as minimum, if not analyze as maximum
          dplyr::mutate(bad_samp = ifelse(CHR_UID_Unique %in% c('1648b', '985')
                                          , ifelse(MEASUREMENT_num
                                                   < df_DU_v5$Criteria_Value, 1, 0)
                                          , ifelse(MEASUREMENT_num
                                                   >= df_DU_v5$Criteria_Value, 1, 0)))

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- "Data Sufficient"
        df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else {
        # not to exceed 10% (<10%) of measurements
        results <- df_subset_v3 %>%
          #If DO or pH low, analyze as minimum, if not analyze as maximum
          dplyr::mutate(bad_samp = ifelse(CHR_UID_Unique %in% c('1648b', '985')
                                          , ifelse(MEASUREMENT_num
                                                   < df_DU_v5$Criteria_Value, 1, 0)
                                          , ifelse(MEASUREMENT_num
                                                   >= df_DU_v5$Criteria_Value, 1, 0)))

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
  df_Conventionals_LW <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_Conventionals_LW)

} # END ~ Function
