#' Analysis of non hardness dependent toxics data against ALU standards
#'
#' This function compares non hardness dependent toxics data against water quality
#' standards for aquatic life use (ALU). For more information, see the NMED
#' Consolidated Assessment and Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#' @param Criteria_table Quality controlled criteria table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed non hardness dependent toxics data
#' compared to ALU water quality criteria.
#'
#' @examples
#' \dontrun{
#' df_Tox_ALU_nHDM <- Toxics_ALU_nonHDM(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed
#' , Criteria_table = df_Criteria)}
#'
Toxics_ALU_nonHDM <- function(Chem_table
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

  # Format data ####
  ALUs <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")

  df_DU_v2 <- DU_table %>%
    dplyr::select(WATER_ID, DU) %>%
    dplyr::distinct() %>%
    dplyr::filter(DU %in% ALUs)

  df_Chem_v2 <- dplyr::left_join(Chem_table, df_DU_v2, by = "WATER_ID")

  # Toxics ALU ####
  RFunctionName <- "Toxics_ALU_nonHDM"

  # Filter only relevant criteria
  df_Crit_v2 <- Criteria_table %>%
    dplyr::filter(DU %in% ALUs
                  & TOXIC == "Y"
                  & HD_METAL == "N"
                  & (Criteria_Type == "ACUTE" | Criteria_Type == "CHRONIC"))

  CHR_UID_ToxicALU <- unique(df_Crit_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v3 <- df_Chem_v2 %>%
    dplyr::filter(CHR_UID %in% CHR_UID_ToxicALU)

  ## AU Loop ####
  Unique_AUIDs <- unique(df_Chem_v3$WATER_ID) %>% stats::na.omit()
  result_list <- list()
  counter <- 0

  for(i in Unique_AUIDs){
    print(i) # print name of current WATER_ID

    # subset chem data by WATER_ID
    df_subset <- df_Chem_v3 %>%
      dplyr::filter(WATER_ID == i)

    #If no relevant samples, skip WATER_ID
    if(nrow(df_subset)==0){
      next
    }

    # obtain unique constituents from WQ dataset for the WATER_ID
    my_constituents <- unique(df_subset$CHR_UID)

    #Cycle through each parameter to apply logic
    for(j in my_constituents){
      print(j)
      counter <- counter + 1

      # Specify CHR_UID
      filter_by <- j

      # subset chem data by CHR_UID
      df_subset_v2 <- df_subset %>%
        dplyr::filter(CHR_UID %in% filter_by)

      # subset chem data by last three years
      maxYear <- max(lubridate::year(df_subset_v2$DATE))
      YearMinus2 <- maxYear-2

      df_subset_v3 <- df_subset_v2 %>%
        dplyr::mutate(Year = lubridate::year(DATE)) %>%
        dplyr::filter(Year >= YearMinus2)

      n_Samples <- nrow(df_subset_v3)
      Use <- unique(df_subset_v3$DU)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # subset criteria table by WATER_ID
      df_Crit_v3 <- df_Crit_v2 %>%
        dplyr::filter(CHR_UID %in% filter_by) %>%
        dplyr::filter(DU == Use) %>%
        dplyr::select(CHR_UID, DU, Criteria_Type, Magnitude_Numeric)

      # create results table
      df_results <- df_subset_v3 %>%
        dplyr::select(WATER_ID, WATER_NAME, DU, PROJECT_NAME, SAMPLING_EVENT_TYPE
                      , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME
                      , UNITS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(R_Script_Name = RFunctionName
                      , n_Samples = n_Samples)

      # Apply method based on Acute and Chronic criteria
      Crit_Acute <- df_Crit_v3 %>%
        dplyr::filter(Criteria_Type == "ACUTE") %>%
        dplyr::pull(Magnitude_Numeric) %>%
        dplyr::first(default = NA_real_)

      Crit_Chronic <- df_Crit_v3 %>%
        dplyr::filter(Criteria_Type == "CHRONIC") %>%
        dplyr::pull(Magnitude_Numeric) %>%
        dplyr::first(default = NA_real_)

      df_results$Criteria_Value_Acute <- Crit_Acute
      df_results$Criteria_Value_Chronic <- Crit_Chronic

      if(n_Samples == 1){
        results <- df_subset_v3 %>%
          dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                          , MEASUREMENT_num > Crit_Acute ~ 1
                                                          , TRUE ~ 0),
                        bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                            , is.na(Crit_Chronic) ~ 0
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                            , MEASUREMENT_num > Crit_Chronic ~ 1
                                                            , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                           == "Nonassessable"
                                                           , na.rm = TRUE)

        bad_tot_acute <- sum(results$bad_samp_acute)
        bad_tot_chronic <- sum(results$bad_samp_chronic)

        df_results$Method <- "No acute exceedance AND no more than one
                                  chronic exceedance"
        df_results$Rationale <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                       , "Data Sufficient"
                                       , "Data Insufficient")
        df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >=1
                                    , 'Yes', 'No')
        df_results$n_Exceed_Acute <- bad_tot_acute
        df_results$n_Exceed_Chronic <- bad_tot_chronic

      } else if (n_Samples == 2 | n_Samples == 3){
        results <- df_subset_v3 %>%
          dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                          , MEASUREMENT_num > Crit_Acute ~ 1
                                                          , TRUE ~ 0),
                        bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                            , is.na(Crit_Chronic) ~ 0
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                            , MEASUREMENT_num > Crit_Chronic ~ 1
                                                            , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                           == "Nonassessable"
                                                           , na.rm = TRUE)

        bad_tot_acute <- sum(results$bad_samp_acute)
        bad_tot_chronic <- sum(results$bad_samp_chronic)

        df_results$Method <- "No acute exceedance AND no more than one
                                  chronic exceedance"
        df_results$Rationale <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                       , "Data Sufficient"
                                       , "Data Insufficient")
        df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                    , 'Yes', 'No')
        df_results$n_Exceed_Acute <- bad_tot_acute
        df_results$n_Exceed_Chronic <- bad_tot_chronic

      } else if (n_Samples >= 4 ) {
        results <- df_subset_v3 %>%
          dplyr::mutate(bad_samp_acute = dplyr::case_when(is.na(Crit_Acute) ~ 0
                                                          , (LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))  ~ 0
                                                          , MEASUREMENT_num > Crit_Acute ~ 1
                                                          , TRUE ~ 0),
                        bad_samp_chronic = dplyr::case_when(SAMPLE_TYPE == "Storm" ~ 0
                                                            , is.na(Crit_Chronic) ~ 0
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))  ~ 0
                                                            , MEASUREMENT_num > Crit_Chronic ~ 1
                                                            , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_Acute <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , (LESS_THAN_YN == "Y"
                                                               & (Crit_Chronic <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        n_Samples_assessable_value <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                      == "Nonassessable"
                                                      , na.rm = TRUE)

        df_results$n_Samples_assessable <- n_Samples_assessable_value

        bad_tot_acute <- sum(results$bad_samp_acute)
        bad_tot_chronic <- sum(results$bad_samp_chronic)

        df_results$Rationale <- ifelse(n_Samples_assessable_value >= 4
                                       , "Data Sufficient"
                                       , ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                                , "Data Sufficient"
                                                , "Data Insufficient"))

        df_results$Method <- "No acute exceedance AND no more than one chronic exceedance"
        df_results$Exceed <- ifelse(bad_tot_acute >=1 | bad_tot_chronic >1
                                    , 'Yes', 'No')
        df_results$n_Exceed_Acute <- bad_tot_acute
        df_results$n_Exceed_Chronic <- bad_tot_chronic

      } # END ~ method if/else

      result_list[[counter]] <- df_results
    } # END ~ Parameter for loop
  } # END ~ AU for loop

  # combine results from for loop
  df_Toxics_ALU_nonHDM <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_Toxics_ALU_nonHDM)

} # END ~ Function
