#' Analysis of toxics data against HH standards for ALU
#'
#' This function compares toxics data against water quality standards for
#' human health (HH) under the aquatic life use (ALU). For more information,
#' see the NMED Consolidated Assessment and Listing Methodology (CALM)
#' guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param Criteria_table Quality controlled criteria table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed toxics data compared to HH
#' water quality criteria.
#'
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_Criteria <- NMwateR::example_criteria_processed
#'
#' df_Toxics_HH <- Toxics_HH(Chem_table = df_Chem_combined
#' , Criteria_table = df_Criteria)
#' @export
Toxics_HH <- function(Chem_table
                       , Criteria_table){

  # QC ####
  # QC messages for required files
  if (missing(Chem_table)) {
    stop(paste0("Error: 'Chem_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  if (missing(Criteria_table)) {
    stop(paste0("Error: 'Criteria_table' is required but was not provided. ",
                "This is an output from the Data_Prep function."
    ))}

  # Toxics HH ####
  RFunctionName <- "Toxics_HH"
  DU_Name <- "HH"

  # Filter only relevant criteria
  df_Crit_v2 <- Criteria_table %>%
    dplyr::filter(Criteria_Type == DU_Name
                  & TOXIC == "Y")

  CHR_UID_ToxicHH <- unique(df_Crit_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v2 <- Chem_table %>%
    dplyr::filter(CHR_UID %in% CHR_UID_ToxicHH)

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

      n_Samples <- nrow(df_subset_v2)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # subset criteria table by WATER_ID
      df_Crit_v3 <- df_Crit_v2 %>%
        dplyr::filter(CHR_UID %in% filter_by) %>%
        dplyr::select(-c(DU)) %>%
        dplyr::distinct() %>%
        dplyr::select(CHR_UID, Criteria_Type, Magnitude_Numeric)

      # create results table
      df_results <- df_subset_v2 %>%
        dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, SAMPLING_EVENT_TYPE
                      , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME
                      , UNITS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(R_Script_Name = RFunctionName
                      , DU = DU_Name
                      , n_Samples = n_Samples) %>%
        dplyr::left_join(., df_Crit_v3, by = "CHR_UID") %>%
        dplyr::rename(Criteria_value = Magnitude_Numeric)

      # Apply method based on Human Health criteria
      Crit_HH <- df_Crit_v3 %>%
        dplyr::pull(Magnitude_Numeric)

      if(n_Samples == 1){
        results <- df_subset_v2 %>%
          dplyr::mutate(bad_samp = dplyr::case_when((LESS_THAN_YN == "Y"
                                                     & (Crit_HH <= MEASUREMENT_num))  ~ 0
                                                    , MEASUREMENT_num > Crit_HH ~ 1
                                                    , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_HH <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                           == "Nonassessable"
                                                           , na.rm = TRUE)

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"
        df_results$Rationale <- "Data Insufficient"
        df_results$Exceed <- ifelse(bad_tot ==1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else if (n_Samples == 2 | n_Samples == 3){
        results <- df_subset_v2 %>%
          dplyr::mutate(bad_samp = dplyr::case_when((LESS_THAN_YN == "Y"
                                                     & (Crit_HH <= MEASUREMENT_num))  ~ 0
                                                    , MEASUREMENT_num > Crit_HH ~ 1
                                                    , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_HH <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        df_results$n_Samples_assessable <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                           == "Nonassessable"
                                                           , na.rm = TRUE)

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
          dplyr::mutate(bad_samp = dplyr::case_when((LESS_THAN_YN == "Y"
                                                     & (Crit_HH <= MEASUREMENT_num))  ~ 0
                                                    , MEASUREMENT_num > Crit_HH ~ 1
                                                    , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_HH <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        n_Samples_assessable_value <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                      == "Nonassessable"
                                                      , na.rm = TRUE)

        df_results$n_Samples_assessable <- n_Samples_assessable_value

        bad_tot <- sum(results$bad_samp)

        df_results$Method <- "No more than one exceedance"

        df_results$Rationale <- ifelse(n_Samples_assessable_value >= 4
                                       , "Data Sufficient"
                                       , ifelse(bad_tot >1
                                                , "Data Sufficient; Low N Samples"
                                                , "Data Insufficient"))

        df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- NA

      } else {
        # not to exceed 10% (<10%) of measurements
        results <- df_subset_v2 %>%
          dplyr::mutate(bad_samp = dplyr::case_when((LESS_THAN_YN == "Y"
                                                     & (Crit_HH <= MEASUREMENT_num))  ~ 0
                                                    , MEASUREMENT_num > Crit_HH ~ 1
                                                    , TRUE ~ 0),
                        FLAG_MRLgrtrCrit = dplyr::case_when((LESS_THAN_YN == "Y"
                                                             & (Crit_HH <= MEASUREMENT_num))
                                                            ~ "Nonassessable"
                                                            , TRUE ~ NA))

        n_Samples_assessable_value <- n_Samples - sum(results$FLAG_MRLgrtrCrit
                                                      == "Nonassessable"
                                                      , na.rm = TRUE)

        df_results$n_Samples_assessable <- n_Samples_assessable_value

        bad_tot <- sum(results$bad_samp)
        bad_pct <- 100*(bad_tot/n_Samples_assessable_value) #make into percent

        if(n_Samples_assessable_value >= 10) {
          df_results$Method <- "Not to Exceed 10% of Measurements"
          df_results$Rationale <- "Data Sufficient"
          df_results$Exceed <- ifelse(bad_pct >= 10, 'Yes', 'No')
        } else {
          df_results$Method <- "No more than one exceedance"
          df_results$Rationale <- ifelse(n_Samples_assessable_value >= 4
                                         , "Data Sufficient"
                                         , ifelse(bad_tot >1
                                                  , "Data Sufficient; Low N Samples"
                                                  , "Data Insufficient"))

          df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
        } # END ~ if/else

        df_results$n_Exceed <- bad_tot
        df_results$pct_Exceed <- round(bad_pct,2)

      } # END ~ method if/else

      result_list[[counter]] <- df_results
    } # END ~ Parameter for loop
  } # END ~ AU for loop

  # combine results from for loop
  df_Toxics_HH <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_Toxics_HH)

} # END ~ Function
