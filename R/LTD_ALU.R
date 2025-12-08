#' Analysis of long-term deployment data against ALU standards
#'
#' This function compares conventional long-term deployment water chemistry data
#' against water quality standards for aquatic life use (ALU). For more
#' information, see the NMED Consolidated Assessment and Listing Methodology
#' (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed conventional LTD data compared to ALU
#' water quality criteria.
#'
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_DU_processed <- NMwateR::example_DU_processed
#'
#' df_LTD_ALU <- LTD_ALU(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed)
#'
#' @export
LTD_ALU <- function(Chem_table
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

  # Conventionals AL ####
  RFunctionName <- "LTD_ALU"

  # Only relevant criteria in DU table
  ALUs <- c("ColdWAL", "HQColdWAL", "LW", "MCWAL", "MWWAL")

  df_DU_v2 <- DU_table %>%
    dplyr::filter(DU %in% ALUs) %>%
    dplyr::filter((CHR_UID == "999920" | CHR_UID == "999921"| CHR_UID == "1648"
                   | CHR_UID == "1815" | CHR_UID == "2849" | CHR_UID == "1977"
                   | CHR_UID == "985")) %>%
    dplyr::select(-c(TN_SITE_CLASS, TP_SITE_CLASS)) %>%
    dplyr::distinct()

  (CHR_UID_tempALU <- unique(df_DU_v2$CHR_UID))

  ## Trim chem data ####
  df_Chem_v2 <- Chem_table %>%
    dplyr::filter(SAMPLING_EVENT_TYPE == "LONG TERM DEPLOYMENT") %>%
    dplyr::mutate(CHR_UID_Unique = dplyr::case_when((CHARACTERISTIC_NAME == "Minimum - pH") ~ "1648b"
                                                    , (CHARACTERISTIC_NAME == "Maximum - pH") ~ "1648a"
                                                    , TRUE ~ as.character(CHR_UID))) %>%
    dplyr::filter(CHR_UID %in% CHR_UID_tempALU)

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

    #If no relevant DU, skip WATER_ID
    if(nrow(df_DU_v3)==0){
      next
    }

    # obtain unique constituents from DU dataset for the WATER_ID
    my_constituents <- unique(df_DU_v3$CHR_UID_Unique)

    #Cycle through each parameter to apply logic
    for(j in my_constituents) {
      print(j)

      # Specify CHR_UID
      filter_by <- j

      # subset chem data by CHR_UID
      df_subset_v2 <- df_subset %>%
        dplyr::filter(CHR_UID_Unique %in% filter_by)

      n_Samples <- nrow(df_subset_v2)

      #If no relevant samples, skip CHR_UID
      if(n_Samples == 0) {
        next
      }

      # subset DU table by WATER_ID
      df_DU_v4 <- df_DU_v3 %>%
        dplyr::filter(CHR_UID_Unique %in% filter_by)

      #If samples exist, but the AU doesn't have criteria for it - skip
      if(nrow(df_DU_v4) == 0) {
        next
      }

      #Loop through Uses
      for(k in df_DU_v4$DU) {
        counter <- counter + 1

        df_DU_v5 <- df_DU_v4 %>%
          dplyr::filter(DU == k)

        # create results table
        df_results <- df_subset_v2 %>%
          dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, SAMPLING_EVENT_TYPE
                        , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHR_UID_Unique
                        , CHARACTERISTIC_NAME, UNITS) %>%
          dplyr::distinct() %>%
          dplyr::mutate(R_Script_Name = RFunctionName
                        , DU = k
                        , n_Samples = n_Samples
                        , Criteria_Value = df_DU_v5$Criteria_Value)

        unique_AQC <- unique(df_subset_v2$ASSESSABILITY_QUALIFIER_CODE)

        results <- df_subset_v2 %>%
          dplyr::left_join(., df_DU_v5) %>%
          dplyr::mutate(
            #Does the sample exceed the criteria? Y/N
            bad_samp = ifelse(CHR_UID_Unique %in% c('1648b', '985')
                              , ifelse(MEASUREMENT_num < df_DU_v5$Criteria_Value
                                       , 1, 0)
                              , ifelse(MEASUREMENT_num >= df_DU_v5$Criteria_Value
                                       , 1, 0))

            #Does this sample have an impact on decision? Y/N
            , relevant = dplyr::case_when(ASSESSABILITY_QUALIFIER_CODE == 'NEITHER'~ 0
                                          , ASSESSABILITY_QUALIFIER_CODE == 'NON'
                                          & bad_samp == 0 ~ 0
                                          , TRUE ~ 1))

        #Total of relevant exceedances
        bad_tot <- nrow(dplyr::filter(results, results$bad_samp == 1 & results$relevant == 1))

        df_results$Method <- "No more than one exceedance"

        #If no relevance -> Data Insufficient
        df_results$Rationale <- ifelse(sum(results$relevant) == 0
                                       , "Data Insufficient"
                                       , "Data Sufficient")
        df_results$Exceed <- ifelse(bad_tot >=1, 'Yes', 'No')
        df_results$n_Exceed <- bad_tot

        result_list[[counter]] <- df_results
      }

    } # END ~ Parameter for loop
  } # END ~ AU for loop

  # combine results from for loop
  df_LTD_ALU <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_LTD_ALU)

} # END ~ Function
