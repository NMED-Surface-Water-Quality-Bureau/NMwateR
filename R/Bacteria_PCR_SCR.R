#' Analysis of bacteria data against PCR and SCR standards
#'
#' This function compares bacteria data against water quality
#' standards for primary contact recreation (PCR) and secondary contact recreation
#' (SCR) uses. For more information, see the NMED Consolidated Assessment and
#' Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#'
#' @returns A dataframe containing analyzed bacteria data compared to PCR and SCR
#' water quality criteria.
#'
#' @examples
#' \dontrun{
#' df_Bacteria_PCR_SCR <- Bacteria_PCR_SCR(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed)}
#'
Bacteria_PCR_SCR <- function(Chem_table
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

  # Bacteria PCR/SCR ####
  RFunctionName <- "Bacteria_PCR_SCR"

  # Only relevant criteria in DU table
  df_DU_v2 <- DU_table %>%
    dplyr::filter(DU == "PC" | DU == "SC") %>%
    dplyr::filter(Criteria_Name == "ECOLI_GEOMEAN"
                  | Criteria_Name == "ECOLI_SINGLE") %>%
    dplyr::select(-c(TN_SITE_CLASS, TP_SITE_CLASS)) %>%
    dplyr::distinct()

  (CHR_UID_BactPCRSCR <- unique(df_DU_v2$CHR_UID))

  ## Trim chem data ####
  df_Chem_v2 <- Chem_table %>%
    dplyr::filter(CHR_UID %in% CHR_UID_BactPCRSCR)

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
      df_DU_v4 <- df_DU_v3 %>%
        dplyr::filter(CHR_UID_Unique %in% filter_by)

      #If samples exist, but the AU doesn't have criteria for it - skip
      if(nrow(df_DU_v4) == 0) {
        next
      }

      # create results table
      df_results <- df_subset_v3 %>%
        dplyr::select(WATER_ID, WATER_NAME, PROJECT_NAME, SAMPLING_EVENT_TYPE
                      , SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHR_UID_Unique
                      , CHARACTERISTIC_NAME, UNITS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(R_Script_Name = RFunctionName
                      , DU = df_DU_v4$DU
                      , n_Samples = n_Samples
                      , Criteria_Value = df_DU_v4$Criteria_Value)

      # Apply method based on CHR_UID and n_Samples

      Crit_Ecoli <- df_DU_v4 %>%
        dplyr::pull(Criteria_Value)

      ## ECOLI_SINGLE ####
      if(filter_by == "2287b"){

        if(n_Samples == 1){
          results <- df_subset_v3 %>%
            dplyr::mutate(bad_samp = dplyr::case_when(MEASUREMENT_num > Crit_Ecoli ~ 1
                                                      , TRUE ~ 0))

          bad_tot <- sum(results$bad_samp)

          df_results$Method <- "No more than one exceedance"
          df_results$Rationale <- "Data Insufficient"
          df_results$Exceed <- ifelse(bad_tot == 1, 'Yes', 'No')
          df_results$n_Exceed <- bad_tot
          df_results$pct_Exceed <- NA

        } else if (n_Samples == 2 | n_Samples == 3){
          results <- df_subset_v3 %>%
            dplyr::mutate(bad_samp = dplyr::case_when(MEASUREMENT_num > Crit_Ecoli ~ 1
                                                      , TRUE ~ 0))

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
            dplyr::mutate(bad_samp = dplyr::case_when(MEASUREMENT_num > Crit_Ecoli ~ 1
                                                      , TRUE ~ 0))

          bad_tot <- sum(results$bad_samp)

          df_results$Method <- "No more than one exceedance"
          df_results$Rationale <- "Data Sufficient"
          df_results$Exceed <- ifelse(bad_tot >1, 'Yes', 'No')
          df_results$n_Exceed <- bad_tot
          df_results$pct_Exceed <- NA

        } else {
          # not to exceed 10% (<10%) of measurements
          results <- df_subset_v3 %>%
            dplyr::mutate(bad_samp = dplyr::case_when(MEASUREMENT_num > Crit_Ecoli ~ 1
                                                      , TRUE ~ 0))

          bad_tot <- sum(results$bad_samp)
          bad_pct <- 100*(bad_tot/n_Samples) #make into percent

          df_results$Method <- "Not to Exceed 10% of Measurements"
          df_results$Rationale <- "Data Sufficient"
          df_results$Exceed <- ifelse(bad_pct >= 10, 'Yes', 'No')
          df_results$n_Exceed <- NA
          df_results$pct_Exceed <- round(bad_pct,2)

        } # END ~ method if/else

        ## ECOLI_GEOMEAN ####
      } else if (filter_by == "2287a") {

        if(n_Samples <= 10){
          df_results$Method <- "Geometric Mean"
          df_results$Rationale <- "Data Insufficient"
          df_results$Exceed <- "No"
          df_results$n_Exceed <- 0
          df_results$pct_Exceed <- NA

        } else {
          # Calculate dynamic window sizes
          df_subset_v3_sort <- df_subset_v3 %>%
            dplyr::filter(!is.na(DATE)) %>%
            dplyr::arrange(dplyr::desc(DATE))

          window_sizes <- seq_along(df_subset_v3_sort$DATE) -
            findInterval(df_subset_v3_sort$DATE - 30, df_subset_v3_sort$DATE)

          # Calculate results
          results <- df_subset_v3 %>%
            dplyr::arrange(DATE, TIME) %>%
            dplyr::mutate(geo_mean_30d = zoo::rollapplyr(MEASUREMENT_num
                                                         , width = window_sizes
                                                         , FUN = psych::geometric.mean
                                                         , partial = TRUE)
                          , sample_count_30d = window_sizes
                          , Exceed = ifelse(geo_mean_30d >= Crit_Ecoli
                                            & sample_count_30d >= 5, 'Yes', 'No'))


          bad_tot <- nrow(dplyr::filter(results, Exceed == 'Yes'))

          df_results$Method <- "Geometric Mean"
          df_results$Rationale <- "Data Sufficient"
          df_results$Exceed <- ifelse(bad_tot >0 , 'Yes', 'No')
          df_results$n_Exceed <- bad_tot
          df_results$pct_Exceed <- NA

        } # END ~ sample size if/else
      } # END ~ method based if/else

      result_list[[counter]] <- df_results
    } # END ~ constituents for loop
  } # END ~ WATER_ID for loop

  # combine results from for loop
  df_Bacteria_PCR_SCR <- as.data.frame(do.call("rbind", result_list))

  # Export data ####
  return(df_Bacteria_PCR_SCR)

} # END ~ Function
