#' Analysis of turbidity data against ALU standards
#'
#' This function compares turbidity data against water quality
#' standards for aquatic life use (ALU). For more information, see the NMED
#' Consolidated Assessment and Listing Methodology (CALM) guidance manual.
#'
#' @param Chem_table Compiled water chemistry data from Data_Prep function. Usually
#' contains a combination of grab, LTD, and lake profile data.
#' @param DU_table Quality controlled Stations DU table from Data_Prep function.
#' @param Criteria_table Quality controlled criteria table from Data_Prep function.
#'
#' @returns A list of two dataframes. The first contains analyzed turbidity data
#' compared to ALU water quality criteria. The second, labeled "Indiv_Res"
#' is an intermediate file used for QA/QC purposes.
#'
#' @examples
#' df_Chem_combined <- NMwateR::example_chemistry_processed
#' df_Criteria <- NMwateR::example_criteria_processed
#' df_DU_processed <- NMwateR::example_DU_processed
#'
#' Turbidity_ALU_list <- Turbidity_ALU(Chem_table = df_Chem_combined
#' , DU_table = df_DU_processed
#' , Criteria_table = df_Criteria)
#'
#' df_Turbidity_ALU <- Turbidity_ALU_list$Turbidity_ALU
#' df_Turbidity_ALU_Indiv_Res <- Turbidity_ALU_list$Turbidity_ALU_Indiv_Res
#' @export
#'
Turbidity_ALU <- function(Chem_table
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
  RFunctionName <- "Turbidity_ALU"

  # Turbidity applies to only three ALUs rather than all seven
  ALUs <- c("ColdWAL", "HQColdWAL", "MCWAL")

  df_DU_v2 <- DU_table %>%
    dplyr::select(WATER_ID, DU) %>%
    dplyr::distinct() %>%
    dplyr::filter(DU %in% ALUs)

  df_Chem_v2 <- dplyr::left_join(Chem_table, df_DU_v2, by = "WATER_ID") %>%
    dplyr::filter(!is.na(DU)) # removes AUs that don't have correct ALUs

  # Turbidity ALU ####
  # Filter only relevant criteria
  df_Crit_v2 <- Criteria_table %>%
    dplyr::filter(DU %in% ALUs
                  & CHR_UID == 1977)

  CHR_UID_Turb <- unique(df_Crit_v2$CHR_UID)

  ## Trim chem data ####
  df_Chem_v3 <- df_Chem_v2 %>%
    dplyr::filter(CHR_UID %in% CHR_UID_Turb)

  ## AU Loop ####
  Unique_AUIDs <- unique(df_Chem_v3$WATER_ID) %>% stats::na.omit()
  result_list <- list()
  result_indiv_list <- list()
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

    counter <- counter + 1

    # subset chem data as noted in CALM
    # Must be from lowflow or baseflow conditions (added to SAMPLE_TYPE in data prep)
    df_subset_v2 <- df_subset %>%
      dplyr::filter(SAMPLE_TYPE == "Baseflow")

    # must have >= 4 samples, 21 days apart, within a calendar year
    df_subset_v3 <- df_subset_v2 %>%
      dplyr::mutate(DATE = as.Date(DATE),
                    YEAR = year(DATE)) %>%
      dplyr::arrange(YEAR, dplyr::desc(MEASUREMENT_num))  # Prioritize high turbidity

    filter_21_day_apart <- function(df_year) {
      selected <- tibble::tibble()
      excluded <- tibble::tibble()

      for (i in seq_len(nrow(df_year))) {
        current_row <- df_year[i, ]

        #If nothing has been selected yet OR
        #Compare current_row$DATE against all dates already in selected$DATE
        #difftime() computes difference in days
        #abs >= ensures at least 21 days in each direction
        #all() means the row is only added if the condition is true for all selected rows
        if (nrow(selected) == 0 ||
            all(abs(difftime(current_row$DATE, selected$DATE, units = "days")) >= 21)) {
          selected <- dplyr::bind_rows(selected, current_row)
        } else {
          excluded <- dplyr::bind_rows(excluded, current_row)
          cat(paste0("Excluded: ", current_row$STATION, "_", current_row$DATE, "\n"))
        }
      }
      return(selected)
    } # END ~ filter_21_day_apart function

    # Apply per year
    df_subset_v4 <- df_subset_v3 %>%
      dplyr::group_by(YEAR) %>%
      dplyr::group_split() %>%
      purrr::map(filter_21_day_apart) %>%
      purrr::list_rbind()

    #If no relevant samples, skip WATER_ID
    if(nrow(df_subset_v4)==0){
      next
    }

    # Group by AU and YEAR to assess each combination
    df_results_indiv <- df_subset_v4 %>%
      dplyr::mutate(bad_samp = MEASUREMENT_num > 23) %>%
      dplyr::group_by(WATER_ID, YEAR, WATER_NAME, PROJECT_NAME, DU, SAMPLING_EVENT_TYPE,
                      SAMPLE_TYPE, SAMPLE_FRACTION, CHR_UID, CHARACTERISTIC_NAME, UNITS) %>%
      dplyr::group_modify(~ {#can apply a function to groups; everything in {} is considered a function
        data <- .x
        n_Samples <- nrow(data)
        n_Exceed <- sum(data$bad_samp, na.rm = TRUE)

        if (n_Samples < 4) {
          tibble::tibble(Exceed = "No",
                         Rationale = "Data Insufficient",
                         n_Samples = n_Samples,
                         n_Exceed = n_Exceed)
        } else {
          exceed_seq <- rle(data$bad_samp)
          #looking for 4 consecutive exceedance samples
          if (any(exceed_seq$values & exceed_seq$lengths >= 4)) {
            tibble::tibble(Exceed = "Yes",
                           Rationale = "Data Sufficient",
                           n_Samples = n_Samples,
                           n_Exceed = n_Exceed)
          } else {
            tibble::tibble(Exceed = "No",
                           Rationale = "Data Sufficient",
                           n_Samples = n_Samples,
                           n_Exceed = n_Exceed)
          }
        }
      }) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Criteria_value = 23,
                    R_Script_Name = RFunctionName)

    df_results <- df_results_indiv %>%
      dplyr::arrange(WATER_ID,
                     dplyr::desc(Rationale == "Data Sufficient"), # Prioritize data sufficient years
                     dplyr::desc(Exceed == "Yes"), # Prioritize exceed over non-exceed
                     dplyr::desc(n_Exceed)) %>%  # Prioritize more exceedance samples
      dplyr::group_by(WATER_ID) %>%
      dplyr::slice(1) %>%  # Keep only the worst year per WATER_ID
      dplyr::ungroup() %>%
      dplyr::select(-YEAR) %>%
      dplyr::distinct() %>%
      dplyr::mutate(Method = "Turbidity grab sample methodology")

    result_list[[counter]] <- df_results
    result_indiv_list[[counter]] <- df_results_indiv

  } # END ~ AU for loop

  # combine results from for loop
  df_loop_results <- as.data.frame(do.call("rbind", result_list))
  df_loop_results_indiv <- as.data.frame(do.call("rbind", result_indiv_list))

  # Export data ####
  return(list(
    Turbidity_ALU = df_loop_results,
    Turbidity_ALU_Indiv_Res = df_loop_results_indiv))

} # END ~ Function
