# Select evidence consistently for both analytical and SQUID parameter groups.
.assessment_select_result <- function(rows, keys, category_field) {
  numeric_field <- function(field) {
    if (!field %in% names(rows)) return(rep(NA_real_, nrow(rows)))
    suppressWarnings(as.numeric(as.character(rows[[field]])))
  }
  category <- tolower(trimws(as.character(rows[[category_field]])))
  rationale <- if ("Rationale" %in% names(rows)) {
    tolower(trimws(as.character(rows$Rationale)))
  } else rep(NA_character_, nrow(rows))
  rows <- dplyr::ungroup(rows)
  rows$.category_priority <- dplyr::case_when(
    category %in% c("5", "5a", "5c", "4 or 5", "4a") ~ 1L,
    category == "3c" ~ 2L,
    category %in% c("2", "1 or 2") ~ 3L,
    category == "3b" ~ 4L,
    category == "3a" ~ 5L,
    TRUE ~ 6L)
  rows$.sufficiency_priority <- dplyr::case_when(
    grepl("low[[:space:]]+n", rationale) ~ 2L,
    rationale == "data sufficient" ~ 1L,
    rationale == "data insufficient" ~ 3L,
    TRUE ~ 4L)
  # Acute and chronic excursions can overlap. Use the larger known count
  # as a fallback, rather than adding them and double-counting samples.
  rows$.exceedance_count <- dplyr::coalesce(
    numeric_field("n_Exceed"),
    pmax(numeric_field("n_Exceed_Acute"), numeric_field("n_Exceed_Chronic"), na.rm = TRUE))
  rows$.sample_count <- numeric_field("n_Samples")
  # Preserve the former category preference only after the requested evidence
  # tie-breaks. Sorting all remaining fields resolves otherwise exact ties
  # without relying on input row order (including differing rationale text).
  rows$.category_detail <- match(category, c("5c", "5", "5a", "4 or 5", "3c",
                                             "2", "1 or 2", "3b", "3a"))
  original_fields <- setdiff(names(rows), c(".category_priority", ".sufficiency_priority",
                                           ".exceedance_count", ".sample_count", ".category_detail"))
  rows %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort(original_fields)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::arrange(.category_priority, .sufficiency_priority,
                   dplyr::desc(.exceedance_count), dplyr::desc(.sample_count),
                   .category_detail, .by_group = TRUE) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(original_fields))
}

# SQUID aliases apply only to exports; analytical characteristics stay intact
.assessment_squid_parameter <- function(name, fraction, chr_uid) {
  base <- tolower(trimws(sub("^(Maximum|Minimum) - ", "", name,
                            ignore.case = TRUE)))
  fraction <- tolower(trimws(as.character(fraction)))
  dplyr::case_when(
    base %in% c("temperature, water", "temperature", "4t3", "6t3") ~ "Temperature",
    base %in% c("dissolved oxygen (do)", "dissolved oxygen") ~ "Dissolved oxygen",
    base == "specific conductance" ~ "Specific Conductance",
    base == "ph" ~ "pH",
    base %in% c("escherichia coli", "e. coli") ~ "E. coli",
    base %in% c("plant nutrients", "nutrients") ~ "Nutrients",
    base %in% c("total phosphorus", "phosphorus, total", "phosphorus") ~
      "Phosphorus, Total",
    base == "aluminum, dissolved" |
      (base == "aluminum" & (fraction == "dissolved" | chr_uid %in% "549001")) ~
      "Aluminum, Dissolved",
    base %in% c("aluminum, total recoverable", "aluminum, total") |
      (base == "aluminum" & (fraction %in% c("total", "total recoverable") |
                              chr_uid %in% "549002")) ~ "Aluminum, Total Recoverable",
    TRUE ~ name
  )
}

.assessment_uploads <- function(rows, columns, cycle, assessment_date, tmdl_year) {
  for (field in setdiff(c("DELISTING_REASON", "PARAM_COMMENT", "ASSESSMENT_RATIONALE"),
                       names(rows))) rows[[field]] <- NA_character_
  rows$CHARACTERISTIC_NAME <- rows$PARAM_NAME
  rows$PARAM_NAME <- .assessment_squid_parameter(rows$PARAM_NAME, rows$SAMPLE_FRACTION, rows$CHR_UID)
  rod <- .assessment_rod(rows)

  # Decide use attainment across every station/parameter before splitting files
  uses <- rows %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID, USE_NAME) %>%
    dplyr::summarise(
      AU_NAME_REF_ONLY = dplyr::first(AU_NAME_REF_ONLY),
      USE_ATTAINMENT_CODE = dplyr::case_when(
        any(PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5", "4a")) ~ "N",
        any(PARAM_STATE_IR_CAT %in% c("2", "1 or 2", "3c")) ~ "F",
        TRUE ~ "X"), .groups = "drop")

  # Apply the same category/evidence selection used for DU results to aliases.
  parameters <- rows %>%
    dplyr::select(-dplyr::any_of("USE_ATTAINMENT_CODE")) %>%
    dplyr::mutate(
      PARAM_NAME = .assessment_squid_parameter(PARAM_NAME, SAMPLE_FRACTION, CHR_UID),
      has_delisting = !is.na(DELISTING_REASON) & nzchar(trimws(DELISTING_REASON))) %>%
    .assessment_select_result(c("ASSESSMENT_UNIT_ID", "USE_NAME", "PARAM_NAME"),
                               "PARAM_STATE_IR_CAT") %>%
    dplyr::left_join(uses %>% dplyr::select(-AU_NAME_REF_ONLY),
                     by = c("ASSESSMENT_UNIT_ID", "USE_NAME")) %>%
    dplyr::mutate(
      is_cause = PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5", "4a"),
      is_delisting = PARAM_STATE_IR_CAT %in% c("2", "1 or 2") & has_delisting,
      is_monitoring = PARAM_STATE_IR_CAT == "3c" |
        (PARAM_STATE_IR_CAT == "3b" & USE_ATTAINMENT_CODE == "X")) %>%
    dplyr::filter(is_cause | is_delisting | is_monitoring) %>%
    dplyr::mutate(
      PARAM_STATE_IR_CAT = dplyr::case_when(
        is_delisting ~ NA_character_,
        PARAM_STATE_IR_CAT %in% c("5", "4 or 5") ~ "5A",
        TRUE ~ toupper(PARAM_STATE_IR_CAT)),
      PARAM_STATUS_NAME = dplyr::if_else(is_monitoring, "Monitored", "Cause"),
      PARAM_ATTAINMENT_CODE = dplyr::case_when(
        is_cause ~ "not meeting criteria",
        PARAM_STATE_IR_CAT == "3B" ~ "not enough information",
        TRUE ~ "meeting criteria"),
      PARAM_POLLUTANT_INDICATOR = "Y",
      PARAM_TARGET_TMDL_DATE = dplyr::if_else(PARAM_STATE_IR_CAT %in% c("5A", "5C"), as.character(tmdl_year), NA_character_),
      PARAM_PRIORITY_RANKING = dplyr::case_when(
        PARAM_STATE_IR_CAT == "5A" ~ "High",
        PARAM_STATE_IR_CAT == "5C" ~ "Medium",
        TRUE ~ NA_character_),
      DELISTING_REASON = dplyr::if_else(is_delisting, as.character(DELISTING_REASON),
                                       NA_character_))

  parameters <- parameters %>%
    dplyr::select(-PARAM_COMMENT, -ASSESSMENT_RATIONALE) %>%
    dplyr::left_join(rod$statements %>%
                       dplyr::select(ASSESSMENT_UNIT_ID, USE_NAME, PARAM_NAME, PARAM_COMMENT),
                     by = c("ASSESSMENT_UNIT_ID", "USE_NAME", "PARAM_NAME"))

  baseline <- uses %>%
    dplyr::mutate(
      # FS is uploaded first; NS subsequently overwrites these F values with N.
      USE_ATTAINMENT_CODE = dplyr::if_else(USE_ATTAINMENT_CODE == "X", "X", "F"),
      PARAM_ATTAINMENT_CODE = dplyr::if_else(USE_ATTAINMENT_CODE == "X",
                                            "not enough information", "meeting criteria"))
  fs <- dplyr::bind_rows(
    baseline,
    parameters %>% dplyr::filter(is_delisting) %>%
      dplyr::mutate(USE_ATTAINMENT_CODE = "F"))
  ns <- parameters %>% dplyr::filter(is_cause)
  monitoring <- parameters %>% dplyr::filter(is_monitoring)

  format_upload <- function(df) {
    df <- df %>%
      dplyr::left_join(rod$rationale, by = "ASSESSMENT_UNIT_ID") %>% dplyr::mutate(
      CYCLE = as.character(cycle), CYCLE_LAST_ASSESSED = as.character(cycle),
      USE_ASMT_DATE = format(assessment_date, "%m/%d/%Y"),
      STATE_IR_CAT_CODE = NA_character_)
    for (field in setdiff(columns, names(df))) df[[field]] <- NA_character_
    df %>% dplyr::select(dplyr::all_of(columns)) %>%
      dplyr::arrange(ASSESSMENT_UNIT_ID, USE_NAME, PARAM_NAME)
  }
  reports <- lapply(list(FS = fs, NS = ns, monitoring = monitoring), format_upload)
  # Preserve upload order in the combined review report too: F/X initialization,
  # then N causes, then monitoring rows with the final use attainment
  reports$all <- dplyr::bind_rows(reports)
  reports
}

# Compact evidence shared by generated ROD statements and retained context.
.assessment_evidence <- function(row) {
  value <- function(field) {
    x <- row[[field]]
    if (!length(x) || is.na(x[1])) "" else trimws(as.character(x[1]))
  }
  number <- function(field) suppressWarnings(as.numeric(value(field)))
  yes <- function(field) value(field) %in% c("Yes", "Limited Data, Exceedance")
  name <- value("CHARACTERISTIC_NAME")
  source <- paste(value("Analysis_Source"), value("R_Script_Name"), value("SAMPLING_EVENT_TYPE"))
  nutrient <- tolower(name) %in% c("plant nutrients", "nutrients") || grepl("Nutrients_", source)
  if (nutrient) {
    indicators <- value("Char_Exceeding")
    causal <- c(if (grepl("Total Nitrogen|\\bTN\\b", indicators, ignore.case = TRUE)) "TN",
                if (grepl("Total Phosphorus|\\bTP\\b", indicators, ignore.case = TRUE)) "TP")
    response <- c(if (grepl("chlorophyll|chl-a", indicators, ignore.case = TRUE)) "chl-a",
                  if (grepl("\\bpH\\b", indicators)) "pH",
                  if (grepl("Dissolved oxygen|\\bDO\\b", indicators, ignore.case = TRUE)) "DO")
    join <- function(x) paste(x, collapse = " and ")
    if (grepl("Lake", source)) {
      clauses <- c(if (length(causal)) paste0("causal (", join(causal), ") enrichment indicator excs")
                   else if (yes("Enrichment_causal")) "causal enrichment indicator excs",
                   if (length(response)) paste0("response (", join(response), ") enrichment indicator excs")
                   else if (yes("Enrichment_response") || yes("Enrichment_chlorophyll"))
                     "response enrichment indicator excs",
                   if (value("DOsat_flag") == "Flag") "DO saturation >120% flagged")
    } else {
      clauses <- c(if (length(causal)) paste0(join(causal), " median threshold excs")
                   else if (yes("Enrichment_causal")) "causal median threshold excs",
                   if (value("Enrichment_DODELTA") == "Yes") "delta DO excs"
                   else if (value("Enrichment_DODELTA") == "No") "no delta DO exc"
                   else "delta DO data unavailable")
    }
    if (length(clauses)) return(paste(clauses, collapse = ", "))
    return("nutrient indicator details unavailable")
  }

  ltd <- grepl("LTD|LONG TERM DEPLOYMENT", source, ignore.case = TRUE) || name %in% c("4T3", "6T3")
  n <- number("n_Samples_assessable")
  if (is.na(n)) n <- number("n_Samples")
  count <- function(x) {
    if (is.na(x)) return("")
    if (is.na(n)) paste0(x, " excs (sample count unavailable)") else paste0(x, "/", n)
  }
  x <- number("n_Exceed")
  a <- number("n_Exceed_Acute")
  c <- number("n_Exceed_Chronic")
  pct <- number("pct_Exceed")
  if (ltd) {
    indicator <- if (name %in% c("4T3", "6T3")) paste(name, "exceeded")
      else if (grepl("Temperature", name, ignore.case = TRUE)) "LTD tmax WQC excs"
      else if (grepl("Dissolved oxygen", name, ignore.case = TRUE)) "LTD min DO <WQC"
      else if (grepl("Specific conductance", name, ignore.case = TRUE)) "LTD SC WQC excs"
      else paste("LTD", sub(" - ", " ", name), "WQC excs")
    years <- value("LTD_Exceedance_Years")
    if (nzchar(years)) indicator <- paste(indicator, "in", years)
    days <- number("LTD_Exceedance_Days")
    hours <- number("LTD_Exceedance_Hours")
    return(paste(c(indicator, if (!is.na(days)) paste(days, "days"),
                   if (!is.na(hours)) paste(hours, "hours")), collapse = ", "))
  }
  if (!is.na(a) || !is.na(c)) {
    clauses <- c(if (!is.na(a)) paste(count(a), "acute"),
                 if (!is.na(c)) paste(count(c), "chronic"))
    evidence <- paste0(paste(clauses, collapse = " and "), " excs")
  } else if (!is.na(x)) {
    evidence <- paste0(count(x), if (!is.na(n)) " excs" else "")
    if (!is.na(pct)) evidence <- paste0(evidence, " (", format(round(pct, 2), trim = TRUE), "%)")
  } else if (!is.na(pct)) {
    evidence <- paste0(format(round(pct, 2), trim = TRUE), "% excs")
  } else {
    evidence <- "exceedance counts unavailable"
  }
  criterion <- paste(value("Criteria_Type"), value("Criteria_Name"), value("Criterion_Type"))
  if (grepl("SSC|segment.specific", criterion, ignore.case = TRUE)) {
    evidence <- paste(evidence, "of SSC", if (grepl("Phosphorus", name, ignore.case = TRUE))
      "total phosphorus criterion" else "criterion")
  } else if (grepl("chronic", criterion, ignore.case = TRUE) && is.na(c) && is.na(a)) {
    evidence <- paste(evidence, "of chronic criterion")
  } else if (grepl("acute", criterion, ignore.case = TRUE) && is.na(c) && is.na(a)) {
    evidence <- paste(evidence, "of acute criterion")
  }
  evidence
}

# Retain other analysis sources (e.g. grab versus LTD for the same characteristic)
# as editable text, without replacing the selected row's counts and statistics.
.assessment_du_context <- function(rows) {
  if ("ROD_Context" %in% names(rows)) return(rows)
  if (!"Analysis_Source" %in% names(rows)) rows$Analysis_Source <- NA_character_
  keys <- c("WATER_ID", "DU", "CHR_UID", "CHARACTERISTIC_NAME")
  sources <- .assessment_select_result(rows, c(keys, "Analysis_Source"), "Individual_Category")
  best <- .assessment_select_result(rows, keys, "Individual_Category")
  best$ROD_Context <- vapply(seq_len(nrow(best)), function(i) {
    keep <- rep(TRUE, nrow(sources))
    for (key in keys) keep <- keep & (sources[[key]] %in% best[[key]][i])
    keep <- keep & !(sources$Analysis_Source %in% best$Analysis_Source[i]) &
      (sources$Exceed %in% "Yes" | sources$Individual_Category %in% c("5", "5c", "4 or 5", "4a", "3c"))
    statements <- unique(vapply(which(keep), function(j) .assessment_evidence(sources[j, ]), character(1)))
    if (length(statements)) paste(sort(statements), collapse = "; ") else NA_character_
  }, character(1))
  best
}

.assessment_parameter_statement <- function(group) {
  row <- if (nrow(group) == 1L) group else
    .assessment_select_result(group, character(), "PARAM_STATE_IR_CAT")
  text <- function(field) {
    x <- row[[field]]
    if (!length(x) || is.na(x[1])) "" else trimws(as.character(x[1]))
  }
  supplied <- text("PARAM_COMMENT")
  if (nzchar(supplied)) return(supplied)
  category <- toupper(text("PARAM_STATE_IR_CAT"))
  if (category %in% c("5", "4 OR 5")) category <- "5A"
  if (category %in% c("2", "1 OR 2")) category <- "2"
  parameter <- text("PARAM_NAME")
  parameter <- switch(parameter, "Temperature" = "temperature", "Dissolved oxygen" = "DO",
                      "Aluminum, Total Recoverable" = "TR Al", "Aluminum, Dissolved" = "dissolved Al",
                      "Nutrients" = "Plant Nutrients", "Phosphorus, Total" = "total phosphorus", parameter)
  aquatic <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")
  use <- if (text("DU") %in% aquatic) "" else paste0(tolower(text("USE_NAME")), " ")
  # Each characteristic/source contributes only its strongest selected evidence.
  if (category %in% c("5A", "5C", "4A", "3C")) {
    group <- group[group$PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5", "4a", "3c"), ]
  }
  if (nrow(group) > 1L) {
    source <- rep("", nrow(group))
    for (field in intersect(c("Analysis_Source", "R_Script_Name", "SAMPLING_EVENT_TYPE"), names(group))) {
      source <- paste(source, as.character(group[[field]]))
    }
    is_ltd <- grepl("LTD|LONG TERM DEPLOYMENT", source, ignore.case = TRUE) |
      group$CHARACTERISTIC_NAME %in% c("4T3", "6T3")
    group$.evidence_source <- ifelse(is_ltd, "LTD", "other")
    group$.evidence_indicator <- ifelse(is_ltd, group$CHARACTERISTIC_NAME, group$PARAM_NAME)
    group <- .assessment_select_result(group, c(".evidence_source", ".evidence_indicator"), "PARAM_STATE_IR_CAT")
  }
  evidence <- unique(unlist(lapply(seq_len(nrow(group)), function(i) {
    context <- if ("ROD_Context" %in% names(group)) as.character(group$ROD_Context[i]) else NA_character_
    c(.assessment_evidence(group[i, ]), if (!is.na(context) && nzchar(context)) strsplit(context, "; ", fixed = TRUE)[[1]])
  })))
  evidence <- paste(sort(evidence), collapse = "; ")
  qualifier <- paste(text("Assessment_Qualifier"), text("Enrichment_causal"))
  if (grepl("Waterbody Type Mismatch", qualifier, ignore.case = TRUE)) {
    return(paste0("Nutrients AP not applicable due to waterbody type mismatch, however note ", evidence, "."))
  }
  if (category == "5C") evidence <- paste0(evidence, ", further data collection needed")
  paste0(category, " for ", use, parameter, " - ", evidence, ".")
}

.assessment_rod <- function(rows) {
  nonblank <- function(x) unique(trimws(as.character(x[!is.na(x) & nzchar(trimws(x))])))
  combine <- function(x) {
    x <- nonblank(x)
    if (length(x)) paste(x, collapse = " ") else NA_character_
  }
  for (field in setdiff(c("PARAM_COMMENT", "ASSESSMENT_RATIONALE", "AV_Notes", "DELISTING_REASON",
                          "ROD_Include", "Assessment_Qualifier"), names(rows))) rows[[field]] <- NA_character_
  needed <- rows %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID, USE_NAME) %>%
    dplyr::mutate(.assessed = any(PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5", "4a", "3c", "2", "1 or 2"))) %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID, USE_NAME, PARAM_NAME) %>%
    dplyr::filter(any(PARAM_STATE_IR_CAT %in% c("5", "5c", "4 or 5", "4a", "3c") |
                       (PARAM_STATE_IR_CAT %in% "3b" & !.assessed) |
                       (!is.na(DELISTING_REASON) & nzchar(trimws(DELISTING_REASON))) |
                       tolower(as.character(ROD_Include)) %in% c("true", "yes", "1") |
                       grepl("Waterbody Type Mismatch", Assessment_Qualifier, ignore.case = TRUE))) %>%
    dplyr::ungroup()
  make_statement <- function(.x, .y) {
      if (!nrow(.x)) return(tibble::tibble(PARAM_COMMENT = character(), ROD_Include = logical(), ROD_Order = integer()))
      .x$PARAM_NAME <- .y$PARAM_NAME
      if ("USE_NAME" %in% names(.y)) .x$USE_NAME <- .y$USE_NAME
      selected <- if (nrow(.x) == 1L) .x else
        .assessment_select_result(.x, character(), "PARAM_STATE_IR_CAT")
      category <- tolower(as.character(selected$PARAM_STATE_IR_CAT[1]))
      delisting <- category %in% c("2", "1 or 2") && length(nonblank(.x$DELISTING_REASON)) > 0L
      mismatch <- any(grepl("Waterbody Type Mismatch", .x$Assessment_Qualifier, ignore.case = TRUE))
      include <- category %in% c("5", "5c", "4 or 5", "4a", "3c") || delisting || mismatch ||
        any(tolower(as.character(.x$ROD_Include)) %in% c("true", "yes", "1"))
      tibble::tibble(PARAM_COMMENT = .assessment_parameter_statement(.x),
                     ROD_Include = include,
                     ROD_Order = if (category %in% c("5", "5c", "4 or 5", "4a")) 1L
                       else if (category == "3c" || mismatch) 2L else 3L)
  }
  statements <- needed %>% dplyr::group_by(ASSESSMENT_UNIT_ID, USE_NAME, PARAM_NAME) %>%
    dplyr::group_modify(make_statement) %>% dplyr::ungroup()
  # Narratives and explicit AU overrides may be supplied on any row, including
  # fully supporting parameters that are otherwise omitted from the rationale.
  narratives <- rows %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID) %>%
    dplyr::summarise(override = combine(sort(nonblank(ASSESSMENT_RATIONALE))),
                     notes = combine(sort(nonblank(AV_Notes))), .groups = "drop")
  # Aquatic life uses omit a use prefix and share one AU/parameter statement.
  aquatic <- c("ColdWAL", "CoolWAL", "HQColdWAL", "LAL", "MCWAL", "MWWAL", "WWAL")
  au_statements <- needed %>%
    dplyr::mutate(ROD_Use = dplyr::if_else(DU %in% aquatic, "", USE_NAME)) %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID, ROD_Use, PARAM_NAME) %>%
    dplyr::group_modify(make_statement) %>% dplyr::ungroup()
  rationale <- au_statements %>%
    dplyr::filter(ROD_Include) %>%
    dplyr::arrange(ROD_Order, PARAM_NAME, ROD_Use) %>%
    dplyr::group_by(ASSESSMENT_UNIT_ID) %>%
    dplyr::summarise(generated = combine(PARAM_COMMENT), .groups = "drop")
  rationale <- narratives %>% dplyr::left_join(rationale, by = "ASSESSMENT_UNIT_ID") %>%
    dplyr::mutate(ASSESSMENT_RATIONALE = vapply(seq_len(dplyr::n()), function(i) {
      combine(c(if (!is.na(override[i])) override[i] else generated[i], notes[i]))
    }, character(1))) %>% dplyr::select(ASSESSMENT_UNIT_ID, ASSESSMENT_RATIONALE)
  list(statements = statements, rationale = rationale)
}
