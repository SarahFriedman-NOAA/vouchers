# ==============================================================================
# Species and Oracle Utility Functions
# ==============================================================================

VOUCHER_TABLE <- "RACE_DATA.VOUCHERS"
VOUCHER_KEY <- c("CRUISE", "VESSEL_ID", "HAUL", "VOUCHER", "FIELD_ID", "NEW_ID")

VOUCHER_COLS <- c(
  "hauljoin", "new_id", "new_species_code", "catalog_num", "region",
  "cruise", "year", "vessel_id", "haul", "voucher", "field_id",
  "field_species_code", "n_specimens", "standard_length_mm",
  "weight_kg", "preservative", "collected_by", "id_date",
  "taxonomist", "notes"
)


#' Ensure an Active Oracle Connection Exists
#' @description Checks if an RODBC channel exists and is alive using a test query. If missing or broken, it closes any stale handle and establishes a new connection.
#' @param channel An existing RODBC channel object. If NULL, checks the calling environment.
#' @param db The database name to pass to gapindex::get_connected() (default: "AFSC").
#' @param check_access Logical, whether gapindex checks access levels (default: FALSE).
#' @return An active, verified RODBC connection handle.
oracle_connect <- function(channel = NULL, db = "AFSC", check_access = FALSE) {
  # If channel not supplied directly, check if 'channel' exists in calling scope
  if (is.null(channel)) {
    if (exists("channel", envir = parent.frame())) {
      channel <- get("channel", envir = parent.frame())
    }
  }

  need_connect <- FALSE

  if (is.null(channel)) {
    message("Oracle channel does not exist. Connecting...")
    need_connect <- TRUE
  } else {
    # 1. Test existing connection
    test_result <- try(RODBC::sqlQuery(channel, "SELECT 1 FROM DUAL"), silent = TRUE)

    # 2. Check if query threw an R error or returned an ODBC/Oracle error message
    is_broken <- inherits(test_result, "try-error") ||
      (is.character(test_result) && any(grepl("ERROR|ORA-", test_result, ignore.case = TRUE)))

    if (is_broken) {
      message("Oracle connection is broken or stale. Attempting to reconnect...")
      try(RODBC::odbcClose(channel), silent = TRUE)
      need_connect <- TRUE
    } else {
      message("Oracle connection is active and working.")
    }
  }

  # Establish new connection if needed
  if (need_connect) {
    channel <- gapindex::get_connected(db = db, check_access = check_access)
  }

  channel
}

# ------------------------------------------------------------------------------
# Standardize a voucher data frame to the Oracle table layout
# ------------------------------------------------------------------------------

format_vouchers <- function(x, key = VOUCHER_KEY) {
  x <- as.data.frame(x)
  names(x) <- tolower(names(x))

  missing <- setdiff(VOUCHER_COLS, names(x))
  if (length(missing) > 0) {
    message("Columns not in source data, set to NA: ", paste(missing, collapse = ", "))
    x[missing] <- NA
  }

  n_before <- nrow(x)

  x <- x[VOUCHER_COLS] %>%
    dplyr::mutate(
      dplyr::across(c(
        cruise, year, vessel_id, haul, voucher,
        new_species_code, field_species_code, n_specimens
      ), as.integer),
      dplyr::across(c(hauljoin, weight_kg), as.numeric),
      dplyr::across(c(
        new_id, standard_length_mm, catalog_num, region, field_id, preservative,
        collected_by, taxonomist, notes
      ), as.character),
      id_date = if (inherits(id_date, "Date")) {
        id_date
      } else {
        as.Date(id_date, format = "%m/%d/%Y")
      }
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(tolower(key))), .keep_all = TRUE)

  if (nrow(x) < n_before) {
    warning(sprintf("format_vouchers() silently dropped %d duplicate row(s) based on key.", n_before - nrow(x)))
  }

  x <- x %>%
    dplyr::arrange(year, cruise, vessel_id, haul, voucher) %>%
    dplyr::rename_with(toupper)

  dups <- x %>% dplyr::filter(duplicated(CATALOG_NUM) & !is.na(CATALOG_NUM))
  if (nrow(dups) > 0) {
    warning(
      "Duplicate CATALOG_NUM values in new data:\n",
      paste(unique(dups$CATALOG_NUM), collapse = ", ")
    )
  }

  x
}


# ------------------------------------------------------------------------------
# Normalize values so old/new compare cleanly (dates, floats, whitespace, NA)
# ------------------------------------------------------------------------------

.norm <- function(x) {
  if (inherits(x, c("Date", "POSIXct"))) {
    x <- format(as.Date(x), "%Y-%m-%d")
  } else if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA_character_,
      format(round(x, 6), trim = TRUE, scientific = FALSE)
    )
  } else {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
  }
  x
}


# ------------------------------------------------------------------------------
# Insert new rows / update changed rows in Oracle (no full re-upload)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# SQL literal formatting
# ------------------------------------------------------------------------------

.oracle_sql_literal <- function(x) {
  if (length(x) != 1) {
    stop("Expected a single value.")
  }

  if (is.na(x)) {
    return("NULL")
  }

  if (inherits(x, "Date")) {
    return(sprintf(
      "TO_DATE('%s', 'YYYY-MM-DD')",
      format(x, "%Y-%m-%d")
    ))
  }

  if (is.character(x)) {
    if (x == "") {
      return("NULL")
    }

    x <- gsub("'", "''", x, fixed = TRUE)
    return(paste0("'", x, "'"))
  }

  if (is.logical(x)) {
    return(ifelse(x, "1", "0"))
  }

  if (is.numeric(x)) {
    if (!is.finite(x)) {
      stop("Cannot write non-finite numeric value to Oracle.")
    }

    return(format(
      x,
      scientific = FALSE,
      trim = TRUE,
      digits = 15,
      decimal.mark = "."
    ))
  }

  stop("Unsupported column type: ", paste(class(x), collapse = "/"))
}


# ------------------------------------------------------------------------------
# Normalize keys for matching ONLY
# ------------------------------------------------------------------------------

.normalize_key <- function(x) {
  if (is.character(x)) {
    x <- trimws(x)

    # Treat literal "NA" as missing for key matching.
    # The original Oracle value is preserved separately.
    x[x %in% c("", "NA")] <- NA_character_
  }

  x
}


# ------------------------------------------------------------------------------
# Construct WHERE clause from ORIGINAL Oracle key values
# ------------------------------------------------------------------------------

.oracle_where_clause <- function(key_row, key) {
  paste(
    vapply(key, function(col) {
      value <- key_row[[col]]

      if (is.na(value)) {
        paste0(col, " IS NULL")
      } else {
        paste0(col, " = ", .oracle_sql_literal(value))
      }
    }, character(1)),
    collapse = " AND "
  )
}


# ------------------------------------------------------------------------------
# Update one Oracle record
# ------------------------------------------------------------------------------

.oracle_update_row <- function(
  channel,
  row,
  key_row,
  table,
  key,
  update_cols
) {
  set_clause <- paste(
    vapply(update_cols, function(col) {
      paste0(col, " = ", .oracle_sql_literal(row[[col]]))
    }, character(1)),
    collapse = ", "
  )

  where_clause <- .oracle_where_clause(key_row, key)

  sql <- paste(
    "UPDATE", table,
    "SET", set_clause,
    "WHERE", where_clause
  )

  tryCatch(
    {
      result <- RODBC::sqlQuery(
        channel,
        sql,
        errors = TRUE
      )

      if (is.character(result) && length(result) > 0) {
        stop(paste(result, collapse = "\n"))
      }
    },
    error = function(e) {
      stop(
        "Oracle update failed: ", conditionMessage(e),
        "\nSQL: ", sql
      )
    }
  )

  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# Verify one Oracle record after an update
# ------------------------------------------------------------------------------

.oracle_verify_row <- function(
  channel,
  row,
  key_row,
  table,
  key,
  verify_cols
) {
  where_clause <- .oracle_where_clause(key_row, key)

  sql <- paste(
    "SELECT",
    paste(verify_cols, collapse = ", "),
    "FROM", table,
    "WHERE", where_clause
  )

  actual <- RODBC::sqlQuery(
    channel,
    sql,
    errors = TRUE,
    stringsAsFactors = FALSE,
    na.strings = character(0)
  )

  if (is.character(actual) && length(actual) > 0) {
    stop(
      "Oracle verification query failed: ",
      paste(actual, collapse = "\n")
    )
  }

  if (nrow(actual) != 1) {
    stop(
      "Oracle verification failed: expected exactly one matching row, ",
      "but found ", nrow(actual), ".\n",
      "WHERE: ", where_clause
    )
  }

  mismatches <- verify_cols[
    !vapply(verify_cols, function(col) {
      identical(
        unname(.norm(row[[col]])),
        unname(.norm(actual[[col]]))
      )
    }, logical(1))
  ]

  if (length(mismatches) > 0) {
    stop(
      "Oracle verification failed. Values do not match for: ",
      paste(mismatches, collapse = ", "),
      "\nWHERE: ", where_clause
    )
  }

  invisible(TRUE)
}


update_oracle_vouchers <- function(
  new_data,
  channel,
  table = VOUCHER_TABLE,
  key = VOUCHER_KEY,
  dry_run = TRUE,
  max_updates = 200,
  overwrite_with_na = FALSE
) {
  names(new_data) <- toupper(names(new_data))

  # ---- Read Oracle data ------------------------------------------------------

  old_data <- RODBC::sqlQuery(
    channel,
    paste0("SELECT * FROM ", table),
    stringsAsFactors = FALSE,
    na.strings = character(0)
  )

  if (is.character(old_data) && length(old_data) > 0) {
    stop("Could not read Oracle table: ", paste(old_data, collapse = "\n"))
  }

  names(old_data) <- toupper(names(old_data))

  # ---- Sanity checks ---------------------------------------------------------

  missing_cols <- setdiff(names(new_data), names(old_data))

  if (length(missing_cols) > 0) {
    stop(
      "Columns not in Oracle table (schema change needs a full re-upload): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # ---- Clean character values without destroying literal "NA" ---------------

  # Trim incoming values and convert empty strings to missing.
  # Do NOT convert literal "NA" or "N/A" globally.

  for (col in names(new_data)) {
    if (is.character(new_data[[col]])) {
      new_data[[col]] <- trimws(new_data[[col]])
      new_data[[col]][new_data[[col]] == ""] <- NA_character_
    }
  }

  # Clean Oracle comparison columns, but preserve the ORIGINAL key values.
  compare_cols <- setdiff(names(new_data), key)

  for (col in compare_cols) {
    if (is.character(old_data[[col]])) {
      old_data[[col]] <- trimws(old_data[[col]])
      old_data[[col]][old_data[[col]] == ""] <- NA_character_
    }
  }

  # ---- Create normalized key copies -----------------------------------------

  # These copies are used only for matching.
  # old_data itself retains the original Oracle key values.

  new_key_data <- new_data
  old_key_data <- old_data

  for (k in key) {
    if (is.character(new_key_data[[k]])) {
      new_key_data[[k]] <- .normalize_key(new_key_data[[k]])
      old_key_data[[k]] <- .normalize_key(old_key_data[[k]])
    } else {
      new_key_data[[k]] <- as.integer(new_key_data[[k]])
      old_key_data[[k]] <- as.integer(old_key_data[[k]])
    }
  }

  # ---- Check normalized key uniqueness --------------------------------------

  if (anyDuplicated(new_key_data[key]) > 0) {
    stop(
      "Duplicate keys in new data after normalization: ",
      paste(key, collapse = ", ")
    )
  }

  if (anyDuplicated(old_key_data[key]) > 0) {
    stop(
      "Duplicate keys exist in Oracle after normalization. ",
      "Cannot safely determine which records should be updated."
    )
  }

  # ---- New rows --------------------------------------------------------------

  to_insert <- new_data[
    !vctrs::vec_in(
      new_key_data[key],
      old_key_data[key]
    ), ,
    drop = FALSE
  ]

  # ---- Match existing records ------------------------------------------------

  # Keep the original Oracle key values alongside the normalized keys.
  # These original values will be used in the SQL WHERE clauses.

  oracle_key_cols <- paste0(".ORACLE_KEY_", seq_along(key))

  old_match <- old_key_data[c(key, compare_cols)]

  for (i in seq_along(key)) {
    old_match[[oracle_key_cols[i]]] <- old_data[[key[i]]]
  }

  matched <- dplyr::inner_join(
    new_key_data,
    old_match,
    by = key,
    suffix = c("", ".OLD")
  )

  # ---- Identify changed values ----------------------------------------------

  diff_by_col <- lapply(compare_cols, function(col) {
    a <- .norm(matched[[col]])
    b <- .norm(matched[[paste0(col, ".OLD")]])

    changed <- dplyr::coalesce(a, "<NA>") != dplyr::coalesce(b, "<NA>")

    if (!overwrite_with_na) {
      # Cancel the change if new data is NA but Oracle currently has data
      changed[is.na(a) & !is.na(b)] <- FALSE
    }
    changed
  })

  names(diff_by_col) <- compare_cols

  changed_flag <- Reduce(`|`, diff_by_col)

  update_idx <- which(changed_flag)

  to_update <- matched[
    update_idx,
    names(new_data),
    drop = FALSE
  ]

  # Keep the actual Oracle key values separately for SQL operations.
  oracle_keys_to_update <- matched[
    update_idx,
    oracle_key_cols,
    drop = FALSE
  ]

  # Track exactly which columns changed for each record.
  update_cols_by_row <- lapply(update_idx, function(i) {
    names(diff_by_col)[
      vapply(diff_by_col, function(x) x[i], logical(1))
    ]
  })

  # ---- Change summary --------------------------------------------------------

  changes <- dplyr::bind_rows(lapply(compare_cols, function(col) {
    idx <- which(diff_by_col[[col]])

    if (length(idx) == 0) {
      return(NULL)
    }

    data.frame(
      matched[idx, key, drop = FALSE],
      COLUMN = col,
      OLD = .norm(matched[[paste0(col, ".OLD")]])[idx],
      NEW = .norm(matched[[col]])[idx],
      stringsAsFactors = FALSE
    )
  }))

  # ---- Catalog number clashes -----------------------------------------------

  if ("CATALOG_NUM" %in% names(new_data)) {
    old_cat <- old_key_data[
      !is.na(old_data$CATALOG_NUM),
      c(key, "CATALOG_NUM"),
      drop = FALSE
    ]

    new_cat <- new_key_data[
      !is.na(new_data$CATALOG_NUM),
      c(key, "CATALOG_NUM"),
      drop = FALSE
    ]

    clash <- new_cat %>%
      dplyr::anti_join(old_cat, by = c(key, "CATALOG_NUM")) %>%
      dplyr::semi_join(old_cat["CATALOG_NUM"], by = "CATALOG_NUM")

    if (nrow(clash) > 0) {
      warning(
        "CATALOG_NUM already in Oracle under a different key/row:\n",
        paste(unique(clash$CATALOG_NUM), collapse = ", ")
      )
    }
  }

  # ---- Summary ----------------------------------------------------------------

  message(sprintf(
    "Rows to INSERT: %d | Rows to UPDATE: %d | Unchanged: %d",
    nrow(to_insert),
    nrow(to_update),
    nrow(new_data) - nrow(to_insert) - nrow(to_update)
  ))

  if (dry_run) {
    message(
      "dry_run = TRUE: nothing written. Review, then rerun with dry_run = FALSE."
    )

    return(invisible(list(
      insert = to_insert,
      update = to_update,
      changes = changes
    )))
  }

  # ---- Safety guardrail -------------------------------------------------------
  if (nrow(to_update) > max_updates) {
    stop(sprintf(
      "ABORTED: Attempting to update %d rows, which exceeds max_updates (%d).",
      nrow(to_update), max_updates
    ))
  }

  if (nrow(new_data) < nrow(old_data) * 0.9) {
    stop(
      sprintf(
        paste0(
          "ABORTED: New dataset has %d rows, but Oracle currently has %d rows ",
          "(a >10%% drop).\nIf this deletion is intentional, please ",
          "truncate or update Oracle manually."
        ),
        nrow(new_data),
        nrow(old_data)
      )
    )
  }

  # ---- Write ------------------------------------------------------------------
  success <- FALSE
  RODBC::odbcSetAutoCommit(channel, FALSE)
  
  # ... [transaction code continues] ...

  on.exit(
    {
      if (!success) {
        message("Error encountered: rolling back transaction.")
        RODBC::odbcEndTran(channel, commit = FALSE)
      }
      RODBC::odbcSetAutoCommit(channel, TRUE)
    },
    add = TRUE
  )


  # Insert new records.
  if (nrow(to_insert) > 0) {
    my_types <- c(ID_DATE = "DATE")

    RODBC::sqlSave(
      channel,
      as.data.frame(to_insert),
      tablename = table,
      append = TRUE,
      rownames = FALSE,
      safer = TRUE,
      fast = FALSE,
      varTypes = my_types
    )
  }

  # Update existing records.
  if (nrow(to_update) > 0) {
    message("Updating ", nrow(to_update), " Oracle records...")

    for (i in seq_len(nrow(to_update))) {
      key_row <- oracle_keys_to_update[i, , drop = FALSE]
      names(key_row) <- key

      .oracle_update_row(
        channel = channel,
        row = to_update[i, , drop = FALSE],
        key_row = key_row,
        table = table,
        key = key,
        update_cols = update_cols_by_row[[i]]
      )
    }

    message("Verifying Oracle updates...")

    for (i in seq_len(nrow(to_update))) {
      key_row <- oracle_keys_to_update[i, , drop = FALSE]
      names(key_row) <- key

      .oracle_verify_row(
        channel = channel,
        row = to_update[i, , drop = FALSE],
        key_row = key_row,
        table = table,
        key = key,
        verify_cols = update_cols_by_row[[i]]
      )
    }

    message(
      "Successfully verified ",
      nrow(to_update),
      " updated Oracle records."
    )
  }

  message("Update complete.")

  success <- TRUE
  RODBC::odbcEndTran(channel, commit = TRUE)

  invisible(list(insert = to_insert, 
                 update = to_update, 
                 changes = changes)
            )
}


# ------------------------------------------------------------------------------
#' Map R data frame column classes to Oracle datatypes
#'
#' Converts R object classes into corresponding Oracle SQL column types.
#'
#' @param x A data frame or list of vectors.
#'
#' @return A character vector of Oracle datatypes matching input classes.
#'
#' @details
#' - character -> VARCHAR2(255 BYTE)
#' - logical   -> NUMBER(1,0)
#' - numeric   -> NUMBER
#' - date      -> DATE
# ------------------------------------------------------------------------------
# oracle_datatypes <- function(x) {
#   cl <- sapply(x, class)
#
#   cl[cl == "character"] <- "VARCHAR2(255 BYTE)"
#   cl[cl == "logical"] <- "NUMBER(1,0)"
#   cl[cl == "numeric"] <- "NUMBER"
#   cl[cl == "id_date"] <- "DATE"
#
#   cl
# }


# ------------------------------------------------------------------------------
#' Validate Species Vector Interactively
#'
#' @param species_vec Character vector of species names to validate.
#' @param taxonomy Data frame containing taxonomy reference. Defaults to global `taxonomy`.
#'
#' @return A character vector of validated species names.
#' @export
validate_species <- function(species_vec, taxonomy = NULL) {
  if (is.null(taxonomy)) {
    if (!exists("taxonomy", envir = .GlobalEnv)) {
      stop("Object 'taxonomy' not found in environment.")
    }
    taxonomy <- get("taxonomy", envir = .GlobalEnv)
  }

  results <- character(length(species_vec))
  prompt_history <- integer(0)

  # NEW: Initialize cache and a flag for the 'back' feature
  correction_cache <- list()
  ignore_cache_once <- FALSE

  i <- 1
  while (i <= length(species_vec)) {
    sp_input <- species_vec[i]
    safe_key <- as.character(sp_input) # Ensure valid list name

    # NEW: Check cache before running the interactive engine
    if (!ignore_cache_once && !is.na(sp_input) && safe_key %in% names(correction_cache)) {
      results[i] <- correction_cache[[safe_key]]
      i <- i + 1
      next
    }

    ignore_cache_once <- FALSE
    allow_back <- length(prompt_history) > 0

    # Run interactive engine on single item
    res <- validate_species_single(
      sp = sp_input,
      taxonomy = taxonomy,
      allow_back = allow_back,
      current_idx = i,
      total_len = length(species_vec)
    )

    # Handle 'b' / 'back' navigation jump
    if (identical(res$status, "BACK")) {
      i <- prompt_history[length(prompt_history)]
      prompt_history <- prompt_history[-length(prompt_history)]
      ignore_cache_once <- TRUE # NEW: Force the prompt when returning to this index
      next
    }

    # If the species required user intervention, add index to history stack
    if (isTRUE(res$was_flagged)) {
      prompt_history <- c(prompt_history, i)
      correction_cache[[safe_key]] <- res$value # NEW: Save the correction to cache
    }

    results[i] <- res$value
    i <- i + 1
  }

  return(results)
}


#' Helper Engine for Single Species Validation
#' @noRd
validate_species_single <- function(sp, taxonomy, allow_back = FALSE, current_idx = 1, total_len = 1) {
  # 1. Clean input upfront (remove quotes, question marks, leading/trailing spaces)
  if (is.null(sp) || is.na(sp)) {
    return(list(value = NA_character_, status = "OK", was_flagged = FALSE))
  }

  sp_input <- stringr::str_squish(stringr::str_remove_all(sp, "[\"\\?']"))

  # Early exit if string is empty or literal "NA"
  if (!nzchar(sp_input) || tolower(sp_input) == "na") {
    return(list(value = NA_character_, status = "OK", was_flagged = FALSE))
  }

  norm_str <- function(x) stringr::str_squish(stringr::str_remove_all(tolower(x), "\\.+$"))

  # --- STEP 2: DIRECT SCIENTIFIC MATCH (FIRST PASS) ---
  match_sp_idx <- which(norm_str(taxonomy$species_name) == norm_str(sp_input))
  if (length(match_sp_idx) > 0) {
    matched_val <- taxonomy$species_name[match_sp_idx[1]]
    if (!is.na(matched_val) && nzchar(matched_val)) {
      return(list(value = as.character(matched_val), status = "OK", was_flagged = FALSE))
    }
  }

  # --- STEP 3: DIRECT COMMON NAME MATCH (FIRST PASS) ---
  if ("common_name" %in% names(taxonomy)) {
    match_cm_idx <- which(norm_str(taxonomy$common_name) == norm_str(sp_input))
    if (length(match_cm_idx) > 0) {
      target_row <- match_cm_idx[1]
      sci_name <- taxonomy$species_name[target_row]
      com_name <- taxonomy$common_name[target_row]

      matched_sp <- if (!is.na(sci_name) && nzchar(sci_name)) sci_name else com_name
      return(list(value = as.character(matched_sp), status = "OK", was_flagged = FALSE))
    }
  }

  # --- STEP 3.5: SKIP OPEN NOMENCLATURE & UNCERTAIN SPECIES ---
  # Automatically accept and return as-is for entries containing "sp. cf.", "sp.cf.", "sp. ", or ending in "sp."
  if (grepl("sp\\. cf\\.|sp\\. ", sp_input)) {
    return(list(value = as.character(sp_input), status = "OK", was_flagged = FALSE))
  }

  # Standardize trailing 'sp' / 'sp.' syntax for fallback checks
  sp_clean_input <- sp_input
  if (grepl("(?i) sp$", sp_clean_input)) sp_clean_input <- paste0(sp_clean_input, ".")

  # --- HIGHER TAXONOMIC RANK DETECTION ---
  is_higher_rank <- grepl("(?i)(idae|inae|oidea|iformes|acea|ida|ina)$", sp_clean_input)

  # Detect genus-only entries (single word NOT matching higher rank suffixes)
  is_genus_only <- !is_higher_rank && (grepl("(?i)\\bsp\\.?$", sp_clean_input) || !grepl(" ", sp_clean_input))

  # --- STEP 4: GENUS-ONLY / UNKNOWN SP. FALLBACK MATCH ---
  if (is_genus_only) {
    genus_extracted <- stringr::str_extract(sp_clean_input, "^[A-Za-z]+")

    # Check A: Check for 'Genus sp.' in taxonomy
    st_sp <- paste(genus_extracted, "sp.")
    st_idx <- which(norm_str(taxonomy$species_name) == norm_str(st_sp))

    # Check B: Check for bare 'Genus' in taxonomy
    if (length(st_idx) == 0) {
      st_idx <- which(norm_str(taxonomy$species_name) == norm_str(genus_extracted))
    }

    if (length(st_idx) > 0) {
      matched_val <- taxonomy$species_name[st_idx[1]]
      if (!is.na(matched_val) && nzchar(matched_val)) {
        return(list(value = as.character(matched_val), status = "OK", was_flagged = FALSE))
      }
    } else {
      # Keep formatted genus assignment if not found in master table
      formatted_genus <- paste(genus_extracted, "sp.")
      return(list(value = as.character(formatted_genus), status = "OK", was_flagged = FALSE))
    }
  }

  # --- IF EXECUTION REACHES HERE, THE SPECIES IS FLAGGED FOR USER INTERACTION ---

  # 5. Clean string for search term creation
  sp_clean <- stringr::str_remove_all(sp_clean_input, "(?i)\\b(unident|unid|sp|spp)\\b|\\.")
  sp_clean <- stringr::str_squish(sp_clean)
  search_term <- if (nzchar(sp_clean)) sp_clean else sp_clean_input

  if (!nzchar(search_term)) {
    return(list(value = NA_character_, status = "OK", was_flagged = FALSE))
  }

  cat("\n==================================================\n")
  cat(sprintf("[%d/%d] FLAGGED SPECIES: %s\n", current_idx, total_len, sp_clean_input))
  cat("==================================================\n")

  # 6. Strict Word Search
  pattern <- paste0("(?i)\\b", search_term, "\\b")
  fuzzy_sp_rows <- grep(pattern, taxonomy$species_name, perl = TRUE)
  fuzzy_cm_rows <- integer(0)
  if ("common_name" %in% names(taxonomy)) {
    fuzzy_cm_rows <- grep(pattern, taxonomy$common_name, perl = TRUE)
  }

  match_indices <- unique(c(fuzzy_sp_rows, fuzzy_cm_rows))

  # 7. Fuzzy search fallback
  if (length(match_indices) == 0) {
    fuzzy_sp_rows <- agrep(search_term, taxonomy$species_name, ignore.case = TRUE, max.distance = 0.1)
    if ("common_name" %in% names(taxonomy)) {
      fuzzy_cm_rows <- agrep(search_term, taxonomy$common_name, ignore.case = TRUE, max.distance = 0.1)
    }
    match_indices <- unique(c(fuzzy_sp_rows, fuzzy_cm_rows))
  }

  # 8. Interactive Prompt with Genus-Level Filtering
  if (length(match_indices) > 0) {
    matches <- taxonomy[match_indices, , drop = FALSE]

    if (is_genus_only) {
      genus_matches <- matches %>%
        dplyr::filter(
          grepl("(?i)\\bsp\\.?$|(?i)\\bsp\\b", species_name) |
            !grepl(" ", stringr::str_trim(species_name))
        )

      if (nrow(genus_matches) > 0) {
        matches <- genus_matches
      }
    }

    matches <- matches %>%
      dplyr::mutate(display_name = dplyr::coalesce(species_name, common_name))

    if ("common_name" %in% names(matches)) {
      g <- matches[, c("display_name", "common_name"), drop = FALSE]
      g <- g[!duplicated(g$display_name), ]
      g <- g[order(g$display_name), ]
    } else {
      g <- data.frame(display_name = unique(matches$display_name), stringsAsFactors = FALSE)
      g <- g[order(g$display_name), , drop = FALSE]
    }

    rownames(g) <- NULL

    cat(sprintf("\n'%s' not found in database. Close matches found:\n", sp_clean_input))
    print(g)

    prompt_msg <- "\nEnter a row number, type a custom species name manually"
    if (allow_back) prompt_msg <- paste0(prompt_msg, ", type 'b' to go back")
    prompt_msg <- paste0(prompt_msg, ", or press Enter to keep original:\n")

    valid_input <- FALSE
    while (!valid_input) {
      cat(prompt_msg)
      take <- readline(prompt = "> ")

      if (tolower(take) %in% c("b", "back")) {
        if (allow_back) {
          return(list(value = NULL, status = "BACK", was_flagged = TRUE))
        }
        cat("No previous flagged species to return to.\n")
        next
      }

      if (nzchar(take)) {
        num_take <- suppressWarnings(as.numeric(take))

        if (!is.na(num_take)) {
          if (num_take %in% seq_len(nrow(g))) {
            res_sp <- g$display_name[num_take]
            cat(sprintf("Input accepted, selected taxon '%s'.\n", res_sp))
            valid_input <- TRUE
          } else {
            cat(sprintf("Invalid row number '%d'. Enter a number between 1 and %d, or type a text species name.\n", num_take, nrow(g)))
          }
        } else {
          res_sp <- stringr::str_remove_all(take, "[\"']")
          cat(sprintf("Manual entry accepted: '%s'.\n", res_sp))
          valid_input <- TRUE
        }
      } else {
        res_sp <- sp_clean_input
        cat(sprintf("No input provided. Keeping original name: '%s'\n", res_sp))
        valid_input <- TRUE
      }
    }
  } else {
    cat("\nNo similar species found in database.\n")

    prompt_msg <- "Type a custom species name manually"
    if (allow_back) prompt_msg <- paste0(prompt_msg, ", type 'b' to go back")
    prompt_msg <- paste0(prompt_msg, sprintf(", or press Enter to keep original ('%s'):\n", sp_clean_input))

    valid_input <- FALSE
    while (!valid_input) {
      cat(prompt_msg)
      take <- readline(prompt = "> ")

      if (tolower(take) %in% c("b", "back")) {
        if (allow_back) {
          return(list(value = NULL, status = "BACK", was_flagged = TRUE))
        }
        cat("No previous flagged species to return to.\n")
        next
      }

      if (nzchar(take)) {
        num_take <- suppressWarnings(as.numeric(take))
        if (!is.na(num_take)) {
          cat("Numbers cannot be accepted as species names. Please enter a text species name or press Enter.\n")
        } else {
          res_sp <- stringr::str_remove_all(take, "[\"']")
          cat(sprintf("Manual entry accepted: '%s'.\n", res_sp))
          valid_input <- TRUE
        }
      } else {
        res_sp <- sp_clean_input
        valid_input <- TRUE
      }
    }
  }

  return(list(value = as.character(res_sp), status = "OK", was_flagged = TRUE))
}

# ------------------------------------------------------------------------------
#' Lookup species name from species code
#'
#' Searches current and historical taxonomy tables.
#'
#' @param code Species code.
#'
#' @return Species name or NA if not found.
# ------------------------------------------------------------------------------
lookup_name <- function(code) {
  nm <- classification$species_name[
    which(classification$species_code == code)
  ]

  if (length(nm) == 0) {
    nm <- changes$old_species_name[
      which(changes$old_species_code == code)
    ]
  }

  nm
}

# ------------------------------------------------------------------------------
#' Validate Cruise, Vessel, and Haul Combinations Against Master Haul Table
#'
#' Checks whether all `cruise`, `vessel`, and `haul` identifier combinations in
#' a target dataset exist within a master haul reference table (e.g., `all_haul`).
#' Returns `TRUE` invisibly if all records match, or returns a data frame of the
#' invalid combinations if mismatches are found.
#'
#' @param data A data frame containing the record data to validate. Must contain
#'   columns `cruise`, `vessel`, and `haul`.
#' @param all_haul A data frame representing the master reference table. Must contain
#'   columns `cruise`, `vessel`, and `haul`.
#'
#' @return Invisibly returns `TRUE` if all combinations in `data` are valid.
#'   If invalid combinations are found, throws a warning and invisibly returns
#'   a data frame containing the unique invalid `cruise-vessel-haul` records.
#'
#' @importFrom dplyr select all_of distinct anti_join
#' @export
# ------------------------------------------------------------------------------

validate_haul <- function(data, haul_data = NULL) {
  if (is.null(haul_data)) {
    if (!exists("all_haul", envir = .GlobalEnv)) {
      stop("Object 'all_haul' not found in environment.")
    }
    haul_data <- get("all_haul", envir = .GlobalEnv)
  }


  # 1. Define required columns
  req_cols <- c("cruise", "vessel", "haul")

  # Check column existence
  missing_data <- setdiff(req_cols, names(data))
  missing_haul <- setdiff(req_cols, names(haul_data))
  
  if (length(missing_data) > 0) {
    stop("Input 'data' is missing required columns: ", paste(missing_data, collapse = ", "))
  }
  if (length(missing_haul) > 0) {
    stop("Input 'all_haul' is missing required columns: ", paste(missing_haul, collapse = ", "))
  }

  # 2. Extract unique combinations from both tables to avoid row explosion
  data_keys <- data %>%
    dplyr::select(dplyr::all_of(req_cols)) %>%
    dplyr::distinct()

  haul_keys <- haul_data %>%
    dplyr::select(dplyr::all_of(req_cols)) %>%
    dplyr::distinct()

  # 3. Identify invalid keys via anti_join
  invalid_records <- dplyr::anti_join(data_keys, haul_keys, by = req_cols)

  # 4. Report status
  if (nrow(invalid_records) == 0) {
    message("Validation passed: All cruise-vessel-haul combinations exist in database.")
    return(invisible(TRUE))
  } else {
    warning("Validation failed: Found ", nrow(invalid_records), " invalid cruise-vessel-haul combination(s).")
    message("\nInvalid combinations:")
    print(invalid_records)

    return(invisible(invalid_records))
  }
}


# ==============================================================================
# Generate completed voucher dataset with haul metadata
# ==============================================================================

#' Create completed voucher dataset for museum requests
#'
#' Reads a data request file, joins haul metadata, and writes a cleaned
#' output file containing completed voucher information.
#'
#' @param input_file Path to Excel file containing voucher requests
#' @param haul_data Data frame containing haul metadata (e.g., `all_haul`)
#' @param output_file Path to write the completed CSV
#' @param join_cols Named vector specifying join columns
#'
#' @return Invisibly returns the completed data frame
#' @export
get_specimen_metadata <- function(
  input_file,
  haul_data = all_haul,
  cruise_col = "cruise",
  vessel_col = "vessel",
  haul_col = "haul",
  output_file = here::here("output", "data requests", paste0(basename(tools::file_path_sans_ext(input_file)), "_completed.csv"))
) {
  # ---------------------------------------------------------------------------
  # Read input request file
  # ---------------------------------------------------------------------------

  message("Reading input file: ", input_file)

  ext <- tolower(tools::file_ext(input_file))

  qs <- switch(ext,
    "csv"  = readr::read_csv(input_file, show_col_types = FALSE),
    "xlsx" = readxl::read_excel(input_file, sheet = 1),
    "xls"  = readxl::read_excel(input_file, sheet = 1),
    stop("Unsupported file type '.", ext, "'. Must be .csv, .xls, or .xlsx.")
  )

  # ---------------------------------------------------------------------------
  # Build join spec and arrange vars from col name args
  # ---------------------------------------------------------------------------

  join_cols <- c(cruise_col, vessel_col, haul_col)

  # Named vector for left_join: names = qs cols, values = haul_data cols
  # (here they're the same names, but this keeps it flexible)
  join_spec <- setNames(c("cruise", "vessel", "haul"), join_cols)

  # Syms for arrange(), which needs unquoted column references
  arrange_vars <- rlang::syms(join_cols)


  # ---------------------------------------------------------------------------
  # Join with haul metadata and clean
  # ---------------------------------------------------------------------------

  completed <- qs %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(join_cols), as.numeric)) %>%
    dplyr::left_join(haul_data, by = join_spec) %>%
    dplyr::select(-dplyr::contains("join")) %>%
    dplyr::arrange(!!!arrange_vars)


  # ---------------------------------------------------------------------------
  # Write output file
  # ---------------------------------------------------------------------------

  message("Writing output file: ", output_file)

  readr::write_csv(completed, output_file)


  # ---------------------------------------------------------------------------
  # Return result
  # ---------------------------------------------------------------------------

  invisible(completed)
}

# ---------------------------------------------------------------------------
#' Get Species Code from Taxonomy Database
#'
#' Searches the taxonomy database by common or scientific species name. Handles
#' exact matching, fuzzy matching, taxon updates, and optional user prompts.
#'
#' @param sp A character string specifying the species common or scientific name to query.
#'
#' @return A \code{data.frame} with two character columns:
#'   \item{valid_id}{The validated species name or common name.}
#'   \item{species_code}{The corresponding numerical or alphanumeric species code, or \code{NA}.}
#'
#' @importFrom stringr str_remove str_extract
#' @importFrom dplyr arrange
#'
#' @global taxonomy changes
#'
#' @export
get_species_code_single <- function(sp, allow_back = TRUE) {
  # Early exit for NA, NULL, or empty inputs
  if (is.null(sp) || is.na(sp) || !nzchar(trimws(sp))) {
    return(data.frame(
      valid_id = "NA",
      species_code = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Clean input string safely (keep original period-stripped string for matching)
  sp_raw <- trimws(sp)
  sp_clean <- stringr::str_remove(tolower(sp_raw), "\\.")

  # --- STEP 1: Direct Exact Match Check ---
  tax_sp_clean <- stringr::str_remove(tolower(taxonomy$species_name), "\\.")
  tax_com_clean <- stringr::str_remove(tolower(taxonomy$common_name), "\\.")

  exact_sp_idx <- which(tax_sp_clean == sp_clean)
  exact_com_idx <- which(tax_com_clean == sp_clean)

  if (length(exact_sp_idx) > 0) {
    sp_found <- taxonomy$species_name[exact_sp_idx[1]]
  } else if (length(exact_com_idx) > 0) {
    sp_found <- taxonomy$common_name[exact_com_idx[1]]
  } else {
    sp_found <- NULL
  }

  # --- SKIP OPEN NOMENCLATURE & UNCERTAIN SPECIES ---
  # ONLY skip if we didn't just find a valid exact match in Step 1
  if (is.null(sp_found) && grepl("(?i)\\bsp\\.?\\s*cf\\.|(?i)\\bsp\\.\\s|(?i)\\bsp\\.$", sp_raw)) {
    return(data.frame(
      valid_id = sp_raw,
      species_code = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # --- STEP 2: Fuzzy Search Fallback & Narrowing ---
  if (is.null(sp_found)) {
    g <- taxonomy$common_name[agrep(sp_clean, taxonomy$common_name, ignore.case = TRUE)]
    ss <- taxonomy$species_name[agrep(sp_clean, stringr::str_remove(taxonomy$species_name, "\\."), ignore.case = TRUE)]
    gg <- unique(c(g, ss))

    # Check taxonomy updates dataset if no matches found
    if (length(gg) == 0 && isTRUE(grepl(" ", sp_clean))) {
      old_names_clean <- stringr::str_remove(changes$old_species_name, "\\.")
      gg <- changes$new_species_name[agrep(sp_clean, old_names_clean)]

      if (length(gg) == 0) {
        g2 <- stringr::str_extract(sp_clean, "[A-Za-z]+$")
        if (!is.na(g2) && nzchar(g2)) {
          gg <- taxonomy$common_name[agrep(g2, taxonomy$common_name, ignore.case = TRUE)]
        }
      }
    }

    # --- CANDIDATE NARROWING LOGIC ---
    category_keywords <- c("egg case")
    matched_keyword <- category_keywords[sapply(category_keywords, function(k) grepl(k, sp_clean, ignore.case = TRUE))]

    if (length(matched_keyword) > 0 && length(gg) > 0) {
      kw <- matched_keyword[1]
      gg_narrowed <- gg[grepl(kw, gg, ignore.case = TRUE)]
      if (length(gg_narrowed) > 0) {
        gg <- gg_narrowed
      }
    }

    candidates <- data.frame(species = unique(gg), stringsAsFactors = FALSE) %>%
      dplyr::arrange(species)

    if (nrow(candidates) == 1) {
      sp_found <- candidates$species[1]
    } else if (nrow(candidates) > 1 || (nrow(candidates) == 0 && interactive())) {
      if (!interactive()) {
        if (nrow(candidates) > 0) {
          message("Non-interactive session: Multiple matches for '", sp, "'. Defaulting to first match: ", candidates$species[1])
          sp_found <- candidates$species[1]
        } else {
          sp_found <- "NA"
        }
      } else {
        # Interactive prompt
        if (nrow(candidates) > 0) {
          print(candidates)
          cat(sprintf("\n'%s' not found in database. Closest matches found above.\n", sp))
        } else {
          cat(sprintf("\nNo similar species found in database for '%s'.\n", sp))
        }

        prompt_msg <- "Enter row #, type a custom species name manually"
        if (allow_back) prompt_msg <- paste0(prompt_msg, ", type 'b' to go back")
        prompt_msg <- paste0(prompt_msg, ", or press Enter to keep original:\n")

        valid_input <- FALSE
        while (!valid_input) {
          cat(prompt_msg)
          take <- readline(prompt = "> ")
          take_clean <- stringr::str_squish(take)

          # 1. Navigation: Go Back
          if (tolower(take_clean) %in% c("b", "back")) {
            if (allow_back) {
              return(data.frame(valid_id = "BACK", species_code = NA_real_, stringsAsFactors = FALSE))
            } else {
              cat("Already at the first flagged species. Cannot go back further.\n")
              next
            }
          }

          # 2. Process Input
          if (nzchar(take_clean)) {
            num_take <- suppressWarnings(as.numeric(take_clean))

            if (!is.na(num_take)) {
              if (nrow(candidates) > 0 && num_take %in% seq_len(nrow(candidates))) {
                sp_found <- candidates$species[num_take]
                cat(sprintf("Input accepted: taking taxon '%s'.\n", sp_found))
                valid_input <- TRUE
              } else {
                if (nrow(candidates) > 0) {
                  cat(sprintf("Invalid row number '%d'. Enter a number between 1 and %d, or type a text name.\n", num_take, nrow(candidates)))
                } else {
                  cat("Numbers cannot be accepted as species names. Type a text name or press Enter.\n")
                }
              }
            } else {
              sp_found <- stringr::str_remove_all(take_clean, "[\"']")
              cat(sprintf("Manual entry accepted: '%s'.\n", sp_found))
              valid_input <- TRUE
            }
          } else {
            sp_found <- sp_raw
            cat(sprintf("Keeping original name: '%s'\n", sp_found))
            valid_input <- TRUE
          }
        }
      }
    } else {
      sp_found <- "NA"
    }
  }

  # --- STEP 3: Retrieve Species Code ---
  if (!is.null(sp_found) && !is.na(sp_found) && sp_found != "NA") {
    code_idx <- which(sp_found == taxonomy$common_name)
    if (length(code_idx) == 0) {
      code_idx <- which(sp_found == taxonomy$species_name)
    }

    code <- if (length(code_idx) > 0) as.numeric(taxonomy$species_code[code_idx[1]]) else NA_real_
  } else {
    sp_found <- "NA"
    code <- NA_real_
  }

  data.frame(
    valid_id = sp_found,
    species_code = as.numeric(code),
    stringsAsFactors = FALSE
  )
}


get_species_code <- function(species_vec, current_codes = NULL, na_only = TRUE) {
  # Safety check
  if (!is.null(current_codes) && length(current_codes) != length(species_vec) && na_only) {
    stop("Length of 'current_codes' must match 'species_vec'.")
  }

  results_list <- vector("list", length(species_vec))
  prompt_history <- integer(0)

  correction_cache <- list()
  ignore_cache_once <- FALSE

  i <- 1
  while (i <= length(species_vec)) {
    # NEW: If a valid code already exists, keep it and skip to the next row
    if (!is.null(current_codes) && !is.na(current_codes[i])) {
      results_list[[i]] <- data.frame(
        valid_id = as.character(species_vec[i]),
        species_code = as.numeric(current_codes[i]),
        stringsAsFactors = FALSE
      )
      i <- i + 1
      next
    }

    sp_input <- species_vec[i]
    safe_key <- as.character(sp_input)

    # Check cache before processing
    if (!ignore_cache_once && !is.na(sp_input) && safe_key %in% names(correction_cache)) {
      results_list[[i]] <- correction_cache[[safe_key]]
      i <- i + 1
      next
    }

    ignore_cache_once <- FALSE
    allow_back <- length(prompt_history) > 0

    sp_clean <- stringr::str_remove(tolower(trimws(sp_input)), "\\.")
    tax_sp_clean <- stringr::str_remove(tolower(taxonomy$species_name), "\\.")
    tax_com_clean <- stringr::str_remove(tolower(taxonomy$common_name), "\\.")
    is_direct_match <- (sp_clean %in% tax_sp_clean) || (sp_clean %in% tax_com_clean)
    is_sp_cf <- grepl("sp cf\\.", trimws(sp_input))

    # Run single-item function
    res <- get_species_code_single(sp_input, allow_back = allow_back)

    # Intercept BACK signal
    if (identical(res$valid_id, "BACK")) {
      i <- prompt_history[length(prompt_history)]
      prompt_history <- prompt_history[-length(prompt_history)]
      ignore_cache_once <- TRUE
      next
    }

    # Store item in cache and history if it required interactive resolution
    if (interactive() && !is_direct_match && !is_sp_cf) {
      if (length(prompt_history) == 0 || prompt_history[length(prompt_history)] != i) {
        prompt_history <- c(prompt_history, i)
        correction_cache[[safe_key]] <- res
      }
    }

    results_list[[i]] <- res
    i <- i + 1
  }

  # Extract and return just the numeric species_code as a flat vector
  extracted_codes <- sapply(results_list, function(x) x$species_code)
  return(as.numeric(extracted_codes))
}

