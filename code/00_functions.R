# ==============================================================================
# Species and Oracle Utility Functions
# ==============================================================================


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
oracle_datatypes <- function(x) {
  cl <- sapply(x, class)
  
  cl[cl == "character"] <- "VARCHAR2(255 BYTE)"
  cl[cl == "logical"]   <- "NUMBER(1,0)"
  cl[cl == "numeric"]   <- "NUMBER"
  cl[cl == "date"]      <- "DATE"
  
  cl
}



# ------------------------------------------------------------------------------
#' Resolve species name to valid Racebase species code
#'
#' Attempts to match a user-provided species string to Racebase taxonomy.
#' Uses fuzzy matching against common and scientific names, then falls back
#' to taxonomic change tables if needed. If multiple matches are found,
#' user is prompted to select one interactively.
#'
#' @param sp Character string species name to resolve.
#'
#' @return A data frame with:
#'   \describe{
#'     \item{valid_id}{Resolved species name}
#'     \item{species_code}{Corresponding species code}
#'   }
# ------------------------------------------------------------------------------
get_species_code <- function(sp) {
  
  # Clean input
  sp <- stringr::str_remove(tolower(sp), "\\.")
  
  ## --------------------------------------------------------------------------
  ## Attempt fuzzy match against known names
  ## --------------------------------------------------------------------------
  g  <- classy$common_name[
    agrep(sp, classy$common_name, ignore.case = TRUE)
  ]
  
  ss <- classy$species_name[
    agrep(sp, stringr::str_remove(classy$species_name, "\\."), ignore.case = TRUE)
  ]
  
  gg <- c(g, ss)
  
  ## --------------------------------------------------------------------------
  ## Direct match or near-match handling
  ## --------------------------------------------------------------------------
  if (
    any(stringr::str_remove(tolower(gg), "\\.") == sp) |
    any(tolower(gg) == paste(sp, "sp."))
  ) {
    
    ind <- which(stringr::str_remove(tolower(gg), "\\.") == sp)
    
    if (length(ind) == 0) {
      ind <- which(tolower(gg) == paste(sp, "sp."))
    }
    
    sp <- gg[[ind]]
  }
  
  ## --------------------------------------------------------------------------
  ## If no valid match found, attempt broader searches or user selection
  ## --------------------------------------------------------------------------
  if (length(gg) == 0 || (!any(tolower(gg) == sp) && !exists("ind"))) {
    
    if (length(gg) == 0 & grepl(" ", sp)) {
      
      gg <- tax_changes$new_species_name[
        agrep(sp, stringr::str_remove(tax_changes$old_species_name, "\\."))
      ]
      
      if (length(gg) == 0) {
        g2 <- stringr::str_extract(sp, "[A-Za-z]+$")
        gg <- classy$common_name[
          agrep(g2, classy$common_name, ignore.case = TRUE)
        ]
      }
    }
    
    ## Format candidate matches
    gg <- data.frame(species = gg) %>%
      dplyr::arrange(species)
    
    rownames(gg) <- NULL
    
    if (nrow(gg) > 0) {
      
      if (nrow(gg) == 1) {
        
        sp <- gg$species[[1]]
        
      } else {
        
        print(gg)
        
        message(
          paste(
            sp,
            "not found in racebase. Here are the closest matches.\nEnter row number of taxon:\n"
          )
        )
        
        take <- scan(n = 1, quiet = TRUE, what = "raw")
        
        if (length(take) == 0) {
          take <- 0
          message("No species selected.\n")
          sp <- "NA"
        }
        
        if (take %in% seq_len(nrow(gg))) {
          take <- as.numeric(take)
          message(
            "Input accepted, selected taxon '",
            as.character(gg$species[take]), "'.\n"
          )
          sp <- gg$species[take]
        }
      }
      
    } else {
      sp <- "NA"
    }
    
  } else {
    message(sp, " found in database!\n")
  }
  
  ## --------------------------------------------------------------------------
  ## Retrieve species code if valid match exists
  ## --------------------------------------------------------------------------
  if (sp != "NA") {
    
    co <- classy$species_code[
      which(sp == classy$common_name)
    ]
    
    if (length(co) == 0) {
      co <- classy$species_code[
        which(sp == classy$species_name)
      ]
    }
    
  } else {
    co <- NA
  }
  
  data.frame(valid_id = sp, species_code = co)
}



# ------------------------------------------------------------------------------
# Regular expressions for cleaning species names
# ------------------------------------------------------------------------------

rm_bits <- paste0(
  c(
    "\\(adult\\)", "\\(juvenile\\)", "egg case", "egg",
    "hybrid", "larva", "larvae", "tubes", "group"
  ),
  collapse = "| "
)

rm_all <- paste0(rm_bits, "| sp\\. cf\\.| sp\\.| cf\\.")



# ------------------------------------------------------------------------------
#' Check if species exists in database (interactive fallback)
#'
#' Attempts to validate species name against database. If no exact match is
#' found, performs fuzzy matching and optionally prompts user for selection.
#'
#' @param sp Character species name.
#'
#' @return Validated species name (possibly user-selected).
# ------------------------------------------------------------------------------
check_species <- function(sp) {
  
  sp <- stringr::str_remove_all(sp, "\\?")
  
  if (!sp %in% species0$species) {
    
    if (!grepl(" ", sp) | grepl("|sp\\.$", sp)) {
      
      st <- paste(
        stringr::str_extract(sp, "[A-Za-z]+"),
        "sp."
      )
      
      if (st %in% species0$species) {
        stop(return(st))
      }
    }
    
    st <- species0$species[
      agrep(sp, species0$species, ignore.case = TRUE)
    ]
    
    message("\n\n")
    
    g <- data.frame(species = st) %>%
      dplyr::arrange(species)
    
    rownames(g) <- NULL
    
    if (nrow(g > 0)) {
      
      message(
        "\n", sp,
        " not found in racebase. Here are closest matches.\nEnter row number:\n"
      )
      
      print(g)
      
      take <- scan(n = 1, quiet = TRUE, what = "raw")
      
      if (length(take) == 0) {
        message("\nNo species selected. Keeping original name\n")
        stop(return(sp))
      }
      
      if (take %in% seq_len(NROW(g))) {
        take <- as.numeric(take)
        
        message(
          "Input accepted, selected taxon '",
          as.character(g$species[take]), "'.\n"
        )
        
        sp <- g$species[take]
      }
      
    } else {
      cat("No similar species found in database. Check spelling.\n")
    }
  }
  
  sp
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
  
  nm <- classy$species_name[
    which(classy$species_code == code)
  ]
  
  if (length(nm) == 0) {
    nm <- tax_changes$old_species_name[
      which(tax_changes$old_species_code == code)
    ]
  }
  
  nm
}



# ------------------------------------------------------------------------------
#' Retrieve catch record field ID
#'
#' Finds matching catch record based on cruise, vessel, haul, and field ID.
#'
#' @param record A list or data frame row containing:
#'   cruise, vessel, haul, new_id
#'
#' @return Matching field_id or NA if not found.
# ------------------------------------------------------------------------------
catch_lookup <- function(record) {
  
  nn <- catch %>%
    dplyr::filter(
      cruise   == record$cruise &
        vessel   == record$vessel &
        haul     == record$haul &
        field_id == record$new_id
    )
  
  if (nrow(nn) == 1) {
    out <- nn$field_id
  } else {
    NA
  }
  
  out
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
    output_file = here::here("output", basename(input_file), "_completed.csv"),
    join_cols = c("cruise" = "cruise", "vessel" = "vessel", "haul" = "haul")
    ) {
  
  # ---------------------------------------------------------------------------
  # Read input request file
  # ---------------------------------------------------------------------------
  
  message("Reading input file: ", input_file)
  
  qs <- readxl::read_excel(input_file)
  
  
  # ---------------------------------------------------------------------------
  # Join with haul metadata and clean
  # ---------------------------------------------------------------------------
  
  completed <- qs %>%
    dplyr::left_join(haul_data, by = join_cols) %>%
    dplyr::select(-dplyr::contains("join")) %>%
    dplyr::arrange(cruise, haul, vessel)
  
  
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