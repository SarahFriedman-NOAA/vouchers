# ==============================================================================
# UPLOAD COMPLETED VOUCHER DATA TO ORACLE
# ==============================================================================


# ------------------------------------------------------------------------------
# Define year and file paths
# ------------------------------------------------------------------------------

last_year <- as.numeric(format(Sys.time(), "%Y")) - 1

drive_name   <- paste0("voucher_data_", last_year, "_fish_edited")
local_name   <- paste0("voucher_data_", last_year, "_fish_completed.csv")
local_path   <- here::here("output", "completed", local_name)


# ------------------------------------------------------------------------------
# Download completed voucher data from Google Drive (if not already local)
# ------------------------------------------------------------------------------

if (!file.exists(local_path)) {
  
  message("Downloading completed voucher file from Google Drive...")
  
  googledrive::drive_download(
    file      = drive_name,
    path      = local_path,
    type      = "csv",
    overwrite = FALSE
  )
}


# ------------------------------------------------------------------------------
# Read and clean new voucher data
# ------------------------------------------------------------------------------

new_fish_vouch <- readr::read_csv(local_path)

new_vouch <- new_fish_vouch %>%
  dplyr::filter(!is.na(new_id)) %>%                     # keep only resolved IDs
  dplyr::select(-field_family) %>%                      # drop unused column
  dplyr::mutate(
    date   = as.Date(date, format = "%m/%d/%Y"),
    new_id = purrr::map_chr(new_id, check_species)      # fix misspellings
  ) %>%
  dplyr::left_join(
    taxonomy %>% dplyr::select(species_name, species_code),
    by = c("new_id" = "species_name")
  ) %>%
  dplyr::select(
    field_id:new_id,
    any_of(c(
      "species_code",
      "catalog_num"
    )),
    n_specimens:notes
  ) %>%
  dplyr::arrange(new_id) %>%
  dplyr::rename_with(toupper)


# ------------------------------------------------------------------------------
# Validate species resolution (must fix before proceeding)
# ------------------------------------------------------------------------------

unresolved <- new_vouch %>%
  dplyr::filter(is.na(SPECIES_CODE))

stopifnot("Resolve species name issues before upload" = nrow(unresolved) == 0)



# ------------------------------------------------------------------------------
# Combine new and existing voucher data
# ------------------------------------------------------------------------------

vouchers <- existing_vouch %>%
  rename_all(toupper) %>%
  dplyr::bind_rows(new_vouch) %>%
  dplyr::distinct(
    FIELD_ID, REGION, CRUISE, VESSEL_ID, HAUL, VOUCHER, NEW_ID,
    .keep_all = TRUE
  ) %>%
  dplyr::mutate(
    DATE = as.Date(DATE, format = "%m/%d/%Y")
  ) %>%
  dplyr::arrange(
    dplyr::desc(CRUISE),
    VESSEL_ID,
    HAUL
  )



# ------------------------------------------------------------------------------
# Establish Oracle connection (GAP products schema)
# ------------------------------------------------------------------------------

if (file.exists("Z:/Projects/ConnectToOracle_GAPproducts.R")) {
  
  message("Using local Oracle connection script.")
  source("Z:/Projects/ConnectToOracle_GAPproducts.R")
  
} else {
  
  message("Using gapindex::get_connected() fallback.")
  gapindex::get_connected()
}



# ------------------------------------------------------------------------------
# Construct table metadata for upload
# ------------------------------------------------------------------------------

metadata <- tibble::tibble(
  COLNAME  = colnames(vouchers),
  datatype = oracle_datatypes(vouchers)
) %>%
  dplyr::left_join(
    RODBC::sqlQuery(
      channel = channel,
      query   = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN"
    ),
    by = c("COLNAME" = "METADATA_COLNAME")
  ) %>%
  dplyr::rename_with(~ stringr::str_remove(.x, "METADATA_")) %>%
  dplyr::mutate(
    DATATYPE = dplyr::if_else(is.na(DATATYPE), datatype, DATATYPE)
  ) %>%
  dplyr::select(-datatype) %>%
  janitor::clean_names() 


# ------------------------------------------------------------------------------
# Apply manual datatype overrides
# ------------------------------------------------------------------------------

metadata$datatype[metadata$colname == "weight_kg"] <- "NUMBER(38,3)"
metadata$datatype[metadata$colname == "notes"]     <- "VARCHAR2(4000 BYTE)"


# ------------------------------------------------------------------------------
# Upload combined voucher table to Oracle
# ------------------------------------------------------------------------------

message("Uploading combined voucher table to Oracle...")

gapindex::upload_oracle(
  x                    = vouchers,
  table_name           = "VOUCHERS",
  table_metadata       = "Voucher table",
  metadata_column      = metadata,
  channel              = channel,
  schema               = "GAP_PRODUCTS",
  share_with_all_users = TRUE
)

message("Upload complete.")