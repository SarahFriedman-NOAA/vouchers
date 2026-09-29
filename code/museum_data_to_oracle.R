# ==============================================================================
# MUSEUM DATA WRANGLING → ORACLE VOUCHER FORMAT
# ==============================================================================

# This script standardizes museum-provided voucher data 
# into a format consistent with the Oracle voucher table.


# ==============================================================================
# 1. STATIC LOOKUPS
# ==============================================================================

vessels <- tibble::tribble(
  ~vessel, ~vessel_id,
  "Ocean Explorer", 148,
  "Sea Storm", 143,
  "Alaska Provider", 176,
  "Vesteraalen", 94,
  "Miller Freeman", 21,
  "Northwest Explorer", 134,
  "Alaska Knight", 162,
  "Morning Star", 57
)



# ==============================================================================
# 3. INGEST + CLEAN SOURCE DATA
# ==============================================================================

# # ---- UW DATA ------------------------------------------------------------------
# 
# uw_info <- readr::read_csv("data/updates/voucher_fishes_uw_2023.csv") %>%
#   janitor::clean_names() %>%
#   dplyr::relocate(catalog_num, .after = dplyr::last_col()) %>%
#   dplyr::mutate(
#     year = as.numeric(stringr::str_extract(cruise, "^[0-9]{4}")),
#     region = dplyr::case_when(
#       vessel %in% c(162, 94, 134) ~ "BS",
#       vessel %in% c(148, 176) & year %% 2 == 0 ~ "AI",
#       vessel %in% c(148, 176) & year %% 2 != 0 ~ "GOA",
#       TRUE ~ NA_character_
#     )
#   )


# ---- CAL ACADEMY DATA --------------------------------------------------------

museum_data <- readxl::read_xlsx("data/updates/voucher_inverts_cas_2024.xlsx") %>%
  janitor::clean_names() %>%
  tidyr::separate(
    prev_no,
    into = c("cruise", "vessel", "haul", "voucher"),
    convert = TRUE
  ) 


# Double checking all cruise/vessel/haul combinations
museum_data %>%
  validate_haul(., all_haul)



validated_museum_data <- museum_data %>%
  dplyr::mutate(
    collected_by = ifelse(
      grepl("aboard", determiner_last_name),
      stringr::str_remove(determiner_last_name, " aboard .+$|F\\/V .+$"),
      NA
    ),
    year        = as.numeric(stringr::str_extract(cruise, "[0-9]{4}")),
    catalog_num = paste("CAS", catalog_number),
    voucher     = ifelse(is.na(voucher), 1, voucher),
    disposition = "95% ETOH",
    notes       = ifelse(!is.na(remarks), paste0(notes, "; ", remarks), notes),
    field_id    = validate_species(field_id_transcribed),
    field_species_code = purrr::map_dbl(field_id, ~ get_species_code(.x)$species_code)
  ) %>%
  dplyr::select(
    field_id, field_species_code,
    new_id = full_name,
    cruise, vessel, haul, voucher,
    year, disposition, collected_by,
    taxonomist = determiner_last_name,
    notes, catalog_num
  )


# ==============================================================================
# 4. HARMONIZE + BUILD VOUCHER TABLE
# ==============================================================================

message("Building unified voucher dataset...")

all_catch_species <- left_join(all_catch,
  classification,
  by = "species_code"
) %>%
  select(-family, -species_code)


new_vouch <- validated_museum_data %>%
  dplyr::filter(!is.na(new_id)) %>%
  dplyr::mutate(
    new_id = validate_species(new_id),
    new_species_code = purrr::map_dbl(new_id, ~ get_species_code(.x)$species_code)
  ) %>%
  dplyr::left_join(all_catch_species, by = dplyr::join_by(field_id == species_name, cruise, haul, vessel, voucher)) %>%
  dplyr::left_join(all_haul, by = c(cruise, vessel, haul)) %>%
  dplyr::mutate(
    standard_length_mm = NA,
    n_specimens = NA,
    id_date = NA
  ) %>%
  dplyr::select(hauljoin, new_id, new_species_code, catalog_num, region,
                cruise, year, vessel_id, haul, voucher, field_id,
                field_species_code, n_specimens, standard_length_mm, 
                weight_kg, preservative, collected_by, id_date, 
                taxonomist, notes
                )



# ==============================================================================
# 5. QA CHECKS
# ==============================================================================

# Final IDs not in our database
new_vouch %>%
  dplyr::filter(is.na(species_code)) %>%
  View()


# field IDs not in our database
new_vouch %>%
  left_join(taxonomy, by = c("field_id" = "species_name")) %>%
  select(field_id, field_species_code) %>%
  filter(is.na(field_species_code) & field_id != "NA")


# ==============================================================================
# 6. COMBINE WITH EXISTING + FINAL FORMAT
# ==============================================================================

vouchers <- new_vouch %>%
  dplyr::mutate(
    date = as.Date(date, format = "%m/%d/%Y")
  ) %>%
  dplyr::mutate(across(
    c(cruise, year, vessel_id, haul, voucher, species_code, n_specimens),
    as.integer
  )) %>%
  dplyr::bind_rows(existing_vouch) %>%
  dplyr::distinct(cruise, vessel_id, haul, voucher, .keep_all = TRUE) %>%
  dplyr::select(
    field_id, region, cruise, year, vessel_id, haul, voucher,
    weight_kg, new_id, species_code, catalog_num,
    n_specimens, standard_length_mm,
    preservative = disposition,
    collected_by, date, taxonomist, notes
  ) %>%
  dplyr::rename_with(toupper) %>%
  dplyr::arrange(YEAR, REGION, CRUISE, VESSEL_ID, HAUL)


# Check to ensure no duplicate catalog nums
vouchers %>%
  dplyr::filter(duplicated(catalog_num) & !is.na(catalog_num))



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

# message("Uploading combined voucher table to Oracle...")
# 
# gapindex::upload_oracle(
#   x                    = vouchers,
#   table_name           = "vouchers",
#   table_metadata       = "Voucher table",
#   metadata_column      = metadata,
#   channel              = channel,
#   schema               = "RACE_DATA",
#   share_with_all_users = TRUE
# )
# 
# message("Upload complete.")
