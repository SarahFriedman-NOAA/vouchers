# ==============================================================================
# RACEBASE DATA EXTRACTION + LOCAL CACHING
# ==============================================================================


# ------------------------------------------------------------------------------
# Config
# ------------------------------------------------------------------------------

cache_dir   <- here::here("data", "oracle")
cache_days  <- 365   # refresh cache if older than this (set Inf to never refresh)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------------------------
# Helper: check if cache is fresh
# ------------------------------------------------------------------------------

is_fresh <- function(path, max_age_days = cache_days, override = override) {
  if (!file.exists(path) | override) return(FALSE)
  age <- as.numeric(Sys.time() - file.info(path)$mtime, units = "days")
  age <= max_age_days
}


# ------------------------------------------------------------------------------
# Helper: run SQL + clean
# ------------------------------------------------------------------------------

run_query <- function(query, channel) {
  RODBC::sqlQuery(channel, query) %>%
    tibble::as_tibble() %>%
    janitor::clean_names()
}


# ------------------------------------------------------------------------------
# Helper: cached query (read if fresh, otherwise download)
# ------------------------------------------------------------------------------

cached_query <- function(query, file, channel, override = override) {
  
  if (is_fresh(file, override = override)) {
    message("Reading cached: ", basename(file))
    return(readr::read_csv(file, show_col_types = FALSE))
  }
  
  message("Querying Oracle: ", basename(file))
  dat <- run_query(query, channel)
  
  readr::write_csv(dat, file)
  dat
}


# ==============================================================================
# 1. CORE TABLES (CACHED)
# ==============================================================================

if (!exists("channel")) stop("Oracle connection `channel` not found.")
if (!exists("year"))    stop("`year` must be defined.")

# Species tables
spec0 <- cached_query(
  "SELECT * FROM RACEBASE.SPECIES",
  file = file.path(cache_dir, "racebase-species.csv"),
  channel,
  override
)

tax0 <- cached_query(
  "SELECT * FROM RACEBASE.SPECIES_CLASSIFICATION",
  file = file.path(cache_dir, "racebase-species_classification.csv"),
  channel,
  override
)


# ------------------------------------------------------------------------------
# Build species lookup
# ------------------------------------------------------------------------------

species0 <- spec0 %>%
  dplyr::left_join(tax0, by = "species_code") %>%
  janitor::clean_names() %>%
  dplyr::select(
    species_code,
    species = species_name,
    family  = family_taxon
  )


# ------------------------------------------------------------------------------
# Taxonomy tables (cached)
# ------------------------------------------------------------------------------

classification <- cached_query(
  "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
  file = file.path(cache_dir, "gap_products-taxonomic_classification.csv"),
  channel,
  override
) %>%
  dplyr::rename_with(~ stringr::str_remove(.x, "_taxon")) %>%
  dplyr::filter(survey_species == 1) %>%
  dplyr::select(species_name, common_name, species_code, family)


changes <- cached_query(
  "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CHANGES",
  file = file.path(cache_dir, "gap_products-taxonomic_changes.csv"),
  channel,
  override
) %>%
  dplyr::left_join(classification, by = c("new_species_code" = "species_code"))

taxonomy <- dplyr::bind_rows(classification, 
                             changes %>% select(species_name = old_species_name,
                                                species_code = new_species_code)
)



# ==============================================================================
# 2. CRUISE + CATCH DATA
# ==============================================================================

survey_def_ids <- c(52, 47, 98, 78, 143)

year_filter <- if (length(year) > 1) {
  paste0("year IN (", paste(year, collapse = ", "), ")")
} else {
  paste0("year = ", year)
}

query_cruise <- paste0(
  "SELECT DISTINCT year, vessel_id, cruise ",
  "FROM race_data.v_cruises ",
  "WHERE survey_definition_id IN (", paste(survey_def_ids, collapse = ", "), ") ",
  "AND ", year_filter
)

cruise0 <- run_query(query_cruise, channel)

if (nrow(cruise0) == 0) {
  stop("No cruises found for given year(s).")
}


# ------------------------------------------------------------------------------
# HAUL table (cached; needed for bottom_depth)
# ------------------------------------------------------------------------------

all_haul <- cached_query(
  "SELECT * FROM RACEBASE.HAUL",
  file = file.path(cache_dir, "racebase-haul.csv"),
  channel,
  override
) %>%
  janitor::clean_names() %>%
  dplyr::mutate(
    collecting_gear = dplyr::case_when(
      gear == 172 ~ "Poly-noreastern",
      gear == 44  ~ "Eastern trawl"
    )
  )


# ------------------------------------------------------------------------------
# Catch data (filtered) + bottom_depth join
# ------------------------------------------------------------------------------

cruise_ids <- paste(cruise0$cruise, collapse = ", ")

query_catch <- paste0(
  "SELECT * FROM racebase.catch ",
  "WHERE cruise IN (", cruise_ids, ") ",
  "AND voucher IS NOT NULL"
)

catch_year <- run_query(query_catch, channel) 

if(nrow(catch_year) > 0 ) {
  catch_year <- catch_year %>%
    dplyr::left_join(
    all_haul %>% dplyr::select(cruisejoin, hauljoin, bottom_depth),
    by = c("cruisejoin", "hauljoin")
  )
} else rm(catch_year)


# ------------------------------------------------------------------------------
# Full catch table (cached)
# ------------------------------------------------------------------------------

all_catch <- cached_query(
  "SELECT * FROM RACEBASE.CATCH",
  file = file.path(cache_dir, "racebase-catch.csv"),
  channel,
  override
)


# ------------------------------------------------------------------------------
# Existing vouchers (cached)
# ------------------------------------------------------------------------------

existing_vouch <- cached_query(
  "SELECT * FROM RACE_DATA.VOUCHERS",
  file = file.path(cache_dir, "race-data-vouchers.csv"),
  channel,
  override
) %>%
  dplyr::arrange(dplyr::desc(year))
