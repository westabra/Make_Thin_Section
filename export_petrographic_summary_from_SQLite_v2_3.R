# ============================================================
# export_petrographic_summary_from_SQLite_v2_3.R
# ------------------------------------------------------------
# Purpose:
#   Export a petrographic summary table from Make_Thin_Section
#   SQLite database.
#
# Output fields:
#   No; ID_Sample; Latitude; Longitude; Rock Name; Primary mineralogy;
#   Alteration; Veins; Weathering; Q; A; P; AI
#
# SQLite schema expected:
#   projects:
#     project_id, project_name, created_at, notes
#
#   samples:
#     project_id, sample_id, author, date, field_rock_name,
#     field_desc, lat, lon, rock_name, grain_size, texture, rock_desc
#
#   mineral_composition:
#     id, project_id, sample_id, row_order, mineral, vol_text,
#     morphology, genesis
#
# Mineral abbreviations:
#   White et al. 2010, "Abbreviations for names of rock-forming minerals",
#   American Mineralogist, v.95, p.185-187.
#
# Author:
#   Generated with ChatGPT for Sergey Abramov workflow
# Version note:
#   v2.1 wraps database work in main() to avoid premature connection closing
#   when the script is run/source'd from RStudio.
#   v2.2 replaces summarise(cur_data()) with group_modify() for mineral lists
#   to avoid dplyr/vctrs size mismatch errors.
#   v2.3 adds Latitude/Longitude to summary and displays Trace/<1 volumes as <1%.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "DBI",
  "RSQLite",
  "dplyr",
  "stringr",
  "readr",
  "openxlsx",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages required for the petrographic summary: ",
    paste(missing_packages, collapse = ", "),
    ". Run source(\"install_main_dependencies.R\") first.",
    call. = FALSE
  )
}

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(openxlsx)


# ============================================================
# 1. User settings
# ============================================================

# SQLite database path used only when this file is run directly with Rscript.
db_path <- Sys.getenv(
  "PETRO_DB_PATH",
  unset = "Make_Thin_Section_demo.sqlite"
)

# Mineral abbreviations CSV.
# Keep this file in the same folder as this R script,
# or use a full path.
abbr_path <- "White et. al. Abbreviations for names of rock-forming minerals.csv"

# Target project name used only when this file is run directly with Rscript.
target_project <- Sys.getenv("PETRO_SUMMARY_PROJECT", unset = "Mazarally")

# Direct Rscript runs save to the current working directory. The Shiny app
# supplies a browser-managed temporary download path instead.
output_dir <- getwd()

# If TRUE, volume values are printed in mineral lists:
#   Hbl (45%), Pl (35%), Chl (5%)
# If FALSE:
#   Hbl, Pl, Chl
show_volumes_in_lists <- TRUE

# Numeric value used for "Trace" / "tr" minerals.
trace_value <- 0.1

# Numeric value used for "<1" minerals.
less_than_one_value <- 0.5

# Numeric value used for ">1" minerals.
greater_than_one_value <- 1.0


# ============================================================
# 2. Helper functions
# ============================================================

stop_if_file_missing <- function(path, label = "file") {
  if (!file.exists(path)) {
    stop(label, " not found: ", path, call. = FALSE)
  }
}


normalize_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[_\\-]+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_squish()
}


normalize_mineral <- function(x) {
  normalize_text(x)
}


parse_vol_pct <- function(x,
                          trace_value = 0.1,
                          less_than_one_value = 0.5,
                          greater_than_one_value = 1.0) {
  # Converts vol_text into numeric values.
  #
  # Supported examples:
  #   "10"       -> 10
  #   "10.5"     -> 10.5
  #   "10,5"     -> 10.5
  #   "Trace"    -> 0.1
  #   "tr"       -> 0.1
  #   "<1"       -> 0.5
  #   ">1"       -> 1.0
  #   "1-2"      -> 1.5
  #   "1–2"      -> 1.5
  #   "1 to 2"   -> 1.5
  #
  # Empty or unparseable values become NA_real_.

  x0 <- as.character(x)
  x0 <- stringr::str_squish(x0)
  x1 <- stringr::str_to_lower(x0)
  x1 <- stringr::str_replace_all(x1, ",", ".")
  x1 <- stringr::str_replace_all(x1, "–|—", "-")

  out <- rep(NA_real_, length(x1))

  empty_idx <- is.na(x1) | x1 == ""
  trace_idx <- stringr::str_detect(x1, "^(trace|tr\\.?|t)$")
  lt1_idx   <- stringr::str_detect(x1, "^<\\s*1(\\.0+)?$")
  gt1_idx   <- stringr::str_detect(x1, "^>\\s*1(\\.0+)?$")

  range_idx <- stringr::str_detect(x1, "^[0-9.]+\\s*(-|to)\\s*[0-9.]+$")

  out[trace_idx] <- trace_value
  out[lt1_idx]   <- less_than_one_value
  out[gt1_idx]   <- greater_than_one_value

  if (any(range_idx, na.rm = TRUE)) {
    ranges <- x1[range_idx]
    parts <- stringr::str_split(ranges, "\\s*(?:-|to)\\s*", simplify = TRUE)
    left <- suppressWarnings(as.numeric(parts[, 1]))
    right <- suppressWarnings(as.numeric(parts[, 2]))
    out[range_idx] <- (left + right) / 2
  }

  simple_numeric_idx <- !empty_idx & is.na(out) &
    stringr::str_detect(x1, "^[0-9.]+$")

  out[simple_numeric_idx] <- suppressWarnings(as.numeric(x1[simple_numeric_idx]))

  out
}


read_abbreviations <- function(abbr_path) {
  # Reads White et al. abbreviation table.
  #
  # Expected columns:
  #   Mineral Name
  #   Symbol
  #
  # The function is tolerant to minor variants in column names.

  stop_if_file_missing(abbr_path, "Mineral abbreviation CSV")

  abbr_raw <- readr::read_csv(abbr_path, show_col_types = FALSE)

  original_names <- names(abbr_raw)
  clean_names <- normalize_text(original_names)

  mineral_col <- original_names[clean_names %in% c(
    "mineral name",
    "mineral",
    "name"
  )][1]

  symbol_col <- original_names[clean_names %in% c(
    "symbol",
    "abbreviation",
    "abbr"
  )][1]

  if (is.na(mineral_col) || is.na(symbol_col)) {
    stop(
      "Cannot identify columns in abbreviation CSV.\n",
      "Expected columns similar to 'Mineral Name' and 'Symbol'.\n",
      "Actual columns: ", paste(original_names, collapse = ", "),
      call. = FALSE
    )
  }

  abbr_raw %>%
    transmute(
      mineral_name = .data[[mineral_col]],
      symbol = .data[[symbol_col]],
      mineral_key = normalize_mineral(.data[[mineral_col]])
    ) %>%
    filter(!is.na(mineral_key), mineral_key != "") %>%
    distinct(mineral_key, .keep_all = TRUE)
}


make_abbr <- function(mineral_name, abbr_table) {
  key <- normalize_mineral(mineral_name)
  out <- abbr_table$symbol[match(key, abbr_table$mineral_key)]

  # If no abbreviation is found, keep the mineral name in Title Case.
  ifelse(
    is.na(out) | out == "",
    stringr::str_to_title(as.character(mineral_name)),
    out
  )
}


format_volume_label <- function(vol_text, vol_num) {
  # Creates user-facing volume labels for mineral lists.
  #
  # Numeric values are still used internally for sorting and for Q-A-P-AI sums.
  # In the exported mineral lists, Trace / tr / <1 are shown as <1%,
  # not as 0.1% or 0.5%.

  x <- as.character(vol_text)
  x <- stringr::str_squish(x)
  xl <- stringr::str_to_lower(x)
  xl <- stringr::str_replace_all(xl, ",", ".")
  xl <- stringr::str_replace_all(xl, "–|—", "-")

  dplyr::case_when(
    is.na(xl) | xl == "" ~ ifelse(is.na(vol_num), NA_character_, as.character(vol_num)),
    stringr::str_detect(xl, "^(trace|tr\\.?|t)$") ~ "<1",
    stringr::str_detect(xl, "^<\\s*1(\\.0+)?$") ~ "<1",
    TRUE ~ x
  )
}


collapse_by_volume <- function(df, show_volumes = TRUE) {
  if (nrow(df) == 0) return("")

  df2 <- df %>%
    arrange(
      desc(ifelse(is.na(vol_num), -Inf, vol_num)),
      row_order,
      mineral_symbol
    )

  if (show_volumes) {
    df2 <- df2 %>%
      mutate(
        vol_label = format_volume_label(vol_pct, vol_num),
        item = ifelse(
          is.na(vol_label) | !nzchar(trimws(vol_label)),
          mineral_symbol,
          paste0(mineral_symbol, " (", vol_label, "%)")
        )
      )
  } else {
    df2 <- df2 %>%
      mutate(item = mineral_symbol)
  }

  paste(df2$item, collapse = ", ")
}


classify_genesis <- function(genesis_key) {
  # Edit keywords here if your Genesis vocabulary changes.
  #
  # Typical app values may include:
  #   Primary
  #   Secondary
  #   Metamorphic
  #   Alteration
  #   Vein
  #   Gangue
  #   Weathering

  dplyr::case_when(
    is.na(genesis_key) | genesis_key == "" ~ "Unclassified",

    stringr::str_detect(
      genesis_key,
      "primary|magmatic|igneous|relict|phenocryst|matrix primary"
    ) ~ "Primary",

    stringr::str_detect(
      genesis_key,
      "vein|veins|gangue|fracture|fissure|amygdule|cavity"
    ) ~ "Veins",

    stringr::str_detect(
      genesis_key,
      "weathering|weathered|supergene|oxidation|oxide|limonite|goethite"
    ) ~ "Weathering",

    stringr::str_detect(
      genesis_key,
      "alteration|secondary|metamorphic|retrograde|hydrothermal|replacement|saussuritization|chloritization|sericitization|epidotization"
    ) ~ "Alteration",

    TRUE ~ "Unclassified"
  )
}



safe_report_name <- function(project_name) {
  out <- gsub("[\\\\/:*?\"<>|]", "_", trimws(project_name))
  out <- gsub("[[:space:]]+", "_", out)
  out <- gsub("_+", "_", out)
  out <- sub("^[._]+", "", out)
  out <- sub("[._]+$", "", out)
  if (!nzchar(out)) out <- "project"
  substr(out, 1, 120)
}

mapping_unit_export_label <- function(unit_code, unit_name, shortname, unit_description, class_id) {
  mapply(
    function(code, name, short, description, class) {
      values <- lapply(
        list(code, name, short, description, class),
        function(x) if (is.na(x)) "" else trimws(as.character(x))
      )
      code <- values[[1]]
      name <- values[[2]]
      short <- values[[3]]
      description <- values[[4]]
      class <- values[[5]]
      lithology <- if (nzchar(short)) short else name
      class_text <- if (nzchar(description)) description else class

      if (nzchar(lithology) && nzchar(class_text)) {
        paste0(lithology, " - ", class_text)
      } else if (nzchar(code) && nzchar(lithology)) {
        paste0(code, " - ", lithology)
      } else if (nzchar(lithology)) {
        lithology
      } else {
        code
      }
    },
    unit_code, unit_name, shortname, unit_description, class_id,
    USE.NAMES = FALSE
  )
}

export_petrographic_summary <- function(
    db_path,
    target_project,
    output_dir = getwd(),
    abbr_path = "White et. al. Abbreviations for names of rock-forming minerals.csv",
    output_file = NULL,
    progress_callback = NULL) {

report_progress <- function(value, detail) {
  if (is.function(progress_callback)) {
    try(progress_callback(value, detail), silent = TRUE)
  }
  invisible(NULL)
}

report_progress(0.03, "Checking report files")

# ============================================================
# 3. Check input files
# ============================================================

stop_if_file_missing(db_path, "SQLite database")
stop_if_file_missing(abbr_path, "Mineral abbreviation CSV")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (is.null(output_file) || !nzchar(trimws(output_file))) {
  out_xlsx <- paste0(
    safe_report_name(target_project),
    "_petrographic_summary_v2_3.xlsx"
  )
  out_xlsx_path <- file.path(output_dir, out_xlsx)
} else {
  out_xlsx_path <- output_file
}

message("SQLite database: ", db_path)
message("Abbreviations CSV: ", abbr_path)
message("Target project: ", target_project)
message("Output folder: ", output_dir)


# ============================================================
# 4. Read abbreviation table
# ============================================================

report_progress(0.10, "Loading mineral abbreviations")
abbreviation_source_table <- readr::read_csv(abbr_path, show_col_types = FALSE)
abbr <- read_abbreviations(abbr_path)

message("Loaded mineral abbreviations: ", nrow(abbr))


# ============================================================
# 5. Connect to SQLite and check schema
# ============================================================

report_progress(0.18, "Opening SQLite database")
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path,
  flags = RSQLite::SQLITE_RO
)
on.exit({
  if (DBI::dbIsValid(con)) {
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
}, add = TRUE)

tables <- DBI::dbListTables(con)
report_progress(0.28, "Checking database structure")

message("Tables found in database:")
message(paste(tables, collapse = ", "))

required_tables <- c("projects", "samples", "mineral_composition")
missing_tables <- setdiff(required_tables, tables)

if (length(missing_tables) > 0) {
  stop(
    "Missing required table(s): ",
    paste(missing_tables, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 6. Load raw tables
# ============================================================

projects_raw <- DBI::dbReadTable(con, "projects")
samples_raw  <- DBI::dbReadTable(con, "samples")
minerals_raw <- DBI::dbReadTable(con, "mineral_composition")
report_progress(0.40, "Project and mineral tables loaded")

if (!"suggested_mapping_unit_id" %in% names(samples_raw)) {
  samples_raw$suggested_mapping_unit_id <- NA_integer_
}

mapping_units_lookup <- data.frame(
  unit_id = integer(0),
  suggested_lithological_unit = character(0),
  stringsAsFactors = FALSE
)

if ("mapping_units" %in% tables) {
  mapping_units_raw <- DBI::dbReadTable(con, "mapping_units")
  required_mapping_columns <- c(
    "unit_id", "unit_code", "unit_name", "shortname",
    "unit_description", "class_id"
  )
  for (column_name in setdiff(required_mapping_columns, names(mapping_units_raw))) {
    mapping_units_raw[[column_name]] <- if (column_name == "unit_id") NA_integer_ else ""
  }
  mapping_units_lookup <- data.frame(
    unit_id = suppressWarnings(as.integer(mapping_units_raw$unit_id)),
    suggested_lithological_unit = mapping_unit_export_label(
      mapping_units_raw$unit_code,
      mapping_units_raw$unit_name,
      mapping_units_raw$shortname,
      mapping_units_raw$unit_description,
      mapping_units_raw$class_id
    ),
    stringsAsFactors = FALSE
  )
}

required_projects_cols <- c("project_id", "project_name")
required_samples_cols  <- c("project_id", "sample_id", "rock_name", "lat", "lon")
required_mineral_cols  <- c(
  "project_id",
  "sample_id",
  "row_order",
  "mineral",
  "vol_text",
  "genesis"
)

check_columns <- function(df, required, table_name) {
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Table '", table_name, "' is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }
}

check_columns(projects_raw, required_projects_cols, "projects")
check_columns(samples_raw, required_samples_cols, "samples")
check_columns(minerals_raw, required_mineral_cols, "mineral_composition")


# ============================================================
# 7. Prepare samples and minerals
# ============================================================

samples <- samples_raw %>%
  left_join(projects_raw, by = "project_id") %>%
  left_join(
    mapping_units_lookup,
    by = c("suggested_mapping_unit_id" = "unit_id")
  ) %>%
  transmute(
    project_id   = project_id,
    project_name = project_name,
    sample_id    = sample_id,
    latitude     = lat,
    longitude    = lon,
    rock_name    = rock_name,
    suggested_lithological_unit = ifelse(
      is.na(suggested_lithological_unit),
      "",
      suggested_lithological_unit
    )
  )

available_projects <- sort(unique(samples$project_name))

if (!(target_project %in% available_projects)) {
  stop(
    "Target project was not found: ", target_project, "\n",
    "Available projects are:\n  ",
    paste(available_projects, collapse = "\n  "),
    call. = FALSE
  )
}

minerals <- minerals_raw %>%
  transmute(
    project_id = project_id,
    sample_id  = sample_id,
    row_order  = row_order,
    mineral    = mineral,
    vol_pct    = vol_text,
    morphology = morphology,
    genesis    = genesis
  )


# ============================================================
# 8. Clean and classify minerals
# ============================================================

minerals_clean <- minerals %>%
  mutate(
    mineral_key = normalize_mineral(mineral),
    genesis_key = normalize_text(genesis),
    vol_num = parse_vol_pct(
      vol_pct,
      trace_value = trace_value,
      less_than_one_value = less_than_one_value,
      greater_than_one_value = greater_than_one_value
    ),
    mineral_symbol = make_abbr(mineral, abbr),
    mineral_group = classify_genesis(genesis_key)
  )
report_progress(0.55, "Mineral records classified")


# ============================================================
# 9. Calculate Q, A, P, and AI
# ============================================================

# Q = quartz
# A = K-feldspar / alkali feldspar group
# P = plagioclase group
#
# AI here is defined as:
#   sum of vol.% for minerals classified as "Alteration"
# based on the Genesis field.

qap_ai <- minerals_clean %>%
  group_by(project_id, sample_id) %>%
  summarise(
    Q = sum(
      vol_num[mineral_key %in% c(
        "quartz"
      )],
      na.rm = TRUE
    ),

    A = sum(
      vol_num[mineral_key %in% c(
        "k feldspar",
        "k-feldspar",
        "potassium feldspar",
        "alkali feldspar",
        "orthoclase",
        "microcline",
        "sanidine",
        "anorthoclase"
      )],
      na.rm = TRUE
    ),

    P = sum(
      vol_num[mineral_key %in% c(
        "plagioclase",
        "albite",
        "anorthite",
        "oligoclase",
        "andesine",
        "labradorite",
        "bytownite"
      )],
      na.rm = TRUE
    ),

    AI = sum(
      vol_num[mineral_group == "Alteration"],
      na.rm = TRUE
    ),

    .groups = "drop"
  )


# ============================================================
# 10. Make mineral lists
# ============================================================

# Use group_modify() instead of summarise(cur_data()).
# This avoids a dplyr/vctrs error where columns created earlier inside
# summarise() can be mixed with the original group rows.
make_mineral_lists_one_sample <- function(df, keys) {
  tibble::tibble(
    `Primary mineralogy` = collapse_by_volume(
      df %>% filter(mineral_group == "Primary"),
      show_volumes = show_volumes_in_lists
    ),

    Alteration = collapse_by_volume(
      df %>% filter(mineral_group == "Alteration"),
      show_volumes = show_volumes_in_lists
    ),

    Veins = collapse_by_volume(
      df %>% filter(mineral_group == "Veins"),
      show_volumes = show_volumes_in_lists
    ),

    Weathering = collapse_by_volume(
      df %>% filter(mineral_group == "Weathering"),
      show_volumes = show_volumes_in_lists
    ),

    `Unclassified minerals` = collapse_by_volume(
      df %>% filter(mineral_group == "Unclassified"),
      show_volumes = show_volumes_in_lists
    )
  )
}

mineral_lists <- minerals_clean %>%
  group_by(project_id, sample_id) %>%
  group_modify(make_mineral_lists_one_sample) %>%
  ungroup()


# ============================================================
# 11. Build final summary table
# ============================================================

summary_table <- samples %>%
  filter(project_name == target_project) %>%
  left_join(mineral_lists, by = c("project_id", "sample_id")) %>%
  left_join(qap_ai, by = c("project_id", "sample_id")) %>%
  mutate(
    `Primary mineralogy` = ifelse(is.na(`Primary mineralogy`), "", `Primary mineralogy`),
    Alteration = ifelse(is.na(Alteration), "", Alteration),
    Veins = ifelse(is.na(Veins), "", Veins),
    Weathering = ifelse(is.na(Weathering), "", Weathering),
    `Unclassified minerals` = ifelse(is.na(`Unclassified minerals`), "", `Unclassified minerals`),
    `Primary mineralogy` = dplyr::case_when(
      nzchar(`Primary mineralogy`) & nzchar(`Unclassified minerals`) ~ paste0(
        `Primary mineralogy`, ", ", `Unclassified minerals`,
        " [Genesis unclassified; see unclassified_records]"
      ),
      !nzchar(`Primary mineralogy`) & nzchar(`Unclassified minerals`) ~ paste0(
        `Unclassified minerals`, " [Genesis unclassified; see unclassified_records]"
      ),
      TRUE ~ `Primary mineralogy`
    ),
    Q  = ifelse(is.na(Q), 0, Q),
    A  = ifelse(is.na(A), 0, A),
    P  = ifelse(is.na(P), 0, P),
    AI = ifelse(is.na(AI), 0, AI)
  ) %>%
  arrange(sample_id) %>%
  mutate(No = row_number()) %>%
  transmute(
    No,
    ID_Sample = sample_id,
    Latitude = latitude,
    Longitude = longitude,
    `Rock Name` = rock_name,
    `Suggested lithological unit` = suggested_lithological_unit,
    `Primary mineralogy`,
    Alteration,
    Veins,
    Weathering,
    Q,
    A,
    P,
    AI
  )
report_progress(0.72, "Summary table prepared")


# ============================================================
# 12. Unclassified records
# ============================================================

# Unclassified mineral records for target project
unclassified_records <- minerals_clean %>%
  left_join(projects_raw, by = "project_id") %>%
  filter(
    project_name == target_project,
    mineral_group == "Unclassified"
  ) %>%
  select(
    project_name,
    project_id,
    sample_id,
    row_order,
    mineral,
    vol_pct,
    genesis,
    mineral_symbol
  ) %>%
  arrange(sample_id, row_order)


# ============================================================
# 13. Export XLSX
# ============================================================

report_progress(0.82, "Building Excel workbook")
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeDataTable(wb, "summary", summary_table)

openxlsx::addWorksheet(wb, "unclassified_records")
openxlsx::writeDataTable(wb, "unclassified_records", unclassified_records)

report_progress(0.88, "Adding mineral abbreviations sheet")
openxlsx::addWorksheet(wb, "mineral_abbreviations")
openxlsx::writeDataTable(wb, "mineral_abbreviations", abbreviation_source_table)

# Basic formatting
for (sheet in names(wb)) {
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = 1:50, widths = "auto")
}

report_progress(0.92, "Writing XLSX file")
openxlsx::saveWorkbook(wb, out_xlsx_path, overwrite = TRUE)
report_progress(1.00, "Report is ready")


# ============================================================
# 14. Console report
# ============================================================

message("\nExport completed successfully.")
message("XLSX: ", out_xlsx_path)

message("\nSummary rows exported: ", nrow(summary_table))

message("\nProjects available in database:")
message(paste("  -", available_projects, collapse = "\n"))

if (nrow(unclassified_records) > 0) {
  message(
    "\nWarning: some mineral records have unclassified Genesis values.\n",
    "They were exported in column 'Unclassified minerals'.\n",
    "See XLSX sheet: unclassified_records"
  )
}

invisible(list(
  xlsx_path = normalizePath(out_xlsx_path, winslash = "/", mustWork = TRUE),
  project_name = target_project,
  summary_rows = nrow(summary_table),
  unclassified_rows = nrow(unclassified_records)
))

}

if (sys.nframe() == 0L) {
  export_petrographic_summary(
    db_path = db_path,
    target_project = target_project,
    output_dir = output_dir,
    abbr_path = abbr_path
  )
}
