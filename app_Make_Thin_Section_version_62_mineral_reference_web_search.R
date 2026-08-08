# app.R
# Petrographic Shiny panel (FULL) — officer 0.7.2 compatible

# This version:
# - LAN-supported launch mode with optional read-only protection via SHINY_READ_ONLY=TRUE
# - Mineral autocomplete via selectizeInput
# - Mineral composition table ALWAYS stores FULL mineral names (via minerals.csv mapping):
#   * if user types abbreviation (Chl, Ab) -> stored as full mineral_name
#   * if user types non-mineral term (myrmekite, granophyre, etc.) -> stored as typed
# - Reference library suggestions from biblio_for_mineral_descriptions.csv
# - Search supports full name + abbreviation (synonyms)
# - V56: Multi-texture components editor (Texture / Importance / Domain / Interpretation)
#   stored in sample_textures; samples.texture remains the Primary texture for backward compatibility.
# - V56 GIS update: GIS/map exports now use a combined Texture field:
#   Primary texture + recognized texture component names separated by semicolons.
# - V58: Project map mode selector adds All projects overview (super-heavy)
# - V58.1: Exact project_name lookup; imported names with underscores/mixed case now load/delete correctly
#   with project-colored Leaflet markers and project-colored static map exports.
# - V59.6.1: Manual rock-type / mapping-unit classification:
#   mapping_units reference table + Suggested lithological unit / Confidence / Comment per sample.
# - V59.6.1: DOCX export order fix: texture history table now follows 4b before 4c.
# - V62: independent Mineral Reference lookup, online literature query builder,
#   and an editable curated web-resource hub.
#
# Packages:
# install.packages(c("shiny","DT","officer","flextable","magick","base64enc","sf","leaflet"))
library(shiny)
library(DT)
library(tools)
library(officer)   # 0.7.2
library(flextable)
# install.packages("sf")
library(sf)
library(leaflet)
# ---- SQLite (fixed path) ----
# install.packages(c("DBI","RSQLite"))
library(DBI)
library(RSQLite)
options(shiny.launch.browser = TRUE)
options(shiny.maxRequestSize = 50 * 1024^2)  # allow large uploads; images are optimized after upload
#DB_PATH <- "Y:/PL AND DATABASE/DATABASE/PETROGRAHY DATABASE/petro_db/petro_v3.sqlite"
DB_PATH <- Sys.getenv("PETRO_DB_PATH", unset = "C:/GCDkit.data/petro_db/petro_v3.sqlite")
#DB_PATH <- "C:/GCDkit.data/petro_db/exports/Takutu-2025_export.sqlite"


DB_IMAGES_DIR <- file.path(dirname(DB_PATH), "images")
DB_EXPORTS_DIR <- file.path(dirname(DB_PATH), "exports")

# ---- LAN / read-only mode ----
# Normal local/LAN edit mode: Sys.setenv(SHINY_READ_ONLY = "FALSE")
# LAN read-only mode:        Sys.setenv(SHINY_READ_ONLY = "TRUE")
# The run_LAN.R and run_LAN_readonly.R launcher files set this environment variable.
READ_ONLY_MODE <- identical(Sys.getenv("SHINY_READ_ONLY"), "TRUE")

# ---- Image Fragmentator launcher (standalone only) ----
# This launches Image_Fragmentator as a separate Shiny app in a separate R session.
# No bridge files, no selected image transfer, no sample_id transfer, no automatic import.
FRAGMENTATOR_APP_PATH <- file.path(getwd(), "image_fragmentator_V6.R")
MINERAL_WEB_RESOURCES_PATH <- file.path(getwd(), "mineral_reference_web_resources.csv")

# Embedded implementation of export_petrographic_summary_from_SQLite_v2_3.R.
# The module is isolated so its helper names do not leak into the Shiny app.
PETROGRAPHIC_SUMMARY_EXPORTER <- local({
  delayedAssign("%>%", dplyr::`%>%`)
  delayedAssign("transmute", dplyr::transmute)
  delayedAssign("filter", dplyr::filter)
  delayedAssign("distinct", dplyr::distinct)
  delayedAssign("arrange", dplyr::arrange)
  delayedAssign("desc", dplyr::desc)
  delayedAssign("mutate", dplyr::mutate)
  delayedAssign("left_join", dplyr::left_join)
  delayedAssign("group_by", dplyr::group_by)
  delayedAssign("summarise", dplyr::summarise)
  delayedAssign("group_modify", dplyr::group_modify)
  delayedAssign("ungroup", dplyr::ungroup)
  delayedAssign("row_number", dplyr::row_number)
  delayedAssign("select", dplyr::select)

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

  # Explanatory notes below the final summary table.
  summary_note_row <- nrow(summary_table) + 3L
  summary_note_last_col <- max(1L, ncol(summary_table))
  unclassified_note <- paste0(
    "Note on unclassified_records: entries marked ",
    "'[Genesis unclassified; see unclassified_records]' are included in ",
    "Primary mineralogy because their Genesis value could not be assigned to ",
    "Primary, Alteration, Veins, or Weathering. Full record details are available ",
    "on the 'unclassified_records' worksheet."
  )
  abbreviations_note <- paste0(
    "Mineral abbreviations follow: Whitney, D. L., & Evans, B. W. (2010). ",
    "Abbreviations for names of rock-forming minerals. American Mineralogist, ",
    "95(1), 185-187."
  )
  summary_note_style <- openxlsx::createStyle(
    fontSize = 10,
    fontColour = "#404040",
    textDecoration = "italic",
    valign = "top",
    wrapText = TRUE
  )

  openxlsx::writeData(
    wb, "summary", unclassified_note,
    startRow = summary_note_row, startCol = 1, colNames = FALSE
  )
  openxlsx::writeData(
    wb, "summary", abbreviations_note,
    startRow = summary_note_row + 1L, startCol = 1, colNames = FALSE
  )
  openxlsx::mergeCells(
    wb, "summary", cols = seq_len(summary_note_last_col), rows = summary_note_row
  )
  openxlsx::mergeCells(
    wb, "summary", cols = seq_len(summary_note_last_col), rows = summary_note_row + 1L
  )
  openxlsx::addStyle(
    wb, "summary", summary_note_style,
    rows = summary_note_row:(summary_note_row + 1L),
    cols = 1,
    gridExpand = FALSE,
    stack = TRUE
  )
  openxlsx::setRowHeights(
    wb, "summary",
    rows = summary_note_row:(summary_note_row + 1L),
    heights = c(36, 30)
  )
  
  openxlsx::addWorksheet(wb, "unclassified_records")
  openxlsx::writeDataTable(wb, "unclassified_records", unclassified_records)
  
  report_progress(0.88, "Adding mineral abbreviations sheet")
  openxlsx::addWorksheet(wb, "mineral_abbreviations")
  openxlsx::writeDataTable(wb, "mineral_abbreviations", abbreviation_source_table)
  
  # Basic formatting
  for (sheet in names(wb)) {
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    if (identical(sheet, "summary")) {
      # Do not let the long merged notes below the table determine column A.
      # Widths must be calculated only from the summary table cells.
      openxlsx::setColWidths(
        wb, sheet,
        cols = seq_len(ncol(summary_table)),
        widths = "auto",
        ignoreMergedCells = TRUE
      )
      # Keep the row-number column compact even if Excel/openxlsx versions
      # differ in how they handle merged cells during automatic sizing.
      openxlsx::setColWidths(wb, sheet, cols = 1, widths = 6)
    } else {
      openxlsx::setColWidths(wb, sheet, cols = 1:50, widths = "auto")
    }
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

  export_petrographic_summary
})

ensure_db_dirs <- function() {
  # In read-only LAN mode we do not create folders. The application should only
  # use the existing SQLite database and existing image/export directories.
  if (isTRUE(READ_ONLY_MODE)) return(invisible(TRUE))
  db_dir <- dirname(DB_PATH)
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(DB_IMAGES_DIR)) dir.create(DB_IMAGES_DIR, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(DB_EXPORTS_DIR)) dir.create(DB_EXPORTS_DIR, recursive = TRUE, showWarnings = FALSE)
}
db_connect <- function(read_only = READ_ONLY_MODE) {
  if (isTRUE(read_only)) {
    # Physical SQLite read-only connection. This prevents INSERT/UPDATE/DELETE
    # even if a write handler is accidentally triggered.
    dbConnect(SQLite(), dbname = DB_PATH, flags = RSQLite::SQLITE_RO)
  } else {
    ensure_db_dirs()
    dbConnect(SQLite(), DB_PATH)
  }
}
db_init <- function(con) {
  # Schema creation and ALTER TABLE are write operations. In read-only LAN mode
  # we assume the production DB has already been initialized.
  if (isTRUE(READ_ONLY_MODE)) return(invisible(TRUE))
  # settings
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key TEXT PRIMARY KEY,
      value TEXT
    );
  ")
  # projects
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      project_id   INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name TEXT NOT NULL UNIQUE,
      created_at   TEXT DEFAULT (date('now')),
      notes        TEXT
    );
  ")
  # samples (composite PK)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS samples (
      project_id  INTEGER NOT NULL,
      sample_id   TEXT NOT NULL,
      author      TEXT,
      date        TEXT,
      field_rock_name  TEXT,
      field_desc  TEXT,
      lat         TEXT,
      lon         TEXT,
      rock_name   TEXT,
      grain_size  TEXT,
      texture     TEXT,
      rock_desc   TEXT,
      PRIMARY KEY (project_id, sample_id),
      FOREIGN KEY (project_id) REFERENCES projects(project_id)
    );
  ")
  # ensure new columns exist (for upgraded DBs)
  cols <- tryCatch(dbGetQuery(con, "PRAGMA table_info(samples)"), error = function(e) NULL)
  if (!is.null(cols) && !"field_rock_name" %in% cols$name) {
    tryCatch(dbExecute(con, "ALTER TABLE samples ADD COLUMN field_rock_name TEXT"), error = function(e) NULL)
  }

  # V59: per-sample manual rock-type / mapping-unit interpretation.
  # suggested_mapping_unit_id points to mapping_units.unit_id when available.
  if (!is.null(cols) && !"suggested_mapping_unit_id" %in% cols$name) {
    tryCatch(dbExecute(con, "ALTER TABLE samples ADD COLUMN suggested_mapping_unit_id INTEGER"), error = function(e) NULL)
  }
  if (!is.null(cols) && !"mapping_unit_confidence" %in% cols$name) {
    tryCatch(dbExecute(con, "ALTER TABLE samples ADD COLUMN mapping_unit_confidence TEXT"), error = function(e) NULL)
  }
  if (!is.null(cols) && !"mapping_unit_comment" %in% cols$name) {
    tryCatch(dbExecute(con, "ALTER TABLE samples ADD COLUMN mapping_unit_comment TEXT"), error = function(e) NULL)
  }

  # V59: mapping / rock-type classifier reference table.
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS mapping_units (
      unit_id INTEGER PRIMARY KEY AUTOINCREMENT,
      classifier_name TEXT DEFAULT 'Canadian Geological Survey rock-type classifier',
      source_id TEXT,
      class_id TEXT,
      unit_code TEXT,
      unit_name TEXT NOT NULL,
      shortname TEXT,
      unit_description TEXT,
      source_reference TEXT,
      notes TEXT,
      is_active INTEGER DEFAULT 1,
      UNIQUE(classifier_name, unit_name)
    );
  ")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_mapping_units_classifier ON mapping_units(classifier_name, unit_name);")

  # minerals
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS mineral_composition (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      sample_id  TEXT NOT NULL,
      row_order  INTEGER,
      mineral    TEXT,
      vol_text   TEXT,
      morphology TEXT,
      genesis    TEXT,
      FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
    );
  ")
  # images
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS images (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      sample_id  TEXT NOT NULL,
      img_order  INTEGER,
      file_path  TEXT,
      caption    TEXT,
      FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
    );
  ")

  # V56: multi-texture components. The old samples.texture column is preserved
  # and used as the Primary texture for backward compatibility.
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sample_textures (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      sample_id  TEXT NOT NULL,
      row_order  INTEGER,
      texture    TEXT,
      importance TEXT,
      domain     TEXT,
      interpretation TEXT,
      FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
    );
  ")
}
settings_get <- function(con, key, default = "") {
  res <- tryCatch(dbGetQuery(con, "SELECT value FROM settings WHERE key = :k", params = list(k = key)), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0) return(default)
  as.character(if (length(res$value) >= 1) res$value[1] else default)
}
settings_set <- function(con, key, value) {
  if (isTRUE(READ_ONLY_MODE)) stop("Read-only LAN mode: settings cannot be saved.")
  dbExecute(con, "
    INSERT INTO settings(key, value) VALUES(:k, :v)
    ON CONFLICT(key) DO UPDATE SET value=excluded.value
  ", params = list(k = key, v = value %||% ""))
}
normalize_hex_color <- function(value, default = "#ffffff") {
  value <- trimws(as.character(value %||% default))
  if (grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", value)) value else default
}
# --- Project helpers ---
# V58.1 patch:
# Imported project names may contain underscores, mixed case, hyphens or numbers
# e.g. "West_Aranka_2". Older code converted names with to_title_case()
# before lookup, changing "West_Aranka_2" to "West_aranka_2".
# Result: the project was visible in the project list but samples could not be loaded,
# and Delete Project could report "Project not found in DB."
#
# Rule:
#   1) Try exact project_name first.
#   2) Use title-case only as a fallback for older manually typed names.
#   3) Never alter a project_name selected from DB.
ensure_project_id <- function(con, project_name) {
  if (isTRUE(READ_ONLY_MODE)) stop("Read-only LAN mode: project creation is disabled.")
  pn_raw <- trimws(project_name %||% "")
  if (!nzchar(pn_raw)) pn_raw <- "Default"

  # Exact match first.
  res <- tryCatch(
    dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = :pn",
               params = list(pn = pn_raw)),
    error = function(e) NULL
  )
  if (!is.null(res) && nrow(res) >= 1) return(as.integer(res$project_id[1]))

  # Backward-compatible title-case fallback.
  pn_title <- to_title_case(pn_raw)
  if (!identical(pn_title, pn_raw)) {
    res2 <- tryCatch(
      dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = :pn",
                 params = list(pn = pn_title)),
      error = function(e) NULL
    )
    if (!is.null(res2) && nrow(res2) >= 1) return(as.integer(res2$project_id[1]))
  }

  # Case-insensitive fallback.
  res3 <- tryCatch(
    dbGetQuery(con, "SELECT project_id FROM projects WHERE lower(project_name) = lower(:pn)",
               params = list(pn = pn_raw)),
    error = function(e) NULL
  )
  if (!is.null(res3) && nrow(res3) >= 1) return(as.integer(res3$project_id[1]))

  # Create with exact name.
  dbExecute(con, "INSERT INTO projects(project_name) VALUES(:pn)", params = list(pn = pn_raw))
  res4 <- dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = :pn", params = list(pn = pn_raw))
  as.integer(res4$project_id[1])
}

list_projects <- function(con) {
  df <- tryCatch(dbGetQuery(con, "SELECT project_name FROM projects ORDER BY project_name"),
                 error = function(e) data.frame(project_name = character(0)))
  as.character(df$project_name)
}

get_project_id <- function(con, project_name) {
  pn_raw <- trimws(project_name %||% "")
  if (!nzchar(pn_raw)) return(NA_integer_)

  # Exact match first.
  df <- tryCatch(
    dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = :pn",
               params = list(pn = pn_raw)),
    error = function(e) data.frame(project_id = integer(0))
  )
  if (nrow(df) >= 1) return(as.integer(df$project_id[1]))

  # Backward-compatible title-case fallback.
  pn_title <- to_title_case(pn_raw)
  if (!identical(pn_title, pn_raw)) {
    df2 <- tryCatch(
      dbGetQuery(con, "SELECT project_id FROM projects WHERE project_name = :pn",
                 params = list(pn = pn_title)),
      error = function(e) data.frame(project_id = integer(0))
    )
    if (nrow(df2) >= 1) return(as.integer(df2$project_id[1]))
  }

  # Case-insensitive fallback.
  df3 <- tryCatch(
    dbGetQuery(con, "SELECT project_id FROM projects WHERE lower(project_name) = lower(:pn)",
               params = list(pn = pn_raw)),
    error = function(e) data.frame(project_id = integer(0))
  )
  if (nrow(df3) >= 1) return(as.integer(df3$project_id[1]))

  NA_integer_
}

list_samples_in_project <- function(con, project_name) {
  pid <- get_project_id(con, project_name)
  if (is.na(pid)) return(character(0))
  df <- tryCatch(dbGetQuery(con, "SELECT sample_id FROM samples WHERE project_id = :pid ORDER BY sample_id",
                            params = list(pid = pid)), error = function(e) data.frame(sample_id = character(0)))
  as.character(df$sample_id)
}

# Genesis suggestions for Mineral composition.
# The database structure is NOT changed: suggestions are loaded from the existing
# mineral_composition.genesis column across all projects.
load_genesis_choices <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)

  df <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT DISTINCT TRIM(genesis) AS genesis
      FROM mineral_composition
      WHERE genesis IS NOT NULL
        AND TRIM(genesis) <> ''
      ORDER BY genesis
    "),
    error = function(e) data.frame(genesis = character(0))
  )

  db_values <- as.character(df$genesis)
  db_values <- db_values[!is.na(db_values) & nzchar(trimws(db_values))]

  base_values <- c(
    "Primary",
    "Alteration",
    "Secondary",
    "Metamorphic",
    "Hydrothermal",
    "Retrograde",
    "Vein",
    "Gangue",
    "Weathering"
  )

  unique(c(base_values, db_values))
}

# Morphology suggestions for Mineral composition.
# The database structure is NOT changed: suggestions are loaded from the existing
# mineral_composition.morphology column across all projects, filtered by full mineral name.
load_morphology_choices_for_mineral <- function(mineral_name) {
  mineral_name <- trimws(mineral_name %||% "")
  if (!nzchar(mineral_name)) return(character(0))

  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)

  df <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT DISTINCT TRIM(morphology) AS morphology
      FROM mineral_composition
      WHERE morphology IS NOT NULL
        AND TRIM(morphology) <> ''
        AND mineral IS NOT NULL
        AND TRIM(mineral) <> ''
        AND lower(TRIM(mineral)) = lower(TRIM(:mineral_name))
      ORDER BY morphology
    ", params = list(mineral_name = mineral_name)),
    error = function(e) data.frame(morphology = character(0))
  )

  out <- as.character(df$morphology)
  out[!is.na(out) & nzchar(trimws(out))]
}
to_title_case <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return("")
  parts <- strsplit(tolower(x), "\\s+")[[1]]
  parts <- vapply(parts, function(p) {
    if (!nzchar(p)) return("")
    paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p)))
  }, character(1))
  paste(parts[parts != ""], collapse = " ")
}
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

stop_if_read_only <- function(action = "this operation") {
  if (isTRUE(READ_ONLY_MODE)) {
    showNotification(
      paste0("Read-only LAN mode: ", action, " is disabled."),
      type = "warning",
      duration = 7
    )
    return(TRUE)
  }
  FALSE
}

read_only_banner_ui <- function() {
  if (!isTRUE(READ_ONLY_MODE)) return(NULL)
  tags$div(
    style = "margin: 6px 0 10px 0; padding: 8px 10px; border-radius: 6px; background: #fff3cd; border: 1px solid #ffeeba; color: #664d03;",
    tags$b("LAN read-only mode: "),
    "loading and viewing are allowed; saving, deleting, project creation and settings save are disabled."
  )
}

make_img_id <- function() {
  paste0("img_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sprintf("%06d", sample.int(999999, 1)))
}
# ---------- Helpers ----------
sanitize_text <- function(x) {
  x <- x %||% ""
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x
}
clip_text <- function(x, n = 350) {
  x <- as.character(x %||% "")
  x <- vapply(x, function(one) {
    one <- trimws(sanitize_text(one %||% ""))
    if (!nzchar(one)) return("")
    if (nchar(one) <= n) return(one)
    paste0(substr(one, 1, n), " …")
  }, character(1), USE.NAMES = FALSE)
  x
}
parse_coord_text <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return(NA_real_)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

safe_replace_all_text <- function(doc, old_value, new_value) {
  # Avoid noisy warnings when template doesn't contain placeholders
  out <- doc
  out <- suppressWarnings(try(officer::headers_replace_all_text(out, old_value = old_value, new_value = new_value), silent = TRUE))
  if (inherits(out, "try-error")) out <- doc
  out2 <- suppressWarnings(try(officer::body_replace_all_text(out, old_value = old_value, new_value = new_value), silent = TRUE))
  if (inherits(out2, "try-error")) out2 <- out
  out2
}

norm_mineral <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "", x)
  x
}
read_csv_guess <- function(path) {
  # Robust CSV reader for mixed encodings / broken final newline.
  # Strategy:
  # 1) read raw bytes
  # 2) try decode with several encodings
  # 3) parse via textConnection with fill=TRUE
  encs <- c("UTF-8", "UTF-8-BOM", "cp1251", "windows-1251", "latin1")
  raw <- tryCatch(readBin(path, what = "raw", n = file.info(path)$size), error = function(e) NULL)
  if (is.null(raw)) stop("Cannot read file")
  last <- NULL
  for (enc in encs) {
    txt <- tryCatch(rawToChar(raw), error = function(e) NULL)
    if (is.null(txt)) next
    # Try encoding conversion; if fails, skip
    txt2 <- tryCatch(iconv(txt, from = enc, to = "UTF-8", sub = "byte"), error = function(e) NULL)
    if (is.null(txt2)) next
    # Ensure final newline to avoid 'incomplete final line' warnings
    if (!grepl("\n$", txt2)) txt2 <- paste0(txt2, "\n")
    df <- tryCatch(
      {
        con <- textConnection(txt2)
        on.exit(close(con), add = TRUE)
        read.csv(con,
                 stringsAsFactors = FALSE,
                 check.names = FALSE,
                 fill = TRUE,
                 quote = "\"",
                 sep = ",")
      },
      warning = function(w) {
        # still return df if possible
        invokeRestart("muffleWarning")
      },
      error = function(e) { last <<- e; NULL }
    )
    if (!is.null(df)) return(df)
  }
  stop(last %||% "Cannot read CSV")
}

# ---- V59: Mapping-unit / rock-type classifier helpers ----
clean_colnames <- function(df) {
  if (is.null(df)) return(df)
  names(df) <- trimws(gsub("\ufeff", "", names(df)))
  df
}

pick_first_column <- function(df, candidates, fallback_index = NA_integer_) {
  if (is.null(df) || ncol(df) == 0) return(rep("", 0))
  nm <- names(df)
  nm_norm <- tolower(gsub("[^a-z0-9]+", "", nm))
  cand_norm <- tolower(gsub("[^a-z0-9]+", "", candidates))
  hit <- match(cand_norm, nm_norm, nomatch = 0)
  hit <- hit[hit > 0]
  if (length(hit) > 0) return(as.character(df[[hit[1]]]))
  if (!is.na(fallback_index) && fallback_index >= 1 && fallback_index <= ncol(df)) return(as.character(df[[fallback_index]]))
  rep("", nrow(df))
}

mapping_unit_label <- function(unit_code, unit_name, shortname = "", unit_description = "", class_id = "") {
  # For V59.5 classifier rows:
  # - shortname stores the lithology value from the selected lithology column;
  # - unit_description / class_id store LITHOLOGY_CLASSIFICATION;
  # - unit_name may include an internal key for uniqueness.
  unit_code <- trimws(as.character(unit_code %||% ""))
  unit_name <- trimws(as.character(unit_name %||% ""))
  shortname <- trimws(as.character(shortname %||% ""))
  unit_description <- trimws(as.character(unit_description %||% ""))
  class_id <- trimws(as.character(class_id %||% ""))

  lithology <- if (nzchar(shortname)) shortname else unit_name
  class_txt <- if (nzchar(unit_description)) unit_description else class_id

  if (nzchar(lithology) && nzchar(class_txt)) {
    paste0(lithology, " - ", class_txt)
  } else if (nzchar(unit_code) && nzchar(lithology)) {
    paste0(unit_code, " - ", lithology)
  } else if (nzchar(lithology)) {
    lithology
  } else {
    unit_code
  }
}

mapping_classifier_columns <- c(
  "ORIGINAL_LITHOLOGY_ENGLISH",
  "LITHOLOGY_Canada_Geological_Survey, 1:5M scale"
)

mapping_classification_column <- "LITHOLOGY_CLASSIFICATION"

lithology_classification_source <- paste0(
  "Lawley, C. J. M., Giddy, P., Katz, L., Chu, N., Francis, A., Carvajal, J., ",
  "Pinheiro, M., Tirona, K., Dettman, A., Chen, H. & Aucoin, F. (2024). ",
  "Canada geological map compilation. Geological Survey of Canada, Open File, 9169, 18. ",
  "Natural Resources Canada. https://doi.org/10.4095/pf995j5tgu"
)

resolve_csv_column_name <- function(df, candidates) {
  if (is.null(df) || ncol(df) == 0) return(NA_character_)
  nm <- names(df)
  normalize_key <- function(x) gsub("[^a-z0-9]+", "", tolower(x))
  nm_norm <- normalize_key(nm)
  cand_norm <- normalize_key(candidates)

  hit <- match(cand_norm, nm_norm, nomatch = 0)
  hit <- hit[hit > 0]
  if (length(hit) > 0) return(nm[hit[1]])

  for (cand in cand_norm) {
    starts <- which(startsWith(nm_norm, cand))
    if (length(starts) > 0) return(nm[starts[1]])
  }

  NA_character_
}


import_mapping_units_from_csv <- function(con, csv_path, classifier_name = NULL) {
  if (isTRUE(READ_ONLY_MODE)) return(invisible(0L))
  if (is.null(csv_path) || !file.exists(csv_path)) return(invisible(0L))

  df <- tryCatch(read_csv_guess(csv_path), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(invisible(0L))
  df <- clean_colnames(df)

  # V59.5 standard format: one external lookup_rocktype.csv with three fields:
  #   ORIGINAL_LITHOLOGY_ENGLISH
  #   LITHOLOGY_Canada_Geological_Survey
  #   LITHOLOGY_CLASSIFICATION
  #
  # The two lithology columns become two selectable classifiers in the sidebar.
  classifier_col_map <- stats::setNames(
    vapply(mapping_classifier_columns, function(x) resolve_csv_column_name(df, x), character(1)),
    mapping_classifier_columns
  )
  class_col <- resolve_csv_column_name(df, mapping_classification_column)

  if (all(!is.na(classifier_col_map)) && !is.na(class_col)) {
    imported <- 0L

    DBI::dbExecute(
      con,
      paste0(
        "UPDATE mapping_units SET is_active = 0 WHERE classifier_name IN (",
        paste(rep("?", length(mapping_classifier_columns)), collapse = ", "),
        ")"
      ),
      params = as.list(mapping_classifier_columns)
    )

    for (classifier_col in names(classifier_col_map)) {
      source_col <- classifier_col_map[[classifier_col]]
      lithology <- trimws(as.character(df[[source_col]] %||% ""))
      lith_class <- trimws(as.character(df[[class_col]] %||% ""))

      keep <- nzchar(lithology)
      if (!any(keep)) next

      source_row <- which(keep)
      lithology <- lithology[keep]
      lith_class <- lith_class[keep]

      # Internal key keeps both fields unique. This is important because the same
      # lithology may appear in more than one LITHOLOGY_CLASSIFICATION.
      unit_key <- ifelse(
        nzchar(lith_class),
        paste0(lithology, " || ", lith_class),
        lithology
      )

      rows <- data.frame(
        classifier_name = rep(classifier_col, length(lithology)),
        source_id = paste0("row_", sprintf("%06d", source_row)),
        class_id = lith_class,
        unit_code = rep("", length(lithology)),
        unit_name = unit_key,
        shortname = lithology,
        unit_description = lith_class,
        source_reference = rep(paste0("lookup_rocktype.csv:", source_col), length(lithology)),
        notes = rep("", length(lithology)),
        stringsAsFactors = FALSE
      )

      rows <- rows[!duplicated(paste(rows$classifier_name, tolower(rows$unit_name), sep = "::")), , drop = FALSE]
      if (nrow(rows) == 0) next

      for (i in seq_len(nrow(rows))) {
        DBI::dbExecute(con, "
          INSERT INTO mapping_units (
            classifier_name, source_id, class_id, unit_code, unit_name,
            shortname, unit_description, source_reference, notes, is_active
          ) VALUES (
            :classifier_name, :source_id, :class_id, :unit_code, :unit_name,
            :shortname, :unit_description, :source_reference, :notes, 1
          )
          ON CONFLICT(classifier_name, unit_name) DO UPDATE SET
            source_id = excluded.source_id,
            class_id = excluded.class_id,
            shortname = excluded.shortname,
            unit_description = excluded.unit_description,
            source_reference = excluded.source_reference,
            notes = excluded.notes,
            is_active = 1
        ", params = as.list(rows[i, , drop = FALSE]))
      }

      imported <- imported + nrow(rows)
    }

    return(invisible(imported))
  }

  # Legacy fallback for older one-column files. It remains here only for safety,
  # but the intended V59.5 file is lookup_rocktype.csv with the three standard fields above.
  unit_name <- trimws(pick_first_column(df, c("unit_name", "mapping_unit", "Rockname", "Rock Name", "Lithology", "Name"), fallback_index = 1))
  keep <- nzchar(unit_name)
  if (!any(keep)) return(invisible(0L))

  source_id <- trimws(pick_first_column(df, c("source_id", "Rocktype_ID", "RocktypeID", "Rock_Type_ID", "ID")))
  class_id <- trimws(pick_first_column(df, c("class_id", "Class_ID", "ClassID")))
  unit_code <- trimws(pick_first_column(df, c("unit_code", "Unit_Code", "Code", "Legend_Code", "Map_Code")))
  shortname <- trimws(pick_first_column(df, c("shortname", "Shortname", "Short_Name", "abbr", "abbreviation")))
  unit_description <- trimws(pick_first_column(df, c("unit_description", "Description", "Explanation", "definition", "Definition")))
  source_reference <- trimws(pick_first_column(df, c("source_reference", "Source", "Reference")))
  notes <- trimws(pick_first_column(df, c("notes", "Notes", "Comment", "Comments")))

  n <- length(unit_name)
  pad <- function(x) {
    x <- as.character(x %||% "")
    if (length(x) == n) return(x)
    rep("", n)
  }

  classifier_use <- trimws(classifier_name %||% "Rockname")
  if (!nzchar(classifier_use)) classifier_use <- "Rockname"

  rows <- data.frame(
    classifier_name = rep(classifier_use, n),
    source_id = pad(source_id),
    class_id = pad(class_id),
    unit_code = pad(unit_code),
    unit_name = unit_name,
    shortname = pad(shortname),
    unit_description = pad(unit_description),
    source_reference = pad(source_reference),
    notes = pad(notes),
    stringsAsFactors = FALSE
  )
  rows <- rows[keep, , drop = FALSE]
  rows <- rows[!duplicated(paste(rows$classifier_name, tolower(rows$unit_name), sep = "::")), , drop = FALSE]
  if (nrow(rows) == 0) return(invisible(0L))

  for (i in seq_len(nrow(rows))) {
    DBI::dbExecute(con, "
      INSERT INTO mapping_units (
        classifier_name, source_id, class_id, unit_code, unit_name,
        shortname, unit_description, source_reference, notes, is_active
      ) VALUES (
        :classifier_name, :source_id, :class_id, :unit_code, :unit_name,
        :shortname, :unit_description, :source_reference, :notes, 1
      )
      ON CONFLICT(classifier_name, unit_name) DO UPDATE SET
        source_id = CASE WHEN excluded.source_id <> '' THEN excluded.source_id ELSE mapping_units.source_id END,
        class_id = CASE WHEN excluded.class_id <> '' THEN excluded.class_id ELSE mapping_units.class_id END,
        unit_code = CASE WHEN excluded.unit_code <> '' THEN excluded.unit_code ELSE mapping_units.unit_code END,
        shortname = CASE WHEN excluded.shortname <> '' THEN excluded.shortname ELSE mapping_units.shortname END,
        unit_description = CASE WHEN excluded.unit_description <> '' THEN excluded.unit_description ELSE mapping_units.unit_description END,
        source_reference = CASE WHEN excluded.source_reference <> '' THEN excluded.source_reference ELSE mapping_units.source_reference END,
        notes = CASE WHEN excluded.notes <> '' THEN excluded.notes ELSE mapping_units.notes END,
        is_active = 1
    ", params = as.list(rows[i, , drop = FALSE]))
  }

  invisible(nrow(rows))
}

list_mapping_classifiers <- function(con) {
  out <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT DISTINCT classifier_name
      FROM mapping_units
      WHERE is_active = 1
        AND classifier_name IS NOT NULL
        AND TRIM(classifier_name) <> ''
      ORDER BY classifier_name
    "),
    error = function(e) data.frame(classifier_name = character(0))
  )

  vals <- as.character(out$classifier_name)
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]

  # V59.5: if the standard lookup_rocktype.csv classifiers exist, expose only them.
  standard_vals <- mapping_classifier_columns[mapping_classifier_columns %in% vals]
  if (length(standard_vals) > 0) return(standard_vals)

  vals
}


load_mapping_units <- function(con, classifier_name = NULL) {
  classifier_name <- trimws(classifier_name %||% "")
  if (!nzchar(classifier_name)) {
    df <- tryCatch(
      DBI::dbGetQuery(con, "
        SELECT unit_id, classifier_name, source_id, class_id, unit_code, unit_name,
               shortname, unit_description, source_reference, notes, is_active
        FROM mapping_units
        WHERE is_active = 1
        ORDER BY classifier_name, source_id, unit_description, shortname, unit_name
      "),
      error = function(e) data.frame()
    )
  } else {
    df <- tryCatch(
      DBI::dbGetQuery(con, "
        SELECT unit_id, classifier_name, source_id, class_id, unit_code, unit_name,
               shortname, unit_description, source_reference, notes, is_active
        FROM mapping_units
        WHERE is_active = 1
          AND classifier_name = :classifier_name
        ORDER BY source_id, unit_description, shortname, unit_name
      ", params = list(classifier_name = classifier_name)),
      error = function(e) data.frame()
    )
  }

  if (!is.null(df) && nrow(df) > 0) {
    df$display_label <- mapply(
      mapping_unit_label,
      df$unit_code,
      df$unit_name,
      df$shortname,
      df$unit_description,
      df$class_id,
      USE.NAMES = FALSE
    )
  }
  df
}

build_user_guide_ui <- function(path = file.path(getwd(), "README.md")) {
  if (!file.exists(path)) {
    return(tags$div(
      class = "alert alert-warning",
      "README.md was not found in the application folder."
    ))
  }

  guide_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  if (requireNamespace("markdown", quietly = TRUE)) {
    guide_html <- markdown::markdownToHTML(
      text = guide_text,
      fragment.only = TRUE,
      encoding = "UTF-8"
    )
    return(tags$div(class = "user-guide-content", HTML(guide_html)))
  }

  tags$div(
    class = "user-guide-content",
    tags$div(
      class = "alert alert-info",
      "The markdown package is not installed, so the guide is shown as plain text. Run install_main_dependencies.R for formatted display."
    ),
    tags$pre(guide_text)
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root { --panel-bg: #ffffff; }
      .desc-panel {
        background: var(--panel-bg);
        border-radius: 10px;
        padding: 10px;
      }
      .small-note { color: #666; font-size: 0.9em; }
      .user-guide-content {
        max-width: 980px;
        margin: 0 auto;
        padding: 8px 18px 30px 18px;
        line-height: 1.55;
      }
      .user-guide-content h1,
      .user-guide-content h2,
      .user-guide-content h3 { color: #244b63; }
      .user-guide-content img { max-width: 100%; height: auto; }
      .user-guide-content pre { white-space: pre-wrap; word-break: normal; }
      .field-label { font-weight: 600; margin-top: 10px; }
      .map-style-block {
        margin: 12px 0 12px 0;
        padding: 10px 12px;
        border: 1px solid #d7dee2;
        border-radius: 8px;
        background: #f8fafb;
      }
      .nav-tabs > li > a {
        margin-right: 4px;
        border: 1px solid #c8d6df;
        border-bottom-color: #aebfc9;
        border-radius: 6px 6px 0 0;
        background: #eef4f7;
        color: #244b63;
        font-weight: 700;
      }
      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        background: #dcebf2;
        border-color: #7fa7bd;
        color: #123f5b;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: #2e6da4;
        border-color: #245580;
        color: #ffffff;
      }
      .settings-secondary-btn {
        background: #e5f1f7;
        border: 1px solid #78a6bd;
        color: #174e6c;
        font-weight: 700;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.08);
      }
      .settings-secondary-btn:hover,
      .settings-secondary-btn:focus {
        background: #cfe5ef;
        border-color: #4d879f;
        color: #0e405a;
      }
      .settings-advanced-details {
        margin-bottom: 16px;
      }
      .settings-advanced-details > summary {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        padding: 12px 14px;
        border: 1px solid #5d92ae;
        border-radius: 7px;
        background: #e1eff6;
        color: #174e6c;
        font-weight: 800;
        cursor: pointer;
        user-select: none;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.10);
      }
      .settings-advanced-details > summary:hover,
      .settings-advanced-details > summary:focus,
      .settings-advanced-details[open] > summary {
        background: #c7e2ee;
        border-color: #2e6da4;
        color: #0f405c;
      }
      .settings-advanced-details > summary::after {
        content: 'Open +';
        margin-left: 12px;
        font-size: 12px;
        font-weight: 800;
        text-transform: uppercase;
      }
      .settings-advanced-details[open] > summary {
        border-radius: 7px 7px 0 0;
      }
      .settings-advanced-details[open] > summary::after {
        content: 'Close -';
      }
      .settings-advanced-details > .well {
        border-top: 0;
        border-radius: 0 0 7px 7px;
      }
      .project-map-settings {
        margin-top: 10px;
      }
      .project-map-settings > summary {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 7px 10px;
        border: 1px solid #b7ccd8;
        border-radius: 6px;
        background: #eef6fb;
        color: #1b5f8c;
        font-weight: 800;
        cursor: pointer;
        user-select: none;
      }
      .project-map-settings > summary:hover,
      .project-map-settings[open] > summary {
        background: #dfeff7;
        border-color: #2e6da4;
        color: #0f4e78;
      }
      .project-map-settings > summary::after {
        content: '+';
        color: #607d8b;
        font-size: 14px;
        font-weight: 700;
        text-transform: uppercase;
      }
      .project-map-settings[open] > summary::after {
        content: '-';
      }
      .map-style-header {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 8px;
        color: #263238;
        font-weight: 700;
      }
      .map-style-header .fa {
        color: #2e6da4;
        font-size: 16px;
      }
      .map-style-title {
        color: #1b5f8c;
        font-weight: 800;
      }
      .map-style-hint {
        margin-left: auto;
        color: #607d8b;
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
      }
      .map-style-block #static_map_mode {
        margin-bottom: 0;
      }
      .map-style-block #static_map_mode > label.control-label {
        display: none;
      }
      .map-style-block #static_map_mode .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
        gap: 8px;
      }
      .map-style-block #static_map_mode .radio {
        margin: 0;
      }
      .map-style-block #static_map_mode .radio label {
        width: 100%;
        min-height: 42px;
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 9px 10px;
        border: 1px solid #cfd8dc;
        border-radius: 6px;
        background: #ffffff;
        color: #263238;
        font-weight: 600;
        line-height: 1.25;
      }
      .map-style-block #static_map_mode .radio label:has(input:checked) {
        border-color: #2e6da4;
        background: #eef6fb;
        box-shadow: inset 0 0 0 1px #2e6da4;
      }
      :root { --thumb-zoom-scale: 1.9; --img-card-width: 280px; --img-thumb-height: 170px; }
      /* ========= TEXTAREA RESIZE ========= */
      .shiny-input-container textarea,
      .shiny-input-container textarea.form-control,
      textarea.form-control,
      #field_desc, #rock_desc,
      textarea[id^='cap_'] {
        resize: both !important;
        overflow: auto !important;
        width: 100%;
        max-width: 100% !important;
        min-height: 80px;
        box-sizing: border-box !important;
      }
      .shiny-input-container { overflow: visible !important; max-width: 100% !important; }
      /* ========= Gallery layout ========= */
      .img-grid { display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-start; }
      .img-card { width: var(--img-card-width); border: 1px solid #ddd; border-radius: 10px; padding: 10px; background: #fff; }
      .img-thumb-wrap {
        width: 100%;
        height: var(--img-thumb-height);
        background: #fafafa;
        border-radius: 8px;
        border: 1px solid #eee;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        position: relative;
      }
      .img-thumb {
        width: 100%;
        height: 100%;
        object-fit: contain;
        display: block;
        cursor: zoom-in;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        position: relative;
      }
      .img-thumb-wrap:hover { overflow: visible; }
      .img-card { position: relative; }
      .img-thumb:hover {
        transform: scale(var(--thumb-zoom-scale));
        z-index: 5;
        box-shadow: 0 10px 30px rgba(0,0,0,0.25);
      }
      .img-meta {
        font-size: 0.85em;
        color: #555;
        margin-top: 6px;
        line-height: 1.3;
      }
      .img-selected-wrap {
        width: 100%;
        max-width: 100%;
        height: 420px;
        background: #fafafa;
        border: 1px solid #ddd;
        border-radius: 10px;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .img-selected {
        width: 100%;
        height: 100%;
        object-fit: contain;
        display: block;
      }
      .warn { color: #a33; font-size: 0.9em; }
      /* ========= Paste area ========= */
      #paste_area {
        border: 2px dashed #bbb;
        border-radius: 10px;
        padding: 14px;
        background: #fcfcfc;
        min-height: 70px;
        outline: none;
      }
      #paste_area:focus {
        border-color: #4a90e2;
        box-shadow: 0 0 0 3px rgba(74,144,226,0.15);
      }
      .mineral-reference-intro {
        margin: 4px 0 14px 0;
        padding: 12px 14px;
        border-left: 4px solid #19778a;
        border-radius: 6px;
        background: #f1f8fa;
      }
      .mineral-resource-hub-btn {
        background: linear-gradient(135deg, #0b4f5c 0%, #16778a 100%);
        color: #ffffff !important;
        border: 1px solid #c8a85a;
        border-radius: 999px;
        padding: 10px 18px;
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 15px;
        font-weight: 700;
        letter-spacing: 0.2px;
        box-shadow: 0 3px 8px rgba(11,79,92,0.25);
      }
      .mineral-resource-hub-btn:hover,
      .mineral-resource-hub-btn:focus {
        background: linear-gradient(135deg, #083e48 0%, #126677 100%);
        color: #ffffff !important;
        border-color: #e0c36f;
        transform: translateY(-1px);
      }
      .web-query-preview {
        margin-top: 10px;
        padding: 10px 12px;
        border: 1px solid #cddde3;
        border-radius: 6px;
        background: #f8fbfc;
        font-family: Consolas, 'Courier New', monospace;
        overflow-wrap: anywhere;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setCssVar', function(msg){
        try{ document.documentElement.style.setProperty(msg.name, msg.value); } catch(e){}
      });
    ")),
    
    tags$script(HTML("
      (function(){
        function setPlaceholder(el){
          if(!el) return;
          el.innerHTML = '<b>Click here</b> and press <b>Ctrl+V</b> to paste an image from clipboard.';
        }
        function sendImageFromClipboard(e){
          var active = document.activeElement;
          if(!active || active.id !== 'paste_area') return;
          var items = (e.clipboardData || window.clipboardData).items;
          if(!items) return;
          for (var i = 0; i < items.length; i++) {
            var item = items[i];
            if (item.type && item.type.indexOf('image') === 0) {
              e.preventDefault();
              var blob = item.getAsFile();
              var reader = new FileReader();
              reader.onload = function(evt){
                var dataURL = evt.target.result;
                Shiny.setInputValue('clipboard_image', {
                  data: dataURL,
                  type: blob.type || 'image/png',
                  ts: Date.now()
                }, {priority: 'event'});
                setPlaceholder(active);
              };
              reader.readAsDataURL(blob);
              return;
            }
          }
        }
        document.addEventListener('paste', sendImageFromClipboard);
        document.addEventListener('DOMContentLoaded', function(){
          setPlaceholder(document.getElementById('paste_area'));
        });
      })();
    ")),
    tags$script(HTML("
      // Set browser tab title (document.title) from server messages
      Shiny.addCustomMessageHandler('set_doc_title', function(message) {
        try {
          var t = (typeof message === 'string') ? message : (message && message.title);
          if (t) { document.title = t; }
        } catch(e) {}
      });

      // Bring the new-sample workflow from Database tools to Sample metadata.
      Shiny.addCustomMessageHandler('focus_sample_id', function(message) {
        window.setTimeout(function() {
          try {
            var el = document.getElementById('sample_id');
            if (el) {
              el.scrollIntoView({behavior: 'smooth', block: 'center'});
              el.focus();
              el.select();
            }
          } catch(e) {}
        }, 150);
      });
    ")),

  ),

  uiOutput("dynamic_header"),
  
  sidebarLayout(
    sidebarPanel(
  div(class = "desc-panel",
      h4("Sample metadata"),
      textInput("sample_id", "1) Sample ID", placeholder = "e.g., GS-2026-001"),
      
      textInput("field_rock_name", "2a) Field Rock Name (field identification)", placeholder = "Field rock name / lithology (field)"),
      
      textAreaInput(
        "field_desc", "2b) Field description (field definition)",
        placeholder = "Field notes: outcrop, structures, weathering, relationships, etc.",
        rows = 5
      ),
      
      fluidRow(
        column(6, textInput("lat", "3) Latitude", placeholder = "-3.12345")),
        column(6, textInput("lon", "Longitude", placeholder = "58.12345"))
      ),

      actionButton("coord_calc_btn", "Coordinate calculator (PSAD56 ⇄ WGS84)", icon = icon("calculator")),
      tags$small("Opens a converter for PSAD56 / UTM (20N/21N) ↔ WGS84. Use \"Insert\" to fill Latitude/Longitude."),
      tags$hr(),
      
      textInput("rock_name", "4) Rock name (microscope identification)", placeholder = "e.g., amphibolite"),
      
      selectizeInput(
        "grain_size", "4a) Grain size. Items marked with an asterisk (*) are provided for reference only",
        choices = character(0),
        selected = NULL,
        options = list(create = TRUE, persist = TRUE, placeholder = "e.g., fine / medium / coarse ...")
      ),
      selectizeInput(
        "texture", "4b) Primary texture. Move your mouse over the name 'texture' to get a hint",
        choices = character(0),
        selected = NULL,
        options = list(create = TRUE, persist = TRUE, placeholder = "e.g., porphyritic / granoblastic / foliated ...")
      ),
      uiOutput("texture_history_status_ui"),
      tags$hr(),
      selectizeInput("project_name", "Project", choices = character(0), selected = NULL, options = list(create = TRUE, persist = TRUE, placeholder = "Type or select a project...")),
      textInput("author", "Author", placeholder = "Your name"),

      tags$hr(),
      h4("4c) Lithology interpretation"),
      tags$p(class = "small-note",
             "Manual interpretation only. Use this to link the sample to a selected lithological classifier."),
      selectInput("mapping_classifier", "Classifier", choices = character(0)),
      selectizeInput(
        "suggested_mapping_unit_id",
        "Suggested lithological unit",
        choices = character(0),
        selected = NULL,
        options = list(
          create = FALSE,
          persist = TRUE,
          placeholder = "Choose a lithological unit...",
          maxOptions = 5000
        )
      ),
      uiOutput("mapping_dropdown_status_ui"),
      selectInput(
        "mapping_unit_confidence",
        "Confidence",
        choices = c("-- not assigned --" = "", "High" = "High", "Medium" = "Medium", "Low" = "Low"),
        selected = ""
      ),
      actionButton("edit_mapping_comment_btn", "Edit lithology comment"),
      uiOutput("mapping_comment_status_ui"),

  ),

downloadButton("download_docx", "Generate DOCX"),
      
tags$hr(),
      h4("SQLite database (fixed path)"),
      tags$p(class="small-note", paste0("DB: ", DB_PATH)),
      actionButton("save_sample_btn", "Save sample to SQLite"),
tags$div(style="margin-top:8px; display:flex; gap:8px; flex-wrap:wrap;",
        actionButton("new_sample_btn", "New Sample"),
        actionButton("reset_form_btn", "Reset Form"),
        actionButton("clone_prev_btn", "Clone previous sample")
      ),
tags$hr(),
      h4("Image upload (container)"),
      fileInput(
        "img_upload",
        "Upload images (multi-select from a folder)",
        multiple = TRUE,
        accept = c("image/png", "image/jpeg", "image/jpg", "image/webp", "image/tiff")
      ),
      actionButton("clear_images", "Clear image container")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Rock description",
          div(class = "desc-panel",
          
          h4("5) Mineral composition"),
          tags$p(class = "small-note",
                 "Mineral: Auto-suggestions, but you can enter any words (myrmekite, granophyre, etc.). "
          ),
          tags$p(class="small-note",
                 "Abbreviations (Chl/Ab/…) are automatically converted to full names (chlorite/albite/…) when the line is preserved."),
          
          fluidRow(
            column(
              3,
              selectizeInput(
                "m_mineral", "Mineral",
                choices = character(0),
                selected = NULL,
                options = list(
                  create = TRUE,
                  persist = TRUE,
                  placeholder = "Start typing: chlorite / Chl / albite / Ab / myrmekite ...",
                  onBlur = I("function(){var v=this.$control_input.val(); if(v && v.length){ this.setValue(v,true); }}")
                )
              ),
              uiOutput("custom_mineral_controls")
            ),
            column(2, textInput("m_vol", "Vol. % (text ok)")),
            column(
              4,
              textAreaInput(
                "m_morph", "Morphology, features",
                value = "",
                rows = 2,
                placeholder = "Type, edit, copy, or insert a morphology suggestion..."
              ),
              selectInput(
                "m_morph_suggestion", "Insert morphology suggestion",
                choices = c("-- choose suggestion --" = ""),
                selected = ""
              )
            ),
            column(
              3,
              textAreaInput(
                "m_gen",
                "Genesis",
                value = "",
                rows = 1,
                width = "100%"
              ),
              selectInput(
                "m_gen_suggestion", "Insert Genesis suggestion",
                choices = c("-- choose suggestion --" = ""),
                selected = ""
              )
            )
          ),
          fluidRow(
            column(2, actionButton("add_mineral", "Add row")),
            column(2, actionButton("update_mineral", "Update selected")),
            column(2, actionButton("clear_mineral", "Clear fields")),
            column(2, actionButton("del_row", "Delete selected row")),
            column(2, actionButton("move_mineral_up", "Move up")),
            column(2, actionButton("move_mineral_down", "Move down"))
          ),
          
          tags$hr(),
          DTOutput("mineral_table"),
          
          tags$hr(),
          h4("6) Rock description (microscope)"),
          textAreaInput(
            "rock_desc", NULL,
            placeholder = "Microscopic description: texture, fabric, mineral relations, alteration, deformation, etc.",
            rows = 10,
            width = "100%"
          ),
          
          tags$hr(),
          h4("Selected images + captions (sorted by Order)"),
          uiOutput("selected_images_ui")
          )
        )
        ,

        tabPanel(
          "Database tools",
          h4("SQLite database (fixed path)"),
          tags$p(class="small-note", paste0("DB: ", DB_PATH)),
          tags$div(style="display:flex; gap:10px; flex-wrap:wrap;",
          actionButton("create_project_btn", "Create project", class="btn btn-success"),
          actionButton("delete_project_btn", "Delete project (this will also delete all samples in this project!)", class="btn btn-danger")
          ),
          tags$hr(),
          selectInput("load_project_name", "Project (from DB)", choices = character(0)),
          selectInput("load_sample_id", "Sample ID (from DB)", choices = character(0)),
          tags$div(style="display:flex; gap:10px; flex-wrap:wrap;",
          actionButton("load_sample_btn", "Load from SQLite"),
          actionButton("delete_sample_btn", "Delete sample from SQLite", class = "btn-danger")
          ),
          tags$hr(),
          h4("New sample description"),
          tags$p(
            class = "small-note",
            "Opens a blank sample form and keeps the current Project and Author. In Sample metadata, enter the new sample number in Sample ID, complete the description, and click Save sample to SQLite in the left panel when the record is ready."
          ),
          actionButton(
            "add_new_sample_description_btn",
            "Add New Sample Description",
            icon = icon("plus"),
            class = "btn-info"
          ),
          tags$hr(),
          h4("Project reports"),
          tags$p(
            class = "small-note",
            "Exports the project selected above to an XLSX workbook containing summary, unclassified_records and mineral_abbreviations sheets. The browser will ask where to save the file."
          ),
          downloadButton(
            "download_petro_summary",
            "Export petrographic summary (XLSX)",
            class = "btn-info"
          ),
          tags$hr(),
          h4("Single Sample Report → Word document"),
          tags$p(
            class = "small-note",
            "Load a sample above, then generate a Word report from the sample currently displayed in the panel."
          ),
          downloadButton(
            "download_docx_database_tools",
            "Generate DOCX",
            class = "btn-info"
          )
          )
        ,

        tabPanel(
          "QAPF",
          h4("QAPF diagram") ,
          tags$p(class="small-note",
                 "Single-point manual tool for Q, A, P, F modal values. Values are normalized to Q+A+P+F = 100 and plotted on the Streckeisen QAPF diagram."),
          tags$p(class="small-note",
                 "Some helper functions used in this QAPF tab were adapted from GCDkit: Janousek, V.; Farrow, C. M.; Erban, V. (2006), Journal of Petrology 47(6), 1255-1259, doi: 10.1093/petrology/egl013."),
          radioButtons("qapf_mode", "Diagram type", choices = c("Plutonic", "Volcanic"), selected = "Plutonic", inline = TRUE),
          tags$div(style="display:flex; gap:10px; flex-wrap:wrap; margin-bottom:10px;",
                   actionButton("open_qapf_modal_btn", "Enter QAPF values", class="btn btn-primary"),
                   actionButton("clear_qapf_btn", "Clear QAPF", class="btn btn-default"),
                   actionButton("export_qapf_image_btn", "Export image diagram", class="btn btn-info")
          ),
          verbatimTextOutput("qapf_summary"),
          plotOutput("qapf_plot", height = "720px")
        )
        ,

        tabPanel(
          "Mineral reference",
          tags$div(
            class = "mineral-reference-intro",
            tags$b("Mineral Reference — local descriptions and online research"),
            tags$div(class = "small-note",
                     "The local library remains under your control. Online searches open external resources in a new browser tab and do not modify the database.")
          ),
          actionButton(
            "open_mineral_resource_hub",
            label = tagList(icon("globe"), " Petrographic Mineral Resource Hub"),
            class = "mineral-resource-hub-btn"
          ),
          tags$span(class = "small-note", style = "margin-left:10px;",
                    "Curated links; editable from the resource window."),
          hr(),
          tabsetPanel(
            type = "pills",
            tabPanel(
              "Local reference library",
              h4("Reference library (biblio_for_mineral_descriptions.csv)"),
              tags$p(class = "small-note",
                     "Lookup Mineral is loaded from minerals.csv. Selecting a mineral searches for matching descriptions in biblio_for_mineral_descriptions.csv."),
              fluidRow(
                column(
                  4,
                  selectizeInput(
                    "lib_mineral", "Lookup Mineral",
                    choices = character(0),
                    selected = NULL,
                    options = list(
                      create = FALSE,
                      openOnFocus = TRUE,
                      placeholder = "Select a mineral..."
                    )
                  ),
                  uiOutput("lib_mineral_status_ui")
                ),
                column(4, uiOutput("lib_rocktype_ui")),
                column(4, numericInput("lib_limit", "Max results", value = 20, min = 5, max = 200, step = 5))
              ),
              fluidRow(
                column(4, actionButton("lib_reload_minerals", "Reload mineral list", icon = icon("refresh")))
              ),
              br(),
              DTOutput("lib_table")
            ),
            tabPanel(
              "Online literature search",
              h4("Build a focused web query"),
              tags$p(class = "small-note",
                     "Example: topic = Optical properties, mineral = Pigeonite, rocks = Basalt and Andesite, logic = OR."),
              fluidRow(
                column(4, textInput("web_ref_topic", "Topic / property", value = "Optical properties")),
                column(4, selectizeInput("web_ref_minerals", "Mineral(s)", choices = character(0), multiple = TRUE,
                                         options = list(create = TRUE, persist = FALSE, placeholder = "Pigeonite ..."))),
                column(4, selectizeInput("web_ref_rocks", "Rock context(s)",
                                         choices = c("Basalt", "Andesite", "Dacite", "Rhyolite", "Gabbro", "Diorite", "Granite", "Peridotite", "Metamorphic rock"),
                                         multiple = TRUE,
                                         options = list(create = TRUE, persist = FALSE, placeholder = "Basalt, Andesite ...")))
              ),
              radioButtons(
                "web_ref_rock_logic", "Relationship between several rock contexts",
                choices = c("Any selected rock (OR)" = "OR", "All selected rocks (AND)" = "AND"),
                selected = "OR", inline = TRUE
              ),
              uiOutput("web_ref_query_preview"),
              uiOutput("web_ref_search_links"),
              tags$p(class = "small-note", style = "margin-top:12px;",
                     "Search results are external. Check the authority, date and citation of a source before using its information in a petrographic description.")
            )
          )
                   
        ),
        tabPanel(
          "Project map / Google Earth",
          h4("Project map and GIS export"),
          tags$p(class = "small-note",
                 "Plots samples with valid coordinates. Use Main Project for normal work, Main + overlay projects for comparison, or All projects overview for presentation/general database review. Popups show key metadata only; photographs are excluded. GIS layer export remains limited to the Main Project."),
          radioButtons(
            "project_map_mode",
            "Map loading mode",
            choices = c(
              "Main project only" = "main_only",
              "Main project + overlay projects" = "main_overlay",
              "All projects overview (super-heavy)" = "all_projects"
            ),
            selected = "main_only",
            inline = FALSE
          ),
          conditionalPanel(
            condition = "input.project_map_mode == 'all_projects'",
            tags$div(
              style = "background:#fff3cd; border:1px solid #ffeeba; padding:8px; border-radius:6px; margin-bottom:10px;",
              tags$b("Super-heavy overview mode: "),
              "all projects with valid coordinates will be loaded and colored by project. For large databases this may be slow. GIS export still uses the Main Project only."
            )
          ),
          fluidRow(
            column(4, selectInput("map_project_name", "Main Project", choices = character(0))),
            column(4, selectizeInput(
              "map_overlay_projects",
              "Overlay projects (map only)",
              choices = character(0),
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional: choose additional projects for visual comparison...")
            )),
            column(4, actionButton("load_project_map_btn", "Load / refresh overview map", class = "btn btn-primary"))
          ),
          fluidRow(
            column(6, selectInput("map_export_format", "GIS export format (Main Project only)", choices = c("KML (Google Earth)" = "kml", "ESRI Shapefile (ZIP)" = "shp", "GeoPackage" = "gpkg"), selected = "kml")),
            column(6, selectInput("map_export_crs", "GIS export CRS (Main Project only)", choices = c("WGS84 geographic (EPSG:4326)" = "4326", "PSAD56 / UTM 20N (EPSG:24820)" = "24820", "PSAD56 / UTM 21N (EPSG:24821)" = "24821"), selected = "4326"))
          ),
          tags$p(class = "small-note",
                 "Overlay projects and All projects overview are used only for the on-screen Leaflet map and map PNG/PDF exports. KML/SHP/GeoPackage export uses the Main Project only. KML is always exported in WGS84; Shapefile is compact; GeoPackage keeps full metadata."),
          tags$div(style="display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
                   downloadButton("download_project_layer", "Export GIS layer")
          ),
          tags$br(),
          tags$details(
            class = "project-map-settings",
            tags$summary("Project map export setting (Click to open)"),
            tags$p(class = "small-note",
                   "Two map styles are available for report/article export. ",
                   "'Ascetic' mode builds a clean static map from coordinates only. ",
                   "'Advanced basemap' mode tries to export a static map with a web basemap. ",
                   "The map extent frame is not drawn; corner coordinates mark the exported extent. ",
                   "Advanced maps also display the basemap source."),
            tags$div(style="display:flex; gap:10px; flex-wrap:wrap; align-items:center; margin: 8px 0 12px 0;",
                     downloadButton("download_project_map_png", "Export project map PNG"),
                     downloadButton("download_project_map_pdf", "Export project map PDF")
            ),
            tags$div(
              class = "map-style-block",
              tags$div(
                class = "map-style-header",
                icon("map"),
                tags$span(class = "map-style-title", "Map style"),
                tags$span(class = "map-style-hint", "PNG/PDF export")
              ),
              radioButtons(
                "static_map_mode",
                label = NULL,
                choices = c("Ascetic (clean static map)" = "static",
                            "Advanced (static map with basemap)" = "advanced"),
                selected = "static",
                inline = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.static_map_mode == 'advanced'",
              tags$div(
                class = "warning-box",
                strong("Important note"),
                br(),
                "Advanced basemap export requires htmlwidgets and webshot2, a working browser rendering backend, ",
                "and a reliable internet connection while map tiles are loading."
              ),
              uiOutput("advanced_map_dependency_status"),
              selectInput(
                "advanced_map_provider",
                "Basemap provider",
                choices = c("OpenStreetMap" = "OpenStreetMap",
                            "CartoDB Positron" = "CartoDB.Positron",
                            "Esri World Imagery" = "Esri.WorldImagery",
                            "OpenTopoMap" = "OpenTopoMap"),
                selected = "CartoDB.Positron"
              )
            ),
            fluidRow(
              column(3, numericInput("static_map_point_cex", "Point size", value = 1.4, min = 0.4, max = 5, step = 0.1)),
              column(3, numericInput("static_map_label_cex", "Sample_ID label size", value = 0.8, min = 0.3, max = 3, step = 0.1)),
              column(3, numericInput("static_map_corner_cex", "Corner coordinate label size", value = 0.7, min = 0.3, max = 2, step = 0.1)),
              column(3, numericInput("static_map_buffer_pct", "Map buffer (%)", value = 8, min = 0, max = 50, step = 1))
            ),
            checkboxInput("static_map_show_sample_labels", "Show Sample_ID labels", value = TRUE),
            checkboxInput("static_map_show_corner_coords", "Show corner coordinate labels", value = TRUE)
          ),
          tags$hr(),
          leafletOutput("project_map", height = "700px"),
          tags$hr(),
          h4("Project point metadata"),
          DTOutput("project_map_table")
        ),
tabPanel(
  "Settings",
  tags$div(
    style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
    tags$div(
      h4(style = "margin-bottom:4px;", "Application settings"),
      tags$p(
        class = "small-note",
        style = "margin-bottom:0;",
        "These settings are shared through the current SQLite database."
      )
    ),
    actionButton("about_program_btn", "About Program", class = "btn settings-secondary-btn")
  ),
  tags$hr(),

  wellPanel(
    h4("General"),
    tags$p(class = "small-note", "Defaults are used when the corresponding sample fields are empty."),
    fluidRow(
      column(6, textInput("author_default", "Default Author")),
      column(6, selectizeInput(
        "project_default",
        "Default Project",
        choices = character(0),
        selected = NULL,
        options = list(placeholder = "Choose an existing project...")
      ))
    ),
    checkboxInput("confirm_load_setting", "Confirm before loading a sample", value = TRUE),
    textInput("panel_bg_setting", "Description panel background color (hex)", value = "#ffffff", placeholder = "#ffffff"),
    tags$p(class = "small-note", "The panel color is previewed immediately; Save all settings makes it persistent.")
  ),

  wellPanel(
    h4("Report and image display"),
    fluidRow(
      column(6, numericInput(
        "docx_font_size_setting",
        "DOCX font size for report values",
        value = 10, min = 7, max = 20, step = 1
      )),
      column(6, numericInput(
        "max_sel_images",
        "Maximum selected images",
        value = 10, min = 1, max = 50, step = 1
      ))
    ),
    tags$p(class = "small-note", "The image limit is enforced on the description panel, SQLite save and DOCX export."),
    h5("Image container display size"),
    tags$p(
      class = "small-note",
      "These values change only the on-screen image cards. They do not resize or recompress files."
    ),
    fluidRow(
      column(6, numericInput("img_card_width_setting", "Image card width (px)", value = 280, min = 180, max = 700, step = 10)),
      column(6, numericInput("img_thumb_height_setting", "Thumbnail height (px)", value = 170, min = 100, max = 600, step = 10))
    ),
    actionButton(
      "apply_image_container_size_btn",
      "Apply size to image cards",
      icon = icon("expand"),
      class = "btn settings-secondary-btn"
    ),
    tags$p(
      class = "small-note",
      style = "margin-top:8px; margin-bottom:0;",
      "Updates the cards in the Image container immediately. Use Save all settings to keep this size after restart."
    )
  ),

  tags$details(
    class = "settings-advanced-details",
    tags$summary(
      "Advanced image optimization"
    ),
    wellPanel(
      tags$p(
        class = "small-note",
        "Images are standardized to JPEG and compressed when they are imported. These limits apply to newly imported images; use Save all settings to keep them after restart."
      ),
      tags$p(
        class = "small-note",
        tags$b("Image archive: "),
        "when a previously saved image is removed from the selection, the app asks what to do when the sample is saved. ",
        "If it is archived, the file is not deleted: it is moved to ",
        tags$code("images/_archive/<Project>/<Sample>/<YYYYMMDD_HHMMSS>/"),
        ", so its origin and archive time remain clear and the file can be restored manually."
      ),
      checkboxInput(
        "track_unused_image_files_setting",
        "Also check the current sample folder for unused image files when saving",
        value = FALSE
      ),
      tags$p(
        class = "small-note",
        "Optional. When enabled, the app also detects image files that exist in ",
        tags$code("images/<Project>/<Sample>/"),
        " but are not selected in the Image container. This includes files copied into the folder manually. ",
        "The default check only tracks previously saved SQLite images that were removed from the selection."
      ),
      fluidRow(
        column(6, numericInput("max_image_mb_setting", "Max image file size (MB)", value = 4, min = 0.5, max = 50, step = 0.5)),
        column(6, numericInput("max_image_dim_setting", "Max image dimension (px, long side)", value = 2500, min = 800, max = 8000, step = 100))
      ),
      fluidRow(
        column(6, numericInput("jpeg_quality_default_setting", "JPEG quality (start)", value = 85, min = 40, max = 100, step = 1)),
        column(6, numericInput("jpeg_quality_min_setting", "JPEG quality (minimum)", value = 60, min = 20, max = 95, step = 1))
      )
    )
  ),

  tags$div(
    style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap; padding:12px; border-top:1px solid #ddd;",
    actionButton("save_settings_btn", "Save all settings", class = "btn btn-primary"),
    actionButton("discard_settings_btn", "Discard unsaved changes", class = "btn settings-secondary-btn"),
    uiOutput("settings_save_status")
  )
),
tabPanel(
  "Maintenance",
  h4("Reference libraries and database maintenance"),
  tags$p(
    class = "small-note",
    "Uploaded reference CSV files are used for the current session only. To make them permanent, replace the corresponding files in the application folder."
  ),
  h4("Reference library (CSV)"),
  fileInput("biblio_file", "Upload biblio_for_mineral_descriptions.csv (optional)", accept = c(".csv")),
  fileInput("minerals_file", "Upload minerals.csv (optional)", accept = c(".csv")),
  fileInput("grains_file", "Upload grainsizes.csv (optional)", accept = c(".csv")),
  fileInput("textures_file", "Upload textures.csv (optional)", accept = c(".csv")),

  tags$hr(),
  h4("Mapping units / rock-type classifier (CSV)"),
  tags$p(class = "small-note",
         "The app now uses one external file only: lookup_rocktype.csv. ",
         "Place it in the app folder or in the SQLite database folder."),
  tags$p(class = "small-note",
         "Required fields: ORIGINAL_LITHOLOGY_ENGLISH; LITHOLOGY_Canada_Geological_Survey; LITHOLOGY_CLASSIFICATION."),
  actionButton("reload_library", "Reload all CSV sources", class = "btn btn-primary"),
  tags$br(),
  tags$br(),
  verbatimTextOutput("mapping_classifier_status"),

  tags$hr(),
  h4("Database info"),
  actionButton("refresh_db_info_btn", "Refresh database info", class = "btn settings-secondary-btn"),
  tags$br(),
  tags$br(),
  verbatimTextOutput("db_info")
),
        tabPanel(
          "Help / User Guide",
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; margin-bottom:8px;",
            tags$div(
              h4(style = "margin-bottom:4px;", "Make_Thin_Section User Guide"),
              tags$p(
                class = "small-note",
                style = "margin-bottom:0;",
                "This page displays README.md from the application folder as an in-app user guide."
              )
            )
          ),
          tags$hr(),
          build_user_guide_ui()
        ),
        tabPanel(
          "Image container (sorted)",
          tags$div(
            style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap; margin-bottom:10px;",
            actionButton("launch_fragmentator_btn", "Launch Fragmentator", class="btn btn-primary"),
            actionButton("about_fragmentator_btn", "About Fragmentator", class="btn btn-default")
          ),
          tags$hr(),
          tags$h4("Paste from clipboard"),
          tags$p(class = "small-note",
                 "Click the box (focus) and press Ctrl+V. Then set Order numbers to control sequence."),
          tags$div(
            id = "paste_area", tabindex = "0", contenteditable = "true",
            HTML("<b>Click here</b> and press <b>Ctrl+V</b> to paste an image from clipboard.")
          ),
          tags$hr(),
uiOutput("image_gallery_ui")
        )
      )
    )
  )
)
server <- function(input, output, session) {

  # ---- V62: editable curated web resources for Mineral Reference ----
  empty_web_resources <- function() {
    data.frame(
      display_order = integer(0),
      resource_name = character(0),
      category = character(0),
      url = character(0),
      description = character(0),
      active = logical(0),
      stringsAsFactors = FALSE
    )
  }

  normalize_web_resources <- function(df) {
    required <- names(empty_web_resources())
    if (is.null(df)) df <- empty_web_resources()
    names(df) <- trimws(gsub("\ufeff", "", names(df)))
    for (nm in setdiff(required, names(df))) df[[nm]] <- NA
    df <- df[, required, drop = FALSE]
    df$display_order <- suppressWarnings(as.integer(df$display_order))
    missing_order <- is.na(df$display_order)
    df$display_order[missing_order] <- seq_len(sum(missing_order)) + max(c(0L, df$display_order[!missing_order]), na.rm = TRUE)
    for (nm in c("resource_name", "category", "url", "description")) {
      df[[nm]] <- trimws(as.character(df[[nm]] %||% ""))
      df[[nm]][is.na(df[[nm]])] <- ""
    }
    active_text <- tolower(trimws(as.character(df$active %||% TRUE)))
    df$active <- active_text %in% c("true", "t", "1", "yes", "y")
    df <- df[nzchar(df$resource_name) & grepl("^https?://", df$url, ignore.case = TRUE), , drop = FALSE]
    df[order(df$display_order, tolower(df$resource_name)), , drop = FALSE]
  }

  load_web_resources <- function() {
    if (!file.exists(MINERAL_WEB_RESOURCES_PATH)) return(empty_web_resources())
    normalize_web_resources(tryCatch(read_csv_guess(MINERAL_WEB_RESOURCES_PATH), error = function(e) NULL))
  }

  web_resources_rv <- reactiveVal(load_web_resources())
  web_resource_editor_index <- reactiveVal(NA_integer_)

  save_web_resources <- function(df) {
    if (isTRUE(READ_ONLY_MODE)) stop("Read-only LAN mode: the web-resource list cannot be changed.")
    df <- normalize_web_resources(df)
    utils::write.csv(df, MINERAL_WEB_RESOURCES_PATH, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    web_resources_rv(df)
    invisible(TRUE)
  }

  show_web_resource_hub <- function() {
    edit_controls <- if (isTRUE(READ_ONLY_MODE)) {
      tags$span(class = "text-muted", "Read-only mode: editing is disabled.")
    } else {
      tagList(
        actionButton("web_resource_add", "Add", icon = icon("plus"), class = "btn btn-success"),
        actionButton("web_resource_edit", "Edit selected", icon = icon("pencil"), class = "btn btn-primary"),
        actionButton("web_resource_delete", "Delete selected", icon = icon("trash"), class = "btn btn-danger")
      )
    }
    showModal(modalDialog(
      title = tagList(icon("globe"), " Petrographic Mineral Resource Hub"),
      tags$p(class = "small-note",
             "A small curated catalogue of microscopy and optical-mineralogy resources. Links open in a new browser tab."),
      DTOutput("web_resource_table"),
      tags$p(class = "small-note",
             "Stored in mineral_reference_web_resources.csv; you may also edit that file in a spreadsheet editor while the app is closed."),
      footer = tagList(edit_controls, actionButton("web_resource_reload", "Reload CSV", icon = icon("refresh")), modalButton("Close")),
      size = "l", easyClose = TRUE
    ))
  }

  show_web_resource_editor <- function(index = NA_integer_) {
    df <- web_resources_rv()
    editing <- !is.na(index) && index >= 1 && index <= nrow(df)
    row <- if (editing) df[index, , drop = FALSE] else data.frame(
      display_order = if (nrow(df)) max(df$display_order, na.rm = TRUE) + 1L else 1L,
      resource_name = "", category = "", url = "https://", description = "", active = TRUE
    )
    web_resource_editor_index(if (editing) index else NA_integer_)
    showModal(modalDialog(
      title = if (editing) "Edit web resource" else "Add web resource",
      numericInput("web_resource_order_input", "Display order", value = row$display_order[[1]], min = 1, step = 1),
      textInput("web_resource_name_input", "Resource name", value = row$resource_name[[1]]),
      textInput("web_resource_category_input", "Category", value = row$category[[1]]),
      textInput("web_resource_url_input", "URL", value = row$url[[1]]),
      textAreaInput("web_resource_description_input", "Short description", value = row$description[[1]], rows = 3),
      checkboxInput("web_resource_active_input", "Active", value = isTRUE(row$active[[1]])),
      footer = tagList(modalButton("Cancel"), actionButton("web_resource_save", "Save resource", class = "btn btn-primary")),
      easyClose = FALSE
    ))
  }

  output$web_resource_table <- renderDT({
    df <- web_resources_rv()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No web resources are configured."), options = list(dom = "t"), rownames = FALSE))
    }
    safe_url <- ifelse(grepl("^https?://", df$url, ignore.case = TRUE), df$url, "")
    link_html <- vapply(seq_len(nrow(df)), function(i) {
      if (!isTRUE(df$active[[i]])) return('<span class="text-muted">Inactive</span>')
      if (!nzchar(safe_url[[i]])) return("")
      sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">Open resource</a>',
              htmltools::htmlEscape(safe_url[[i]], attribute = TRUE))
    }, character(1))
    shown <- data.frame(
      Order = df$display_order,
      Resource = htmltools::htmlEscape(df$resource_name),
      Category = htmltools::htmlEscape(df$category),
      Description = htmltools::htmlEscape(df$description),
      Website = link_html,
      Active = ifelse(df$active, "Yes", "No"),
      stringsAsFactors = FALSE
    )
    datatable(shown, escape = FALSE, selection = "single", rownames = FALSE,
              options = list(pageLength = 8, dom = "tip", autoWidth = TRUE))
  })

  observeEvent(input$open_mineral_resource_hub, show_web_resource_hub(), ignoreInit = TRUE)
  observeEvent(input$web_resource_reload, {
    web_resources_rv(load_web_resources())
    showNotification("Web-resource list reloaded from CSV.", type = "message", duration = 3)
  }, ignoreInit = TRUE)
  observeEvent(input$web_resource_add, show_web_resource_editor(), ignoreInit = TRUE)
  observeEvent(input$web_resource_edit, {
    selected <- input$web_resource_table_rows_selected
    if (length(selected) != 1) {
      showNotification("Select one resource to edit.", type = "warning", duration = 4)
      return()
    }
    show_web_resource_editor(selected[[1]])
  }, ignoreInit = TRUE)
  observeEvent(input$web_resource_save, {
    tryCatch({
      nm <- trimws(input$web_resource_name_input %||% "")
      url <- trimws(input$web_resource_url_input %||% "")
      if (!nzchar(nm)) stop("Resource name is required.")
      if (!grepl("^https?://", url, ignore.case = TRUE)) stop("URL must begin with http:// or https://")
      df <- web_resources_rv()
      new_row <- data.frame(
        display_order = as.integer(input$web_resource_order_input %||% (nrow(df) + 1L)),
        resource_name = nm,
        category = trimws(input$web_resource_category_input %||% ""),
        url = url,
        description = trimws(input$web_resource_description_input %||% ""),
        active = isTRUE(input$web_resource_active_input),
        stringsAsFactors = FALSE
      )
      idx <- web_resource_editor_index()
      if (is.na(idx)) df <- rbind(df, new_row) else df[idx, names(new_row)] <- new_row
      save_web_resources(df)
      showNotification("Web resource saved.", type = "message", duration = 3)
      show_web_resource_hub()
    }, error = function(e) showNotification(e$message, type = "error", duration = 6))
  }, ignoreInit = TRUE)
  observeEvent(input$web_resource_delete, {
    selected <- input$web_resource_table_rows_selected
    if (length(selected) != 1) {
      showNotification("Select one resource to delete.", type = "warning", duration = 4)
      return()
    }
    web_resource_editor_index(selected[[1]])
    showModal(modalDialog(
      title = "Delete web resource?",
      tags$p("This removes the selected entry from mineral_reference_web_resources.csv."),
      footer = tagList(modalButton("Cancel"), actionButton("web_resource_delete_confirm", "Delete", class = "btn btn-danger")),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE)
  observeEvent(input$web_resource_delete_confirm, {
    tryCatch({
      df <- web_resources_rv()
      idx <- web_resource_editor_index()
      if (is.na(idx) || idx < 1 || idx > nrow(df)) stop("The selected resource is no longer available.")
      save_web_resources(df[-idx, , drop = FALSE])
      showNotification("Web resource deleted.", type = "message", duration = 3)
      show_web_resource_hub()
    }, error = function(e) showNotification(e$message, type = "error", duration = 6))
  }, ignoreInit = TRUE)

  saved_settings_state <- reactiveVal(NULL)

  numeric_setting_text <- function(x, default) {
    x <- suppressWarnings(as.numeric(x %||% default))
    if (is.na(x)) x <- default
    as.character(x)
  }

  current_settings_state <- reactive({
    list(
      author = trimws(input$author_default %||% ""),
      project = trimws(input$project_default %||% ""),
      confirm_load = isTRUE(input$confirm_load_setting),
      panel_bg = trimws(input$panel_bg_setting %||% "#ffffff"),
      docx_font_size = numeric_setting_text(input$docx_font_size_setting, 10),
      max_sel_images = numeric_setting_text(input$max_sel_images, 10),
      img_card_width = numeric_setting_text(input$img_card_width_setting, 280),
      img_thumb_height = numeric_setting_text(input$img_thumb_height_setting, 170),
      max_image_mb = numeric_setting_text(input$max_image_mb_setting, 4),
      max_image_dim = numeric_setting_text(input$max_image_dim_setting, 2500),
      jpeg_quality_default = numeric_setting_text(input$jpeg_quality_default_setting, 85),
      jpeg_quality_min = numeric_setting_text(input$jpeg_quality_min_setting, 60),
      track_unused_image_files = isTRUE(input$track_unused_image_files_setting)
    )
  })

  apply_settings_state_to_ui <- function(state) {
    if (is.null(state)) return(invisible(FALSE))

    project_choices <- tryCatch({
      con <- db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      db_init(con)
      unique(c(list_projects(con), state$project %||% ""))
    }, error = function(e) unique(state$project %||% ""))
    project_choices <- project_choices[nzchar(trimws(project_choices))]

    updateTextInput(session, "author_default", value = state$author %||% "")
    updateSelectizeInput(
      session,
      "project_default",
      choices = project_choices,
      selected = state$project %||% "",
      server = TRUE
    )
    updateCheckboxInput(session, "confirm_load_setting", value = isTRUE(state$confirm_load))
    updateTextInput(session, "panel_bg_setting", value = state$panel_bg %||% "#ffffff")
    updateNumericInput(session, "docx_font_size_setting", value = as.numeric(state$docx_font_size %||% 10))
    updateNumericInput(session, "max_sel_images", value = as.numeric(state$max_sel_images %||% 10))
    updateNumericInput(session, "img_card_width_setting", value = as.numeric(state$img_card_width %||% 280))
    updateNumericInput(session, "img_thumb_height_setting", value = as.numeric(state$img_thumb_height %||% 170))
    updateNumericInput(session, "max_image_mb_setting", value = as.numeric(state$max_image_mb %||% 4))
    updateNumericInput(session, "max_image_dim_setting", value = as.numeric(state$max_image_dim %||% 2500))
    updateNumericInput(session, "jpeg_quality_default_setting", value = as.numeric(state$jpeg_quality_default %||% 85))
    updateNumericInput(session, "jpeg_quality_min_setting", value = as.numeric(state$jpeg_quality_min %||% 60))
    updateCheckboxInput(
      session,
      "track_unused_image_files_setting",
      value = isTRUE(state$track_unused_image_files)
    )

    session$sendCustomMessage("setCssVar", list(name = "--panel-bg", value = state$panel_bg %||% "#ffffff"))
    session$sendCustomMessage("setCssVar", list(name = "--img-card-width", value = paste0(state$img_card_width %||% 280, "px")))
    session$sendCustomMessage("setCssVar", list(name = "--img-thumb-height", value = paste0(state$img_thumb_height %||% 170, "px")))
    invisible(TRUE)
  }

  output$settings_save_status <- renderUI({
    saved <- saved_settings_state()
    if (is.null(saved)) {
      return(tags$span(class = "text-muted", "Loading settings..."))
    }
    if (identical(current_settings_state(), saved)) {
      tags$span(style = "color:#2e7d32; font-weight:700;", "All changes saved")
    } else {
      tags$span(style = "color:#b26a00; font-weight:700;", "Unsaved changes")
    }
  })

  observeEvent(input$discard_settings_btn, {
    state <- saved_settings_state()
    if (is.null(state)) return()
    apply_settings_state_to_ui(state)
    showNotification("Unsaved setting changes were discarded.", type = "message", duration = 3)
  }, ignoreInit = TRUE)

  observeEvent(input$about_program_btn, {
    showModal(modalDialog(
      title = "About Make Thin Section",
      tags$p("Petrographic description, image management, SQLite storage and DOCX/GIS reporting application."),
      tags$p(tags$b("Application version: "), "62 — Mineral Reference redesign and online search"),
      tags$p(
        class = "small-note",
        "Settings on this tab are stored in the current SQLite database and may therefore be shared by users of the same database."
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  # ---- Image Fragmentator launcher (standalone only) ----
  fragmentator_launcher_status <- reactiveVal("")

  # V59: sample-level mapping-unit comment is edited in a modal dialog.
  mapping_unit_comment_rv <- reactiveVal("")

  find_fragmentator_app <- function() {
    candidates <- unique(c(
      FRAGMENTATOR_APP_PATH,
      file.path(getwd(), "image_fragmentator_V5_standalone_no_bridge.R"),
      file.path(dirname(DB_PATH), "image_fragmentator_V5_standalone_no_bridge.R"),
      file.path(dirname(DB_PATH), "tools", "image_fragmentator_V5_standalone_no_bridge.R")
    ))

    candidates <- normalizePath(candidates, winslash = "/", mustWork = FALSE)
    hit <- candidates[file.exists(candidates)]

    if (length(hit) < 1) {
      stop(paste0(
        "Image Fragmentator was not found.\n\n",
        "Checked paths:\n",
        paste(candidates, collapse = "\n"),
        "\n\nPlace image_fragmentator_V6.R in the same folder as this app, ",
        "or in the petro_db folder, or in petro_db/tools, or edit FRAGMENTATOR_APP_PATH at the top of this script."
      ))
    }

    hit[1]
  }

  fragmentator_preflight <- function() {
    cran_pkgs <- c("shiny", "magick", "png")
    bioc_pkgs <- c("EBImage")
    helper_pkgs <- c("BiocManager")

    missing_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    missing_helper <- helper_pkgs[!vapply(helper_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    missing_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace, logical(1), quietly = TRUE)]

    list(
      ok = length(c(missing_cran, missing_helper, missing_bioc)) == 0,
      missing_cran = missing_cran,
      missing_helper = missing_helper,
      missing_bioc = missing_bioc
    )
  }

  show_fragmentator_preflight_modal <- function(check) {
    install_lines <- character(0)

    cran_all <- c(check$missing_cran, check$missing_helper)
    if (length(cran_all) > 0) {
      install_lines <- c(
        install_lines,
        paste0("install.packages(c(", paste(shQuote(cran_all), collapse = ", "), "))")
      )
    }

    if (length(check$missing_bioc) > 0) {
      install_lines <- c(install_lines, "BiocManager::install(\"EBImage\", ask = FALSE, update = FALSE)")
    }

    showModal(modalDialog(
      title = "Image Fragmentator dependencies",
      easyClose = TRUE,
      size = "l",
      tags$p(
        "Image Fragmentator needs a few additional image-analysis packages before it can launch."
      ),
      tags$p(
        "Install the missing packages once in R, then click Launch Fragmentator again."
      ),
      tags$pre(paste(install_lines, collapse = "\n")),
      tags$p(class = "small-note",
             "EBImage is distributed through Bioconductor, so it is installed with BiocManager."),
      footer = tagList(
        modalButton("Close")
      )
    ))
  }

  launch_fragmentator_standalone <- function() {
    app_path <- find_fragmentator_app()

    rscript <- file.path(
      R.home("bin"),
      if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
    )
    if (!file.exists(rscript)) rscript <- "Rscript"

    launcher_dir <- file.path(tempdir(), "Make_Thin_Section_fragmentator_launcher")
    if (!dir.exists(launcher_dir)) {
      dir.create(launcher_dir, recursive = TRUE, showWarnings = FALSE)
    }

    runner_path <- file.path(launcher_dir, "run_fragmentator_standalone.R")
    log_path <- file.path(launcher_dir, "fragmentator_standalone_launch.log")
    bat_path <- file.path(launcher_dir, "run_fragmentator_standalone.bat")

    app_path_norm <- normalizePath(app_path, winslash = "/", mustWork = TRUE)
    launcher_dir_norm <- normalizePath(launcher_dir, winslash = "/", mustWork = FALSE)
    log_path_norm <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

    runner_lines <- c(
      paste0("log_path <- ", shQuote(log_path_norm)),
      paste0("app_path <- ", shQuote(app_path_norm)),
      "cat('\\n--- Image Fragmentator standalone launch ---\\n', file = log_path, append = TRUE)",
      "cat('Time: ', format(Sys.time(), '%Y-%m-%d %H:%M:%S'), '\\n', file = log_path, append = TRUE)",
      "cat('App path: ', app_path, '\\n', file = log_path, append = TRUE)",
      "cat('Working directory: ', dirname(app_path), '\\n', file = log_path, append = TRUE)",
      "find_free_port <- function(start = 3860, end = 3899) {",
      "  for (p in seq.int(start, end)) {",
      "    con <- tryCatch(socketConnection(host = '127.0.0.1', port = p, open = 'r+', blocking = FALSE, timeout = 0.2), error = function(e) NULL)",
      "    if (is.null(con)) return(p)",
      "    try(close(con), silent = TRUE)",
      "  }",
      "  stop('No free port found in range 3860:3899')",
      "}",
      "tryCatch({",
      "  setwd(dirname(app_path))",
      "  options(shiny.launch.browser = TRUE)",
      "  port_use <- find_free_port(3860, 3899)",
      "  cat('Starting Image Fragmentator on http://127.0.0.1:', port_use, '/\\n', sep = '', file = log_path, append = TRUE)",
      "  app_env <- new.env(parent = globalenv())",
      "  app_obj <- source(app_path, local = app_env)$value",
      "  if (inherits(app_obj, 'shiny.appobj')) {",
      "    shiny::runApp(app_obj, launch.browser = TRUE, host = '127.0.0.1', port = port_use)",
      "  } else {",
      "    stop('Fragmentator script did not return a Shiny app object.')",
      "  }",
      "}, error = function(e) {",
      "  cat('ERROR: ', conditionMessage(e), '\\n', file = log_path, append = TRUE)",
      "  cat('TRACEBACK:\\n', file = log_path, append = TRUE)",
      "  tb <- capture.output(traceback())",
      "  cat(paste(tb, collapse = '\\n'), '\\n', file = log_path, append = TRUE)",
      "  stop(e)",
      "})"
    )

    writeLines(runner_lines, runner_path, useBytes = TRUE)

    if (.Platform$OS.type == "windows") {
      # More reliable Windows launch:
      # - create a .bat file;
      # - open it with shell.exec();
      # - keep a visible cmd window open with /k so possible errors are not hidden.
      #
      # This is better for debugging than system2(..., wait = FALSE), which may fail silently
      # depending on Windows/RStudio/session policy.

      rscript_win <- normalizePath(rscript, winslash = "/", mustWork = FALSE)
      runner_win  <- normalizePath(runner_path, winslash = "/", mustWork = FALSE)
      log_win     <- normalizePath(log_path, winslash = "/", mustWork = FALSE)

      bat_lines <- c(
        "@echo off",
        "title Image Fragmentator launcher",
        paste0("\"", rscript_win, "\" \"", runner_win, "\""),
        "exit /b 0"
      )

      writeLines(bat_lines, bat_path, useBytes = TRUE)
      system2(
        command = "cmd.exe",
        args = c("/c", "start", "\"Image Fragmentator\"", shQuote(normalizePath(bat_path, winslash = "\\", mustWork = FALSE))),
        wait = FALSE,
        invisible = TRUE
      )
    } else {
      system2(
        command = rscript,
        args = shQuote(runner_path),
        wait = FALSE,
        invisible = FALSE
      )
    }

    list(
      app_path = app_path_norm,
      runner_path = normalizePath(runner_path, winslash = "/", mustWork = FALSE),
      bat_path = normalizePath(bat_path, winslash = "/", mustWork = FALSE),
      log_path = log_path_norm
    )
  }

  observeEvent(input$launch_fragmentator_btn, {
    tryCatch({
      dep_check <- fragmentator_preflight()
      if (!isTRUE(dep_check$ok)) {
        show_fragmentator_preflight_modal(dep_check)
        missing_txt <- paste(c(dep_check$missing_cran, dep_check$missing_helper, dep_check$missing_bioc), collapse = ", ")
        fragmentator_launcher_status(paste("Image Fragmentator dependencies are missing:", missing_txt))
        return(invisible(FALSE))
      }

      info <- launch_fragmentator_standalone()

      fragmentator_launcher_status(paste0(
        "Last launch information\n\n",
        "Fragmentator app:\n", info$app_path, "\n\n",
        "Launcher script:\n", info$runner_path, "\n\n",
        "Windows BAT launcher:\n", info$bat_path, "\n\n",
        "Launch log:\n", info$log_path
      ))

      showNotification("Image Fragmentator launched.", type = "message", duration = 4)
    }, error = function(e) {
      fragmentator_launcher_status(paste("Image Fragmentator launch error:", e$message))
      showNotification(paste("Image Fragmentator launch error:", e$message), type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)


  output$mapping_comment_status_ui <- renderUI({
    cm <- trimws(mapping_unit_comment_rv() %||% "")
    if (!nzchar(cm)) {
      tags$p(class = "small-note", "Lithology comment: empty")
    } else {
      preview <- clip_text(cm, 130)
      tags$p(class = "small-note", paste0("Lithology comment: ", preview))
    }
  })

  observeEvent(input$edit_mapping_comment_btn, {
    showModal(modalDialog(
      title = "Lithology interpretation comment / basis",
      textAreaInput(
        "mapping_unit_comment_edit",
        NULL,
        value = mapping_unit_comment_rv() %||% "",
        rows = 12,
        width = "100%",
        placeholder = "Explain why this lithological unit was selected. You can mention field context, petrography, location, uncertainty, conflicts with regional map, etc."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_mapping_comment_btn", "Save comment", class = "btn btn-primary")
      ),
      size = "l",
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$save_mapping_comment_btn, {
    mapping_unit_comment_rv(input$mapping_unit_comment_edit %||% "")
    removeModal()
    showNotification("Lithology comment updated. Use Save sample to SQLite to store it.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  observeEvent(input$about_fragmentator_btn, {
    showModal(modalDialog(
      title = "About Image Fragmentator",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        tags$p(
          "Image Fragmentator is a small standalone tool for experimental segmentation ",
          "of petrographic and geological images."
        ),
        tags$p(
          "The program is intended mainly for visual separation of textures, structures, ",
          "veins, alteration domains, matrix, clasts, phenocrysts, pores, spots, or other ",
          "contrasting image domains."
        ),
        tags$p(
          "Current methods include grayscale threshold segmentation, k-means RGB clustering, ",
          "and experimental marker-controlled watershed segmentation."
        ),
        tags$div(
          style = "background:#fff3cd; border:1px solid #ffeeba; padding:10px; border-radius:6px;",
          tags$b("Important limitation: "),
          "Fragmentator is not a magic-wand mineral identification tool. ",
          "It does not automatically recognize minerals or produce a final geological interpretation. ",
          "The geological meaning of segmented domains must be evaluated by the user."
        ),
        tags$hr(),
        tags$p(
          "Recommended workflow: upload an original unlabelled image, select ROI if needed, ",
          "run segmentation, and export PNG with legend plus CSV statistics."
        )
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$apply_image_container_size_btn, {
    card_w <- as.integer(input$img_card_width_setting %||% 280)
    thumb_h <- as.integer(input$img_thumb_height_setting %||% 170)

    if (is.na(card_w) || card_w < 180) card_w <- 280
    if (is.na(thumb_h) || thumb_h < 100) thumb_h <- 170

    session$sendCustomMessage("setCssVar", list(name = "--img-card-width", value = paste0(card_w, "px")))
    session$sendCustomMessage("setCssVar", list(name = "--img-thumb-height", value = paste0(thumb_h, "px")))

    showNotification(
      "Image card size applied. Use Save all settings to keep it after restart.",
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE)



  # ---- QAPF helper functions (single-point plutonic / volcanic classification) ----
  `%||num||%` <- function(x, y) if (is.null(x) || is.na(x) || !is.finite(x)) y else x

  qapf_point_in_polygon <- function(x, y, px, py) {
    n <- length(px)
    inside <- FALSE
    j <- n
    for (i in seq_len(n)) {
      yi_gt <- py[i] > y
      yj_gt <- py[j] > y
      intersects <- (yi_gt != yj_gt) && (x < (px[j] - px[i]) * (y - py[i]) / ((py[j] - py[i]) + 1e-12) + px[i])
      if (intersects) inside <- !inside
      j <- i
    }
    inside
  }

  qapf_defs <- local({
    plutonic_labels <- c(
      "quartzolite", "quartz-rich granitoid", "alkali-feldspar granite", "syenogranite",
      "monzogranite", "granodiorite", "tonalite", "quartz alkali-feldspar syenite",
      "quartz syenite", "quartz monzonite", "quartz monzodiorite / quartz monzogabbro",
      "quartz diorite / quartz gabbro / quartz anorthosite", "alkali-feldspar syenite",
      "syenite", "monzonite", "monzodiorite / monzogabbro", "diorite / gabbro / anorthosite",
      "foid-bearing alkali-feldspar syenite", "foid-bearing syenite", "foid-bearing monzonite",
      "foid-bearing monzodiorite / foid-bearing monzogabbro",
      "foid-bearing diorite / foid-bearing gabbro / foid-bearing anorthosite",
      "foid syenite", "foid monzosyenite", "foid monzodiorite / foid monzogabbro",
      "foid diorite / foid gabbro", "foidolite"
    )
    plutonic_polys <- list(
      list(name=plutonic_labels[1], x=c(45,55,50), y=c(77.94229,77.94229,86.60254)),
      list(name=plutonic_labels[2], x=c(30,70,55,45), y=c(51.96152,51.96152,77.94229,77.94229)),
      list(name=plutonic_labels[3], x=c(10,18,34,30), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=plutonic_labels[4], x=c(18,38,44,34), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=plutonic_labels[5], x=c(38,62,56,44), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=plutonic_labels[6], x=c(62,82,66,56), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=plutonic_labels[7], x=c(82,90,70,66), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=plutonic_labels[8], x=c(2.5,12.0,18,10), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=plutonic_labels[9], x=c(12.00,35.75,38,18), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=plutonic_labels[10], x=c(35.75,64.25,62,38), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=plutonic_labels[11], x=c(64.25,88,82,62), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=plutonic_labels[12], x=c(88,97.5,90,82), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=plutonic_labels[13], x=c(0.,10.0,12.0,2.5), y=c(-0.000100,-0.000100,4.330127,4.330127)),
      list(name=plutonic_labels[14], x=c(10.00,35.00,35.75,12.00), y=c(-0.00010,-0.0001000,4.330127,4.330127)),
      list(name=plutonic_labels[15], x=c(35,65,64.25,35.75), y=c(-0.0001000,-0.0001000,4.330127,4.330127)),
      list(name=plutonic_labels[16], x=c(65.00,90.00,88.00,64.25), y=c(-0.0001000,-0.0001000,4.330127,4.330127)),
      list(name=plutonic_labels[17], x=c(90.0,100.0,97.5,88.0), y=c(-0.0001000,-0.0001000,4.330127,4.330127)),
      list(name=plutonic_labels[18], x=c(5,14,10,0), y=c(-8.660254,-8.660254,-0.0001000,-0.000100)),
      list(name=plutonic_labels[19], x=c(14.0,36.5,35,10), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=plutonic_labels[20], x=c(36.5,63.5,65.0,35.0), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=plutonic_labels[21], x=c(58.5,86.0,90.0,65.0), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=plutonic_labels[22], x=c(86,95,100,90), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=plutonic_labels[23], x=c(30,34,14,5), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=plutonic_labels[24], x=c(34,50,50,14), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=plutonic_labels[25], x=c(50,66,86,50), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=plutonic_labels[26], x=c(66,70,95,86), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=plutonic_labels[27], x=c(50,70,30), y=c(-86.60254,-51.96152,-51.96152))
    )

    volcanic_labels <- c(
      "alk-fsp. rhyolite", "rhyolite", "dacite", "q-alk-fsp trachyte", "quartz trachyte",
      "quartz latite", "basalt andesite", "alkali-feldspar trachyte", "trachyte", "latite",
      "foid-bearing alkali-feldspar trachyte", "foid-bearing trachyte", "foid-bearing latite",
      "phonolite", "tephritic phonolite", "phonolitic basanite/tephrite", "basanite/tephrite",
      "phonolitic foidite", "tephritic foidite", "foidite"
    )
    volcanic_polys <- list(
      list(name=volcanic_labels[1], x=c(30,70,50), y=c(51.96152,51.96152,86.60254)),
      list(name=volcanic_labels[2], x=c(18,62,56,34), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=volcanic_labels[3], x=c(62,90,70,56), y=c(17.32051,17.32051,51.96152,51.96152)),
      list(name=volcanic_labels[4], x=c(2.5,12.0,18,10), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=volcanic_labels[5], x=c(12.00,35.75,38,18), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=volcanic_labels[6], x=c(35.75,64.25,62,38), y=c(4.330127,4.330127,17.32051,17.32051)),
      list(name=volcanic_labels[7], x=c(63.5,95,100,90,62,65), y=c(-8.660254,-8.660254,0,17.32051,17.32051,0)),
      list(name=volcanic_labels[8], x=c(0.,10.0,12.0,2.5), y=c(-0.000100,-0.000100,4.330127,4.330127)),
      list(name=volcanic_labels[9], x=c(10.00,35.00,35.75,12.00), y=c(-0.00010,-0.0001000,4.330127,4.330127)),
      list(name=volcanic_labels[10], x=c(35,65,64.25,35.75), y=c(-0.0001000,-0.0001000,4.330127,4.330127)),
      list(name=volcanic_labels[11], x=c(5,14,10,0), y=c(-8.660254,-8.660254,-0.0001000,-0.000100)),
      list(name=volcanic_labels[12], x=c(14.0,36.5,35,10), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=volcanic_labels[13], x=c(36.5,63.5,65.0,35.0), y=c(-8.660254,-8.660254,-0.0001000,-0.0001000)),
      list(name=volcanic_labels[14], x=c(30,34,14,5), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=volcanic_labels[15], x=c(34,50,50,14), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=volcanic_labels[16], x=c(50,66,86,50), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=volcanic_labels[17], x=c(66,70,95,86), y=c(-51.961524,-51.961524,-8.660254,-8.660254)),
      list(name=volcanic_labels[18], x=c(45,50,50,30), y=c(-77.94229,-77.94229,-51.96152,-51.96152)),
      list(name=volcanic_labels[19], x=c(50,55,70,50), y=c(-77.94229,-77.94229,-51.96152,-51.96152)),
      list(name=volcanic_labels[20], x=c(50,55,45), y=c(-86.60254,-77.94229,-77.94229))
    )
    list(plutonic = list(polygons = plutonic_polys), volcanic = list(polygons = volcanic_polys))
  })

  qapf_prepare <- function(Q, A, P, F, diagram_type = "Plutonic") {
    vals <- c(Q = as.numeric(Q) %||num||% 0, A = as.numeric(A) %||num||% 0,
              P = as.numeric(P) %||num||% 0, F = as.numeric(F) %||num||% 0)
    if (any(!is.finite(vals))) stop("Q, A, P, F must be numeric values.")
    if (any(vals < 0)) stop("Q, A, P, F cannot be negative.")
    s <- sum(vals)
    if (!is.finite(s) || s <= 0) stop("At least one of Q, A, P, F must be > 0.")
    norm <- vals / s * 100

    if (norm[["Q"]] > 0 && norm[["F"]] > 0) {
      stop("QAPF input is ambiguous: use either quartz-bearing compositions (F = 0) or feldspathoid-bearing compositions (Q = 0 and F > 0).")
    }

    mode <- if (isTRUE(all.equal(norm[["F"]], 0))) {
      "upper"
    } else if (isTRUE(all.equal(norm[["Q"]], 0)) && norm[["F"]] > 0) {
      "lower"
    } else {
      stop("For this simplified tool, use F = 0 for the upper triangle, or Q = 0 with F > 0 for the lower triangle.")
    }

    bb <- norm[["Q"]] - norm[["F"]]
    x <- norm[["P"]] + abs(bb) / 2
    y <- sqrt(3) * bb / 2

    dtype <- tolower(trimws(diagram_type %||% "Plutonic"))
    if (!dtype %in% c("plutonic", "volcanic")) dtype <- "plutonic"
    polygons_use <- if (identical(mode, "upper")) {
      Filter(function(poly) max(poly$y) >= 0, qapf_defs[[dtype]]$polygons)
    } else {
      Filter(function(poly) min(poly$y) <= 0, qapf_defs[[dtype]]$polygons)
    }

    field <- NA_character_
    for (poly in polygons_use) {
      if (qapf_point_in_polygon(x, y, poly$x, poly$y)) {
        field <- poly$name
        break
      }
    }
    if (is.na(field)) field <- "boundary / outside labelled field"

    list(raw = vals, norm = norm, x = x, y = y, triangle = mode, field = field, diagram_type = tools::toTitleCase(dtype))
  }

  plot_qapf_single <- function(res, show_labels = TRUE) {
    stopifnot(!is.null(res))
    mode <- res$triangle %||% "upper"
    dtype <- tolower(res$diagram_type %||% "Plutonic")

    if (identical(mode, "upper")) {
      xlim <- c(-3, 103)
      ylim <- c(-3, 103)
      main_title <- if (dtype == "volcanic") "QAP upper triangle (volcanic rocks)" else "QAP upper triangle (plutonic rocks)"
    } else {
      xlim <- c(-3, 103)
      ylim <- c(-103, 3)
      main_title <- if (dtype == "volcanic") "QAPF lower triangle (volcanic rocks)" else "QAPF lower triangle (feldspathoid-bearing plutonic rocks)"
    }

    plot(NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", axes = FALSE, asp = 1, main = main_title)
    lines(c(0,100), c(0,0))

    if (identical(mode, "upper")) {
      lines(c(0,50), c(0,86.60254))
      lines(c(50,100), c(86.60254,0))
      if (dtype == "volcanic") {
        upper_lines <- list(
          list(c(30,70), c(51.96152,51.96152)), list(c(10,90), c(17.32051,17.32051)),
          list(c(2.5,64.25), c(4.330127,4.330127)), list(c(0,65), c(0,0)),
          list(c(10,34), c(0,51.96152)), list(c(35,38), c(0,17.32051)),
          list(c(38,44), c(17.32051,51.96152)), list(c(65,56), c(0,51.96152)),
          list(c(82,66), c(17.32051,51.96152))
        )
      } else {
        upper_lines <- list(
          list(c(45,55), c(77.94229,77.94229)), list(c(30,70), c(51.96152,51.96152)),
          list(c(10,90), c(17.32051,17.32051)), list(c(2.5,97.5), c(4.330127,4.330127)),
          list(c(10,34), c(0,51.96152)), list(c(35,38), c(0,17.32051)),
          list(c(38,44), c(17.32051,51.96152)), list(c(65,56), c(0,51.96152)),
          list(c(90,66), c(0,51.96152))
        )
      }
      for (i in seq_along(upper_lines)) {
        ln <- upper_lines[[i]]
        lines(ln[[1]], ln[[2]], lty = if ((dtype == "plutonic" && i == 7) || (dtype == "volcanic" && i %in% c(7,9))) 2 else 1)
      }
      text(-3, 0, "A", xpd = NA, cex = 1.2)
      text(103, 0, "P", xpd = NA, cex = 1.2)
      text(50, 91.5, "Q", xpd = NA, cex = 1.2)

      if (show_labels) {
        if (dtype == "volcanic") {
          text(22,35,"alk-fsp.
rhyolite", srt=60, cex=0.9)
          text(42,35,"rhyolite", cex=0.9)
          text(70,35,"dacite", cex=0.9)
          text(10,11,"q-alk-fsp
trachyte", adj=1, cex=0.9)
          text(27,11,"quartz
trachyte", cex=0.9)
          text(50,11,"quartz
latite", cex=0.9)
          text(77,9,"basalt
andesite", cex=0.9)
          text(7,2,"1", cex=0.9)
          text(22,2,"trachyte", cex=0.9)
          text(50,2,"latite", cex=0.9)
          text(1,98,"1 - alkali-feldspar
trachyte", adj=c(0,1), cex=0.9)
        } else {
          text(50,80,"quartzolite", cex=0.9)
          text(50,60,"quartz-rich
granitoid", cex=0.9)
          text(22,35,"alk-fsp
granite", srt=60, cex=0.9)
          text(34,35,"syeno-
granite", cex=0.9)
          text(50,35,"monzo-
granite", cex=0.9)
          text(66,35,"grano-
diorite", cex=0.9)
          text(78,35,"tonalite", srt=-60, cex=0.9)
          text(10,11,"q-alk-fsp
syenite", adj=1, cex=0.9)
          text(27,11,"quartz
syenite", cex=0.9)
          text(50,11,"quartz
monzonite", cex=0.9)
          text(73,11,"q-monzodiorite
q-monzogabbro", cex=0.9)
          text(88,11,"q-diorite
q-gabbro
q-anorthosite", adj=0, cex=0.9)
          text(7,2,"1", cex=0.9)
          text(22,2,"syenite", cex=0.9)
          text(50,2,"monzonite", cex=0.9)
          text(77,2,"2", cex=0.9)
          text(94,2,"3", cex=0.9)
          text(1,98,
               "1 - alkali-feldspar syenite
2 - monzodiorite, monzogabbro
3 - diorite, gabbro",
               adj=c(0,1), cex=0.9)
        }
      }
    } else {
      lines(c(0,50), c(0,-86.60254))
      lines(c(50,100), c(-86.60254,0))
      if (dtype == "volcanic") {
        lower_lines <- list(
          list(c(5,95), c(-8.660254,-8.660254)), list(c(30,70), c(-51.96152,-51.96152)),
          list(c(45,55), c(-77.94229,-77.94229)), list(c(10,34), c(0,-51.96152)),
          list(c(35,36.5), c(0,-8.660254)), list(c(65,63.5), c(0,-8.660254)),
          list(c(86,66), c(-8.660254,-51.96152)), list(c(50,50), c(-8.660254,-77.94229))
        )
      } else {
        lower_lines <- list(
          list(c(5,95), c(-8.660254,-8.660254)), list(c(30,70), c(-51.96152,-51.96152)),
          list(c(10,34), c(0,-51.96152)), list(c(35,36.5), c(0,-8.660254)),
          list(c(65,63.5), c(0,-8.660254)), list(c(90,66), c(0,-51.96152)),
          list(c(50,50), c(-8.660254,-51.96152))
        )
      }
      for (ln in lower_lines) lines(ln[[1]], ln[[2]])
      text(-3, 0, "A", xpd = NA, cex = 1.2)
      text(103, 0, "P", xpd = NA, cex = 1.2)
      text(50, -91.5, "F", xpd = NA, cex = 1.2)

      if (show_labels) {
        if (dtype == "volcanic") {
          text(7,-4,"4", cex=0.9)
          text(22,-4,"foid-bearing
trachyte", cex=0.9)
          text(50,-4,"foid-bearing
latite", cex=0.9)
          text(23,-35,"phonolite", srt=-60, cex=0.9)
          text(36,-20,"tephritic
phonolite", cex=0.9)
          text(65,-20,"phonolitic
basanite/tephrite", cex=0.9)
          text(79,-30,"basanite/
tephrite", srt=63, cex=0.9)
          text(49,-60,"phonolitic
foidite", adj=1, cex=0.9)
          text(51,-60,"tephritic
foidite", adj=0, cex=0.9)
          text(50,-80,"foidite", cex=0.9)
          text(-2,-70,"4 - foid-bearing
alkali-feldspar-trachyte", adj=c(0,0), cex=0.9)
        } else {
          text(7,-4,"4", cex=0.9)
          text(22,-4,"foid-bearing
syenite", cex=0.9)
          text(50,-4,"foid-bearing
monzonite", cex=0.9)
          text(78,-4,"5", cex=0.9)
          text(93,-4,"6", cex=0.9)
          text(23,-35,"foid-syenite", srt=-60, cex=0.9)
          text(37,-20,"foid-
monzosyenite", cex=0.9)
          text(63,-20,"foid-
monzodiorite
monzogabbro", cex=0.9)
          text(79,-30,"foid-diorite /
foid-gabbro", srt=63, cex=0.9)
          text(50,-60,"foidolite", cex=0.9)
          text(-2,-96,
               "4 - foid-bearing syenite
5 - foid-bearing monzodiorite, foid-bearing monzogabbro
6 - foid-bearing diorite, foid-bearing gabbro",
               adj=c(0,0), cex=0.9)
        }
      }
    }

    points(res$x, res$y, pch = 19, cex = 1.5)
    point_label <- trimws(res$point_label %||% "")
    if (!nzchar(point_label)) point_label <- "Sample"
    text(res$x, res$y, labels = paste0(" ", point_label), pos = 4, cex = 0.9)
  }

  export_qapf_png <- function(res, out_path, width = 1800, height = 1800, res_dpi = 200) {
    grDevices::png(filename = out_path, width = width, height = height, res = res_dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    par(mar = c(2.5, 2.5, 3, 2))
    plot_qapf_single(res, show_labels = TRUE)
    invisible(out_path)
  }

  qapf_state <- reactiveVal(NULL)

  observeEvent(input$about_program_btn, {
    showModal(modalDialog(
      title = "About Program",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        tags$h4("Make_Thin_Section"),
        tags$p(tags$b("Version: "), "62 - Mineral Reference redesign + editable web-resource hub"),
        tags$p( "Shiny console creating a convenient platform for thin section description. ",
                "Data can be exported to a Word document. Data are stored in an SQLite database."),
        tags$hr(),
        tags$p(tags$b("Author: "), "Sergei Abramov"),
        tags$p(tags$b("Email: "), "westabra@gmail.com"),
        tags$p(tags$b("QAPF acknowledgement: "),
               "Some helper functions used in the QAPF tab were adapted from GCDkit: ",
               "Janousek, V.; Farrow, C. M.; Erban, V. (2006). ",
               "Interpretation of whole-rock geochemical data in igneous geochemistry: introducing Geochemical Data Toolkit (GCDkit). ",
               tags$i("Journal of Petrology"), " 47(6): 1255-1259. doi: 10.1093/petrology/egl013."),
        #tags$p(paste0("2025 year - ", format(Sys.Date(), "%Y")))
        tags$p(paste0("2025 year - ", format(Sys.Date(), "%Y"), " year"))
      )
    ))
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$qapf_mode, {
    qapf_state(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$open_qapf_modal_btn, {
    cur <- qapf_state()
    rv <- if (is.null(cur)) c(Q = 0, A = 0, P = 0, F = 0) else cur$raw
    showModal(modalDialog(
      title = paste("Enter QAPF values -", input$qapf_mode %||% "Plutonic"),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("qapf_apply_btn", "Plot QAPF", class = "btn btn-primary")),
      fluidRow(
        column(3, numericInput("qapf_Q", "Q", value = unname(rv[["Q"]]), min = 0)),
        column(3, numericInput("qapf_A", "A", value = unname(rv[["A"]]), min = 0)),
        column(3, numericInput("qapf_P", "P", value = unname(rv[["P"]]), min = 0)),
        column(3, numericInput("qapf_F", "F", value = unname(rv[["F"]]), min = 0))
      ),
      tags$p(class = "small-note", paste("Mode:", input$qapf_mode %||% "Plutonic", "— enter raw modal values or proportions. The tool will normalize them to Q+A+P+F = 100."))
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$qapf_apply_btn, {
    tryCatch({
      res <- qapf_prepare(input$qapf_Q, input$qapf_A, input$qapf_P, input$qapf_F, diagram_type = input$qapf_mode %||% "Plutonic")
      sid_label <- trimws(input$sample_id %||% "")
      res$point_label <- if (nzchar(sid_label)) sid_label else "Sample"
      qapf_state(res)
      removeModal()
      updateTabsetPanel(session, "main_tabs", selected = "QAPF")
      showNotification(paste(res$diagram_type, "QAPF field:", res$field), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("QAPF error:", e$message), type = "error", duration = 6)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$clear_qapf_btn, {
    qapf_state(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$export_qapf_image_btn, {
    res <- qapf_state()
    if (is.null(res)) {
      showNotification("No QAPF diagram to export. Plot a point first.", type = "warning", duration = 5)
      return()
    }

    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    mode_tag <- if (tolower(res$diagram_type %||% "Plutonic") == "volcanic") "VOLC" else "PLUT"
    base_name <- paste0("QAPF_", mode_tag, "_", if (identical(res$triangle, "upper")) "QAP" else "FAP", "_", stamp)
    out_path <- file.path(img_dir, paste0(base_name, ".png"))

    tryCatch({
      export_qapf_png(res, out_path)
      meta <- read_img_meta(out_path)
      pool <- img_pool()
      next_order <- if (length(pool) == 0) 1 else (max(vapply(pool, function(x) as.numeric(x$order %||% 0), numeric(1))) + 1)
      new_id <- make_img_id()
      pool[[length(pool) + 1]] <- list(
        id = new_id,
        name = paste0(base_name, ".png"),
        ext = "png",
        url = paste0("petro_img/", basename(out_path)),
        path = out_path,
        is_converted = FALSE,
        origin = "qapf_export",
        order = next_order,
        meta = meta
      )
      img_pool(pool)

      sel <- img_choices()
      max_n <- as.integer(input$max_sel_images %||% 10)
      if (is.na(max_n) || max_n < 1) max_n <- 10
      sel <- c(sel, new_id)
      if (length(sel) > max_n) sel <- tail(sel, max_n)
      img_choices(sel)

      showNotification("QAPF diagram exported to Image container.", type = "message", duration = 5)
      updateTabsetPanel(session, "main_tabs", selected = "Image container (sorted)")
    }, error = function(e) {
      showNotification(paste("QAPF export error:", e$message), type = "error", duration = 6)
    })
  }, ignoreInit = TRUE)

  output$qapf_summary <- renderPrint({
    res <- qapf_state()
    if (is.null(res)) {
      cat("No QAPF point loaded. Click 'Enter QAPF values'.
")
    } else {
      cat(sprintf("Diagram type: %s

", res$diagram_type %||% "Plutonic"))
      cat("Raw input
")
      cat(sprintf("Q = %.4g, A = %.4g, P = %.4g, F = %.4g

", res$raw[["Q"]], res$raw[["A"]], res$raw[["P"]], res$raw[["F"]]))
      cat("Normalized to Q+A+P+F = 100
")
      cat(sprintf("Q = %.2f, A = %.2f, P = %.2f, F = %.2f
", res$norm[["Q"]], res$norm[["A"]], res$norm[["P"]], res$norm[["F"]]))
      cat(sprintf("Triangle: %s
", if (identical(res$triangle, "upper")) "Q-bearing (upper)" else "F-bearing (lower)"))
      cat(sprintf("Plot coordinates: x = %.2f, y = %.2f
", res$x, res$y))
      cat(sprintf("Field: %s
", res$field))
    }
  })

  output$qapf_plot <- renderPlot({
    res <- qapf_state()
    if (is.null(res)) {
      plot.new()
      title(sprintf("QAPF diagram (%s)", tolower(input$qapf_mode %||% "Plutonic")))
      text(0.5, 0.55, "No QAPF point loaded", cex = 1.2)
      text(0.5, 0.45, "Click 'Enter QAPF values' to plot a single sample", cex = 0.95)
    } else {
      plot_qapf_single(res, show_labels = TRUE)
    }
  })

  # ---- Coordinate calculator (PSAD56 / UTM 20N/21N ⇄ WGS84) ----
  # Note: This is a utility calculator only. The database stores only Latitude/Longitude (WGS84) as before.
  psad56_utm_to_wgs84 <- function(E, N, zone) {
    stopifnot(zone %in% c(20, 21))
    epsg_in <- if (zone == 20) 24820 else 24821
    pts <- st_as_sf(
      data.frame(E = as.numeric(E), N = as.numeric(N)),
      coords = c("E", "N"),
      crs = epsg_in
    )
    pts_wgs <- st_transform(pts, 4326)
    xy <- st_coordinates(pts_wgs)
    list(lon = unname(xy[1, "X"]), lat = unname(xy[1, "Y"]))
  }

  wgs84_to_psad56_utm <- function(lon, lat, zone) {
    stopifnot(zone %in% c(20, 21))
    epsg_out <- if (zone == 20) 24820 else 24821
    pts <- st_as_sf(
      data.frame(lon = as.numeric(lon), lat = as.numeric(lat)),
      coords = c("lon", "lat"),
      crs = 4326
    )
    pts_utm <- st_transform(pts, epsg_out)
    xy <- st_coordinates(pts_utm)
    list(E = unname(xy[1, "X"]), N = unname(xy[1, "Y"]), epsg = epsg_out)
  }

  # Store last calculator results (so "Insert" works even after recompute)
  calc_last_wgs <- reactiveVal(NULL)
  calc_last_utm <- reactiveVal(NULL)

  observeEvent(input$coord_calc_btn, {
    showModal(modalDialog(
      title = "Coordinate calculator (PSAD56 / UTM 20N/21N ⇄ WGS84)",
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        modalButton("Close")
      ),
      tabsetPanel(
        
        tabPanel(
          "PSAD56 / UTM → WGS84",
          fluidRow(
            column(4, selectInput("calc_zone_utm", "UTM zone", choices = c("20N (EPSG:24820)" = 20, "21N (EPSG:24821)" = 21), selected = 21)),
            column(4, numericInput("calc_E", "Easting (X), m", value = NA)),
            column(4, numericInput("calc_N", "Northing (Y), m", value = NA))
          ),
          actionButton("calc_run_utm_to_wgs", "Convert", icon = icon("arrow-right")),
          tags$hr(),
          verbatimTextOutput("calc_out_wgs"),
          actionButton("calc_insert_wgs_to_form", "Insert result into form (Latitude/Longitude)", icon = icon("paste"))
        ),
        tabPanel(
          "WGS84 → PSAD56 / UTM",
          fluidRow(
            column(4, selectInput("calc_zone_wgs", "UTM zone", choices = c("20N (EPSG:24820)" = 20, "21N (EPSG:24821)" = 21), selected = 21)),
            column(4, numericInput("calc_lon", "Longitude (WGS84)", value = NA)),
            column(4, numericInput("calc_lat", "Latitude (WGS84)", value = NA))
          ),
          actionButton("calc_run_wgs_to_utm", "Convert", icon = icon("arrow-right")),
          tags$hr(),
          verbatimTextOutput("calc_out_utm")
        )
      )
    ))
  })

  observeEvent(input$calc_run_utm_to_wgs, {
    req(input$calc_E, input$calc_N, input$calc_zone_utm)
    res <- psad56_utm_to_wgs84(input$calc_E, input$calc_N, as.integer(input$calc_zone_utm))
    calc_last_wgs(res)
  })

  observeEvent(input$calc_run_wgs_to_utm, {
    req(input$calc_lon, input$calc_lat, input$calc_zone_wgs)
    res <- wgs84_to_psad56_utm(input$calc_lon, input$calc_lat, as.integer(input$calc_zone_wgs))
    calc_last_utm(res)
  })

  output$calc_out_wgs <- renderPrint({
    res <- calc_last_wgs()
    if (is.null(res)) {
      cat("Enter Easting/Northing and press Convert.\n")
    } else {
      cat("WGS84 (EPSG:4326)\n")
      cat("Longitude:", format(round(res$lon, 7), nsmall = 7), "\n")
      cat("Latitude :", format(round(res$lat, 7), nsmall = 7), "\n")
    }
  })

  output$calc_out_utm <- renderPrint({
    res <- calc_last_utm()
    if (is.null(res)) {
      cat("Enter Longitude/Latitude and press Convert.\n")
    } else {
      cat("PSAD56 / UTM\n")
      cat("Zone EPSG:", res$epsg, "\n")
      cat("Easting (X):", format(round(res$E, 3), nsmall = 3), "\n")
      cat("Northing(Y):", format(round(res$N, 3), nsmall = 3), "\n")
    }
  })

  observeEvent(input$calc_insert_wgs_to_form, {
    res <- calc_last_wgs()
    req(res)
    updateTextInput(session, "lat", value = as.character(round(res$lat, 7)))
    updateTextInput(session, "lon", value = as.character(round(res$lon, 7)))
    removeModal()
  })


# ---------------------------
# Form lifecycle helpers
# ---------------------------

last_sample_snapshot <- reactiveVal(NULL)

snapshot_current_form <- function() {
  list(
    project_name     = isolate(input$project_name %||% ""),
    author           = isolate(input$author %||% ""),
    sample_id        = isolate(input$sample_id %||% ""),
    field_rock_name  = isolate(input$field_rock_name %||% ""),
    field_desc       = isolate(input$field_desc %||% ""),
    lat              = isolate(input$lat %||% ""),
    lon              = isolate(input$lon %||% ""),
    rock_name        = isolate(input$rock_name %||% ""),
    grain_size       = isolate(input$grain_size %||% ""),
    texture          = isolate(input$texture %||% ""),
    rock_desc        = isolate(input$rock_desc %||% ""),
    mapping_classifier = isolate(input$mapping_classifier %||% ""),
    suggested_mapping_unit_id = isolate(input$suggested_mapping_unit_id %||% ""),
    mapping_unit_confidence = isolate(input$mapping_unit_confidence %||% ""),
    mapping_unit_comment = isolate(mapping_unit_comment_rv() %||% ""),
    minerals_df      = isolate(minerals()),
    texture_components_df = isolate(texture_components()),
    images_keep      = FALSE
  )
}

reset_form_common <- function(keep_project_author = TRUE, set_defaults = TRUE) {
  # snapshot before clearing (for Clone)
  last_sample_snapshot(snapshot_current_form())

  defs <- tryCatch(get_defaults_from_db(), error = function(e) list(author="", project="", confirm_load=TRUE, panel_bg="#ffffff", docx_font_size="11"))
  def_author  <- defs$author %||% ""
  def_project <- defs$project %||% ""

  if (!keep_project_author) {
    if (set_defaults) {
      if (nzchar(def_project)) {
        updateSelectizeInput(session, "project_name", selected = def_project, server = TRUE)
      } else {
        updateSelectizeInput(session, "project_name", selected = NULL, server = TRUE)
      }
      updateTextInput(session, "author", value = def_author)
    } else {
      updateSelectizeInput(session, "project_name", selected = NULL, server = TRUE)
      updateTextInput(session, "author", value = "")
    }
  }

  # Clear sample-specific fields
  updateTextInput(session, "sample_id", value = "")
  updateTextInput(session, "field_rock_name", value = "")
  updateTextAreaInput(session, "field_desc", value = "")
  updateTextInput(session, "lat", value = "")
  updateTextInput(session, "lon", value = "")
  updateTextInput(session, "rock_name", value = "")
  updateSelectizeInput(session, "grain_size", selected = NULL, server = TRUE)
  updateSelectizeInput(session, "texture", selected = NULL, server = TRUE)
  updateTextAreaInput(session, "rock_desc", value = "")
  updateSelectizeInput(session, "suggested_mapping_unit_id", selected = "", server = FALSE)
  updateSelectInput(session, "mapping_unit_confidence", selected = "")
  mapping_unit_comment_rv("")

  minerals(data.frame(
    `Mineral` = character(0),
    `Vol. %` = character(0),
    `Morphology, features` = character(0),
    `Genesis` = character(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))

  texture_components(empty_texture_components_df())

  img_pool(list())
  img_choices(character(0))
  loaded_caps(list())
  try(clear_img_dir(), silent = TRUE)
}

observeEvent(input$new_sample_btn, {
  reset_form_common(keep_project_author = TRUE, set_defaults = FALSE)
  schedule_reference_reload(
    selection_override = list(
      grain_size = "",
      texture = "",
      suggested_mapping_unit_id = ""
    )
  )
  showNotification("New sample: cleared fields (Project/Author kept).", type = "message")
})

observeEvent(input$add_new_sample_description_btn, {
  reset_form_common(keep_project_author = TRUE, set_defaults = FALSE)
  schedule_reference_reload(
    selection_override = list(
      grain_size = "",
      texture = "",
      suggested_mapping_unit_id = ""
    )
  )
  updateTabsetPanel(session, "main_tabs", selected = "Rock description")
  session$sendCustomMessage("focus_sample_id", list())
  showNotification(
    "Enter the new number in Sample ID. When the description is ready, click Save sample to SQLite.",
    type = "message",
    duration = 8
  )
})

observeEvent(input$reset_form_btn, {
  reset_form_common(keep_project_author = FALSE, set_defaults = TRUE)
  schedule_reference_reload(
    selection_override = list(
      grain_size = "",
      texture = "",
      suggested_mapping_unit_id = ""
    )
  )
  showNotification("Form reset to defaults.", type = "message")
})

observeEvent(input$clone_prev_btn, {
  snap <- last_sample_snapshot()
  if (is.null(snap)) {
    showNotification("Nothing to clone yet. Load or work on a sample first.", type = "error")
    return()
  }

  cur_proj <- isolate(input$project_name %||% "")
  cur_auth <- isolate(input$author %||% "")

  if (!nzchar(cur_proj) && nzchar(snap$project_name)) {
    updateSelectizeInput(session, "project_name", selected = snap$project_name, server = TRUE)
  }
  if (!nzchar(cur_auth) && nzchar(snap$author)) {
    updateTextInput(session, "author", value = snap$author)
  }

  updateTextInput(session, "sample_id", value = "")
  updateTextInput(session, "field_rock_name", value = snap$field_rock_name %||% "")
  updateTextAreaInput(session, "field_desc", value = snap$field_desc %||% "")
  updateTextInput(session, "lat", value = snap$lat %||% "")
  updateTextInput(session, "lon", value = snap$lon %||% "")
  updateTextInput(session, "rock_name", value = snap$rock_name %||% "")

  g_val <- to_title_case(trimws(snap$grain_size %||% ""))
  t_val <- to_title_case(trimws(snap$texture %||% ""))

  gg2 <- grains_db()
  if (!is.null(gg2) && "grainsize_title" %in% names(gg2)) {
    gg2$Size_Range <- if ("Size_Range" %in% names(gg2)) trimws(gg2$Size_Range %||% "") else ""
    g_choices <- sort(unique(gg2$grainsize_title))
    g_tt <- gg2$Size_Range
    names(g_tt) <- gg2$grainsize_title
    g_tt <- g_tt[!duplicated(names(g_tt))]
    g_tt_js <- js_obj_from_named(g_tt)
    g_render <- I(paste0(
      "{",
      "option:function(item,escape){var tt=", g_tt_js, ";",
      "var t=tt[item.value]||''; var title=t?('Size range: '+t):'';",
      "return '<div title=\"'+escape(title)+'\">'+escape(item.text)+'</div>';},",
      "item:function(item,escape){var tt=", g_tt_js, ";",
      "var t=tt[item.value]||''; var title=t?('Size range: '+t):'';",
      "return '<div title=\"'+escape(title)+'\">'+escape(item.text)+'</div>';}",
      "}"
    ))
    if (nzchar(g_val) && !(g_val %in% g_choices)) g_choices <- unique(c(g_choices, g_val))
    updateSelectizeInput(
      session, "grain_size",
      choices = g_choices,
      selected = if (nzchar(g_val)) g_val else NULL,
      server = TRUE,
      options = list(render = g_render, create = TRUE)
    )
  } else {
    # fallback
    g_choices <- if (nzchar(g_val)) g_val else character(0)
    updateSelectizeInput(session, "grain_size", choices = g_choices, selected = if (nzchar(g_val)) g_val else NULL, server = TRUE)
  }

  tt2 <- textures_db()
  if (!is.null(tt2) && "texture_title" %in% names(tt2)) {
    tt2$Explanation <- if ("Explanation" %in% names(tt2)) trimws(tt2$Explanation %||% "") else ""
    t_choices <- sort(unique(tt2$texture_title))
    t_tt <- tt2$Explanation
    names(t_tt) <- tt2$texture_title
    t_tt <- t_tt[!duplicated(names(t_tt))]
    t_tt_js <- js_obj_from_named(t_tt)
    t_render <- I(paste0(
      "{",
      "option:function(item,escape){var tt=", t_tt_js, ";",
      "var t=tt[item.value]||''; var title=t?('Explanation: '+t):'';",
      "return '<div title=\"'+escape(title)+'\">'+escape(item.text)+'</div>';},",
      "item:function(item,escape){var tt=", t_tt_js, ";",
      "var t=tt[item.value]||''; var title=t?('Explanation: '+t):'';",
      "return '<div title=\"'+escape(title)+'\">'+escape(item.text)+'</div>';}",
      "}"
    ))
    if (nzchar(t_val) && !(t_val %in% t_choices)) t_choices <- unique(c(t_choices, t_val))
    updateSelectizeInput(
      session, "texture",
      choices = t_choices,
      selected = if (nzchar(t_val)) t_val else NULL,
      server = TRUE,
      options = list(render = t_render, create = TRUE)
    )
  } else {
    t_choices <- if (nzchar(t_val)) t_val else character(0)
    updateSelectizeInput(session, "texture", choices = t_choices, selected = if (nzchar(t_val)) t_val else NULL, server = TRUE)
  }

  updateTextAreaInput(session, "rock_desc", value = snap$rock_desc %||% "")

  if (nzchar(trimws(snap$mapping_classifier %||% ""))) {
    updateSelectInput(session, "mapping_classifier", selected = snap$mapping_classifier)
    refresh_mapping_unit_choices(
      selected_unit_id = snap$suggested_mapping_unit_id %||% "",
      classifier_override = snap$mapping_classifier %||% ""
    )
  } else {
    updateSelectizeInput(session, "suggested_mapping_unit_id", selected = snap$suggested_mapping_unit_id %||% "", server = FALSE)
  }
  updateSelectInput(session, "mapping_unit_confidence", selected = snap$mapping_unit_confidence %||% "")
  mapping_unit_comment_rv(snap$mapping_unit_comment %||% "")

  if (!is.null(snap$minerals_df) && is.data.frame(snap$minerals_df)) {
    minerals(snap$minerals_df)
  }

  if (!is.null(snap$texture_components_df) && is.data.frame(snap$texture_components_df)) {
    texture_components(snap$texture_components_df)
  } else {
    texture_components(empty_texture_components_df())
  }

  img_pool(list())
  img_choices(character(0))
  loaded_caps(list())
  try(clear_img_dir(), silent = TRUE)

  schedule_reference_reload(
    selection_override = list(
      grain_size = g_val,
      texture = t_val,
      mapping_classifier = snap$mapping_classifier %||% "",
      suggested_mapping_unit_id = snap$suggested_mapping_unit_id %||% ""
    )
  )

  showNotification("Cloned previous sample fields (Sample ID cleared; images not cloned).", type = "message")
})


# =========================
# SQLite init + settings
# =========================
ensure_db_dirs()
local({
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
})
# Keep Project selector populated from DB
observe({
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  projs <- list_projects(con)
  cur <- isolate(input$project_name %||% "")
  if (nzchar(cur) && !(cur %in% projs)) projs <- unique(c(projs, cur))
  # Update choices ONLY (do not force selection). This prevents the selector from snapping
  # back to the first item (often the Default Project) during UI refreshes after loading a sample.
  updateSelectizeInput(session, "project_name", choices = projs, server = TRUE)
  default_project <- isolate(input$project_default %||% "")
  default_choices <- unique(c(projs, default_project))
  default_choices <- default_choices[nzchar(trimws(default_choices))]
  updateSelectizeInput(
    session,
    "project_default",
    choices = default_choices,
    selected = default_project,
    server = TRUE
  )
  updateSelectizeInput(session, "map_overlay_projects", choices = projs,
                       selected = isolate(input$map_overlay_projects %||% character(0)), server = TRUE)
})

refresh_sample_list <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  projs <- list_projects(con)
  updateSelectInput(session, "load_project_name", choices = projs,
                    selected = isolate(input$load_project_name %||% if (length(projs) > 0) projs[1] else ""))
  updateSelectInput(session, "map_project_name", choices = projs,
                    selected = isolate(input$map_project_name %||% if (length(projs) > 0) projs[1] else ""))
  updateSelectizeInput(session, "map_overlay_projects", choices = projs,
                       selected = isolate(input$map_overlay_projects %||% character(0)), server = TRUE)
  default_project <- isolate(input$project_default %||% "")
  default_choices <- unique(c(projs, default_project))
  default_choices <- default_choices[nzchar(trimws(default_choices))]
  updateSelectizeInput(
    session,
    "project_default",
    choices = default_choices,
    selected = default_project,
    server = TRUE
  )
  sel_proj <- isolate(input$load_project_name %||% if (length(projs) > 0) projs[1] else "")
  sids <- if (nzchar(sel_proj)) list_samples_in_project(con, sel_proj) else character(0)
  updateSelectInput(session, "load_sample_id", choices = sids,
                    selected = isolate(input$load_sample_id %||% if (length(sids) > 0) sids[length(sids)] else ""))
}
# Load settings from DB at startup
observeEvent(TRUE, {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  a <- settings_get(con, "author", "")
  p <- settings_get(con, "project", "")
  cfl <- settings_get(con, "confirm_load", "TRUE")
  bg <- settings_get(con, "panel_bg", settings_get(con, "panel_color", "#ffffff"))
  bg <- normalize_hex_color(bg, "#ffffff")
  fs <- settings_get(con, "docx_font_size", "10")

ms <- settings_get(con, "max_sel_images", "10")
icw <- settings_get(con, "img_card_width", "280")
ith <- settings_get(con, "img_thumb_height", "170")

mb <- settings_get(con, "max_image_mb", "4")
md <- settings_get(con, "max_image_dim", "2500")
qd <- settings_get(con, "jpeg_quality_default", "85")
qm <- settings_get(con, "jpeg_quality_min", "60")
tuif <- settings_get(con, "track_unused_image_files", "FALSE")
  if (nzchar(a) && !nzchar(input$author %||% "")) updateTextInput(session, "author", value = a)
  if (nzchar(p) && !nzchar(input$project_name %||% "")) updateSelectizeInput(session, "project_name", choices = unique(c(list_projects(con), p)), selected = p, server = TRUE)
  loaded_settings <- list(
    author = a,
    project = p,
    confirm_load = tolower(cfl) %in% c("true", "1", "yes", "y"),
    panel_bg = bg,
    docx_font_size = numeric_setting_text(fs, 10),
    max_sel_images = numeric_setting_text(ms, 10),
    img_card_width = numeric_setting_text(icw, 280),
    img_thumb_height = numeric_setting_text(ith, 170),
    max_image_mb = numeric_setting_text(mb, 4),
    max_image_dim = numeric_setting_text(md, 2500),
    jpeg_quality_default = numeric_setting_text(qd, 85),
    jpeg_quality_min = numeric_setting_text(qm, 60),
    track_unused_image_files = tolower(tuif) %in% c("true", "1", "yes", "y")
  )
  apply_settings_state_to_ui(loaded_settings)
  saved_settings_state(loaded_settings)
  refresh_sample_list()
observeEvent(input$load_project_name, {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  proj <- input$load_project_name %||% ""
  sids <- if (nzchar(proj)) list_samples_in_project(con, proj) else character(0)
  updateSelectInput(session, "load_sample_id", choices = sids,
                    selected = if (length(sids) > 0) sids[length(sids)] else "")
}, ignoreInit = TRUE)
}, once = TRUE)

safe_download_project_name <- function(project_name) {
  out <- gsub("[\\\\/:*?\"<>|]", "_", trimws(project_name %||% ""))
  out <- gsub("[[:space:]]+", "_", out)
  out <- gsub("_+", "_", out)
  out <- sub("^[._]+", "", out)
  out <- sub("[._]+$", "", out)
  if (!nzchar(out)) out <- "project"
  substr(out, 1, 120)
}

output$download_petro_summary <- downloadHandler(
  filename = function() {
    paste0(
      safe_download_project_name(input$load_project_name %||% ""),
      "_petrographic_summary_v2_3.xlsx"
    )
  },
  content = function(file) {
    project_name <- trimws(input$load_project_name %||% "")
    if (!nzchar(project_name)) stop("Select a project before exporting the report.")

    abbr_path <- file.path(getwd(), "White et. al. Abbreviations for names of rock-forming minerals.csv")
    required_report_packages <- c("DBI", "RSQLite", "dplyr", "stringr", "readr", "openxlsx", "tibble")
    missing_report_packages <- required_report_packages[
      !vapply(required_report_packages, requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_report_packages) > 0) {
      stop(
        "Missing report packages: ", paste(missing_report_packages, collapse = ", "),
        ". Run source(\"install_main_dependencies.R\") first."
      )
    }
    if (!file.exists(abbr_path)) {
      stop("Mineral abbreviation file not found: ", abbr_path)
    }

    withProgress(
      message = paste("Creating petrographic summary for", project_name),
      value = 0,
      min = 0,
      max = 1,
      {
        PETROGRAPHIC_SUMMARY_EXPORTER(
          db_path = DB_PATH,
          target_project = project_name,
          output_dir = dirname(file),
          abbr_path = abbr_path,
          output_file = file,
          progress_callback = function(value, detail) {
            setProgress(value = value, detail = detail)
          }
        )
      }
    )
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

output$db_info <- renderText({
  input$refresh_db_info_btn
  db_counts <- tryCatch({
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    n_projects <- as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM projects")$n[1])
    n_samples  <- as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM samples")$n[1])

    list(
      projects = n_projects,
      samples = n_samples
    )
  }, error = function(e) {
    list(
      projects = NA_integer_,
      samples = NA_integer_
    )
  })

  paste0(
    "DB_PATH: ", DB_PATH, "
",
    "Images dir: ", DB_IMAGES_DIR, "
",
    "Working dir: ", getwd(), "
",
    "Number of projects: ", ifelse(is.na(db_counts$projects), "not available", db_counts$projects), "
",
    "Number of records - descriptions of rock samples under the microscope: ", ifelse(is.na(db_counts$samples), "not available", db_counts$samples)
  )
})

  build_project_map_df <- function(project_name) {
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    proj_nm <- trimws(project_name %||% "")
    if (!nzchar(proj_nm)) return(data.frame())
    pid <- get_project_id(con, proj_nm)
    if (is.na(pid)) return(data.frame())

    df <- tryCatch(
      dbGetQuery(con, "SELECT sample_id, author, field_rock_name, field_desc, lat, lon, rock_name, grain_size, texture, rock_desc FROM samples WHERE project_id = :pid ORDER BY sample_id",
                 params = list(pid = pid)),
      error = function(e) data.frame()
    )
    if (is.null(df) || nrow(df) == 0) return(data.frame())

    df$project_name <- proj_nm

    # V56 GIS texture logic:
    # - samples.texture remains the Primary texture (old 4b field);
    # - sample_textures may contain several recognized texture components;
    # - GIS popups/tables/export now expose two user-facing fields:
    #     Primary texture and Recognized textures.
    #   If the new table does not exist or has no rows for a sample,
    #   Recognized textures is empty and the old 4b field remains available
    #   as Primary texture.
    df$primary_texture <- as.character(df$texture %||% "")
    df$recognized_textures <- ""
    df$texture <- as.character(df$texture %||% "")

    has_sample_textures <- tryCatch({
      tab <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'sample_textures'")
      !is.null(tab) && nrow(tab) > 0
    }, error = function(e) FALSE)

    if (isTRUE(has_sample_textures)) {
      tdf <- tryCatch(
        DBI::dbGetQuery(con, "
          SELECT sample_id, row_order, TRIM(texture) AS texture
          FROM sample_textures
          WHERE project_id = :pid
            AND texture IS NOT NULL
            AND TRIM(texture) <> ''
          ORDER BY sample_id, COALESCE(row_order, 999999), id
        ", params = list(pid = pid)),
        error = function(e) data.frame()
      )

      if (!is.null(tdf) && nrow(tdf) > 0) {
        make_unique_texture_list <- function(primary, components) {
          vals <- trimws(as.character(c(primary, components)))
          vals <- vals[!is.na(vals) & nzchar(vals)]
          if (length(vals) == 0) return("")
          keep <- !duplicated(tolower(vals))
          paste(vals[keep], collapse = "; ")
        }
        make_component_list <- function(components) {
          vals <- trimws(as.character(components))
          vals <- vals[!is.na(vals) & nzchar(vals)]
          if (length(vals) == 0) return("")
          keep <- !duplicated(tolower(vals))
          paste(vals[keep], collapse = "; ")
        }

        comp_by_sample <- split(tdf$texture, as.character(tdf$sample_id))
        df$recognized_textures <- vapply(as.character(df$sample_id), function(sid) {
          make_component_list(comp_by_sample[[sid]] %||% character(0))
        }, character(1))
        df$texture <- vapply(seq_len(nrow(df)), function(i) {
          comps <- comp_by_sample[[as.character(df$sample_id[i])]] %||% character(0)
          make_unique_texture_list(df$primary_texture[i], comps)
        }, character(1))
      }
    }

    df$lat_num <- vapply(df$lat, parse_coord_text, numeric(1))
    df$lon_num <- vapply(df$lon, parse_coord_text, numeric(1))
    df <- df[is.finite(df$lat_num) & is.finite(df$lon_num), , drop = FALSE]
    if (nrow(df) == 0) return(data.frame())

    make_popup <- function(row) {
      fmt <- function(label, value, max_chars = 350) {
        value <- clip_text(value, max_chars)
        if (!nzchar(value)) return(NULL)
        paste0("<b>", htmltools::htmlEscape(label), ":</b> ", htmltools::htmlEscape(value))
      }
      bits <- Filter(Negate(is.null), list(
        fmt("Sample ID", row[["sample_id"]], 350),
        fmt("Project", row[["project_name"]], 350),
        fmt("Author", row[["author"]], 350),
        fmt("Field Rock Name", row[["field_rock_name"]], 350),
        fmt("Rock name", row[["rock_name"]], 350),
        fmt("Grain size", row[["grain_size"]], 350),
        fmt("Primary texture", row[["primary_texture"]], 350),
        fmt("Recognized textures", row[["recognized_textures"]], 350),
        fmt("Latitude", row[["lat"]], 350),
        fmt("Longitude", row[["lon"]], 350),
        fmt("Field description", row[["field_desc"]], 450),
        fmt("Rock description", row[["rock_desc"]], 600)
      ))
      paste(bits, collapse = "<br/>")
    }

    df$popup_html <- vapply(seq_len(nrow(df)), function(i) make_popup(df[i, , drop = FALSE]), character(1))
    df$label_text <- ifelse(nzchar(trimws(df$sample_id %||% "")), trimws(df$sample_id), "Sample")
    df
  }

  map_project_palette <- function(projects) {
    projects <- unique(as.character(projects %||% character(0)))
    projects <- projects[!is.na(projects) & nzchar(trimws(projects))]
    if (length(projects) == 0) return(character(0))

    # Stable project colors. The palette is deliberately longer than the old
    # 8-color set because the All projects overview can contain many projects.
    base_cols <- c(
      "#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b",
      "#e377c2", "#17becf", "#bcbd22", "#7f7f7f", "#aec7e8", "#ff9896",
      "#98df8a", "#ffbb78", "#c5b0d5", "#c49c94", "#f7b6d2", "#9edae5",
      "#dbdb8d", "#c7c7c7"
    )
    out <- base_cols[((seq_along(projects) - 1) %% length(base_cols)) + 1]
    names(out) <- projects
    out
  }

  project_map_df <- reactiveVal(data.frame())

  build_project_map_df_multi <- function(main_project, overlay_projects = character(0)) {
    main_project <- trimws(main_project %||% "")
    overlay_projects <- trimws(as.character(overlay_projects %||% character(0)))
    overlay_projects <- overlay_projects[nzchar(overlay_projects)]
    overlay_projects <- setdiff(overlay_projects, main_project)
    projects <- unique(c(main_project, overlay_projects))
    projects <- projects[nzchar(projects)]
    if (length(projects) == 0) return(data.frame())

    parts <- lapply(projects, function(pn) {
      one <- build_project_map_df(pn)
      if (!is.null(one) && nrow(one) > 0) {
        one$map_role <- if (identical(pn, main_project)) "Main Project" else "Overlay Project"
      }
      one
    })
    parts <- parts[!vapply(parts, function(x) is.null(x) || nrow(x) == 0, logical(1))]
    if (length(parts) == 0) return(data.frame())
    do.call(rbind, parts)
  }

  build_project_map_df_all_projects <- function() {
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)
    projects <- list_projects(con)
    projects <- projects[!is.na(projects) & nzchar(trimws(projects))]
    if (length(projects) == 0) return(data.frame())

    parts <- lapply(projects, function(pn) {
      one <- build_project_map_df(pn)
      if (!is.null(one) && nrow(one) > 0) {
        one$map_role <- "All Projects Overview"
      }
      one
    })
    parts <- parts[!vapply(parts, function(x) is.null(x) || nrow(x) == 0, logical(1))]
    if (length(parts) == 0) return(data.frame())
    do.call(rbind, parts)
  }

  build_project_map_df_current <- function() {
    mode <- input$project_map_mode %||% "main_only"
    main_project <- input$map_project_name %||% ""

    if (identical(mode, "all_projects")) {
      return(build_project_map_df_all_projects())
    }

    overlay_projects <- if (identical(mode, "main_overlay")) {
      input$map_overlay_projects %||% character(0)
    } else {
      character(0)
    }

    build_project_map_df_multi(main_project, overlay_projects)
  }

  observeEvent(input$load_project_map_btn, {
    mode <- input$project_map_mode %||% "main_only"
    df <- build_project_map_df_current()
    project_map_df(df)
    if (nrow(df) == 0) {
      showNotification("No valid coordinates were found for the selected map mode/project set.", type = "warning", duration = 5)
    } else {
      n_proj <- length(unique(df$project_name))
      msg_mode <- switch(mode,
                         "all_projects" = "All projects overview",
                         "main_overlay" = "Main + overlay projects",
                         "main_only" = "Main project only",
                         "Project map")
      showNotification(paste(msg_mode, ": loaded", nrow(df), "point(s) from", n_proj, "project(s). GIS layer export remains Main Project only."), type = "message", duration = 6)
    }
  }, ignoreInit = TRUE)

  output$project_map <- renderLeaflet({
    leaflet() %>% addTiles()
  })

  observe({
    df <- project_map_df()
    proxy <- leafletProxy("project_map")
    proxy %>% clearMarkers() %>% clearControls() %>% clearShapes()

    if (is.null(df) || nrow(df) == 0) {
      proxy %>% setView(lng = -58.5, lat = 5.0, zoom = 6)
    } else {
      projects <- unique(as.character(df$project_name))
      pal_values <- unname(map_project_palette(projects))
      pal <- leaflet::colorFactor(palette = pal_values, domain = projects)

      for (proj_nm in projects) {
        sub <- df[df$project_name == proj_nm, , drop = FALSE]
        proxy <- proxy %>% addCircleMarkers(
          data = sub,
          lng = ~lon_num,
          lat = ~lat_num,
          radius = ifelse(sub$map_role == "Main Project", 7, 6),
          stroke = TRUE,
          weight = ifelse(sub$map_role == "Main Project", 2, 1),
          color = ~pal(project_name),
          fillColor = ~pal(project_name),
          fillOpacity = 0.85,
          label = ~label_text,
          popup = lapply(sub$popup_html, htmltools::HTML),
          group = proj_nm
        )
      }

      proxy %>% addLegend(
        position = "bottomright",
        pal = pal,
        values = projects,
        title = "Projects",
        opacity = 1
      ) %>% addLayersControl(
        overlayGroups = projects,
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>% fitBounds(lng1 = min(df$lon_num, na.rm = TRUE), lat1 = min(df$lat_num, na.rm = TRUE),
                      lng2 = max(df$lon_num, na.rm = TRUE), lat2 = max(df$lat_num, na.rm = TRUE))
    }
  })

  output$project_map_table <- renderDT({
    df <- project_map_df()
    if (is.null(df) || nrow(df) == 0) return(datatable(data.frame(Note = "No project points loaded."), options = list(dom = "t"), rownames = FALSE))
    if (!"map_role" %in% names(df)) df$map_role <- "Main Project"
    out <- df[, c("map_role", "sample_id", "project_name", "author", "field_rock_name", "rock_name", "grain_size", "primary_texture", "recognized_textures", "lat", "lon", "field_desc", "rock_desc"), drop = FALSE]
    names(out) <- c("Map role", "Sample ID", "Project", "Author", "Field Rock Name", "Rock name", "Grain size", "Primary texture", "Recognized textures", "Latitude", "Longitude", "Field description", "Rock description")
    datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  })


  # ---- Static project map export for reports / manuscripts ----
  get_static_map_limits <- function(df, buffer_pct = 8) {
    lon <- df$lon_num
    lat <- df$lat_num

    lon_rng <- range(lon, na.rm = TRUE)
    lat_rng <- range(lat, na.rm = TRUE)

    lon_span <- diff(lon_rng)
    lat_span <- diff(lat_rng)

    # If all points have the same coordinate, create a small visible frame.
    if (!is.finite(lon_span) || lon_span <= 0) lon_span <- 0.02
    if (!is.finite(lat_span) || lat_span <= 0) lat_span <- 0.02

    buf <- suppressWarnings(as.numeric(buffer_pct %||% 8))
    if (is.na(buf) || buf < 0) buf <- 8

    lon_pad <- lon_span * buf / 100
    lat_pad <- lat_span * buf / 100

    list(
      xlim = c(lon_rng[1] - lon_pad, lon_rng[2] + lon_pad),
      ylim = c(lat_rng[1] - lat_pad, lat_rng[2] + lat_pad)
    )
  }

  format_coord_pair <- function(lon, lat, digits = 5) {
    paste0(
      "Lon ", format(round(lon, digits), nsmall = digits),
      "\nLat ", format(round(lat, digits), nsmall = digits)
    )
  }

  plot_project_static_map <- function(
      df,
      project_name = "Project",
      point_cex = 1.4,
      label_cex = 0.8,
      corner_cex = 0.7,
      buffer_pct = 8,
      show_sample_labels = TRUE,
      show_corner_coords = TRUE
  ) {
    if (is.null(df) || nrow(df) == 0) {
      stop("No valid coordinates found for the selected project.")
    }

    lims <- get_static_map_limits(df, buffer_pct = buffer_pct)
    xlim <- lims$xlim
    ylim <- lims$ylim

    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)

    par(mar = c(5.2, 5.2, 4.2, 2.2), xpd = NA)

    plot(
      NA,
      xlim = xlim,
      ylim = ylim,
      xlab = "Longitude (WGS84)",
      ylab = "Latitude (WGS84)",
      main = paste("Project map:", project_name),
      asp = 1
    )

    grid(col = "gray85", lty = "dotted")

    projects <- unique(as.character(df$project_name))
    project_cols <- map_project_palette(projects)
    point_bg <- unname(project_cols[as.character(df$project_name)])
    point_bg[is.na(point_bg)] <- "white"

    points(
      df$lon_num,
      df$lat_num,
      pch = 21,
      bg = point_bg,
      col = "black",
      lwd = 1.1,
      cex = point_cex
    )

    if (isTRUE(show_sample_labels)) {
      text(
        df$lon_num,
        df$lat_num,
        labels = df$label_text,
        pos = 4,
        offset = 0.45,
        cex = label_cex
      )
    }

    if (isTRUE(show_corner_coords)) {
      # Corner labels: NW, NE, SW, SE.
      dx <- diff(xlim)
      dy <- diff(ylim)
      x_pad <- dx * 0.015
      y_pad <- dy * 0.015

      text(
        xlim[1] + x_pad,
        ylim[2] - y_pad,
        labels = paste0("NW\n", format_coord_pair(xlim[1], ylim[2])),
        adj = c(0, 1),
        cex = corner_cex
      )

      text(
        xlim[2] - x_pad,
        ylim[2] - y_pad,
        labels = paste0("NE\n", format_coord_pair(xlim[2], ylim[2])),
        adj = c(1, 1),
        cex = corner_cex
      )

      text(
        xlim[1] + x_pad,
        ylim[1] + y_pad,
        labels = paste0("SW\n", format_coord_pair(xlim[1], ylim[1])),
        adj = c(0, 0),
        cex = corner_cex
      )

      text(
        xlim[2] - x_pad,
        ylim[1] + y_pad,
        labels = paste0("SE\n", format_coord_pair(xlim[2], ylim[1])),
        adj = c(1, 0),
        cex = corner_cex
      )
    }

    legend_projects <- projects
    legend_cols <- unname(project_cols[legend_projects])
    max_legend_projects <- 16
    if (length(legend_projects) > max_legend_projects) {
      hidden_n <- length(legend_projects) - max_legend_projects
      legend_projects <- c(legend_projects[seq_len(max_legend_projects)], paste0("... +", hidden_n, " more project(s)"))
      legend_cols <- c(legend_cols[seq_len(max_legend_projects)], NA)
    }

    legend(
      "bottomright",
      legend = c(
        paste0("Samples: ", nrow(df)),
        legend_projects,
        get_basemap_source_text("none")
      ),
      pch = c(NA, rep(21, length(legend_projects)), NA),
      pt.bg = c(NA, legend_cols, NA),
      col = c(NA, rep("black", length(legend_projects)), NA),
      bty = "n",
      cex = 0.72
    )
  }

  export_project_static_map <- function(df, file, type = c("png", "pdf")) {
    type <- match.arg(type)

    map_mode <- input$project_map_mode %||% "main_only"
    proj <- if (identical(map_mode, "all_projects")) "All projects overview" else trimws(input$map_project_name %||% "Project")
    if (!nzchar(proj)) proj <- "Project"

    point_cex <- suppressWarnings(as.numeric(input$static_map_point_cex %||% 1.4))
    label_cex <- suppressWarnings(as.numeric(input$static_map_label_cex %||% 0.8))
    corner_cex <- suppressWarnings(as.numeric(input$static_map_corner_cex %||% 0.7))
    buffer_pct <- suppressWarnings(as.numeric(input$static_map_buffer_pct %||% 8))

    if (is.na(point_cex) || point_cex <= 0) point_cex <- 1.4
    if (is.na(label_cex) || label_cex <= 0) label_cex <- 0.8
    if (is.na(corner_cex) || corner_cex <= 0) corner_cex <- 0.7
    if (is.na(buffer_pct) || buffer_pct < 0) buffer_pct <- 8

    if (identical(type, "png")) {
      grDevices::png(filename = file, width = 2200, height = 1700, res = 220)
    } else {
      grDevices::pdf(file = file, width = 10, height = 8)
    }

    on.exit(grDevices::dev.off(), add = TRUE)

    plot_project_static_map(
      df = df,
      project_name = proj,
      point_cex = point_cex,
      label_cex = label_cex,
      corner_cex = corner_cex,
      buffer_pct = buffer_pct,
      show_sample_labels = isTRUE(input$static_map_show_sample_labels),
      show_corner_coords = isTRUE(input$static_map_show_corner_coords)
    )

    invisible(file)
  }




  get_basemap_source_text <- function(provider = NULL) {
    provider <- trimws(as.character(provider %||% ""))
    if (!nzchar(provider) || identical(provider, "none")) {
      return("Basemap: none (ascetic static map)")
    }

    mapping <- c(
      "OpenStreetMap" = "Basemap source: OpenStreetMap",
      "CartoDB.Positron" = "Basemap source: CartoDB Positron",
      "Esri.WorldImagery" = "Basemap source: Esri World Imagery",
      "OpenTopoMap" = "Basemap source: OpenTopoMap"
    )

    if (provider %in% names(mapping)) {
      mapping[[provider]]
    } else {
      paste0("Basemap source: ", provider)
    }
  }

  build_project_advanced_leaflet <- function(
      df,
      project_name = "Project",
      provider = "CartoDB.Positron",
      point_cex = 1.4,
      label_cex = 0.8,
      corner_cex = 0.7,
      buffer_pct = 8,
      show_sample_labels = TRUE,
      show_corner_coords = TRUE
  ) {
    if (is.null(df) || nrow(df) == 0) {
      stop("No valid coordinates found for the selected project.")
    }

    lims <- get_static_map_limits(df, buffer_pct = buffer_pct)
    xlim <- lims$xlim
    ylim <- lims$ylim

    point_radius <- max(4, round(4 * point_cex))
    sample_label_px <- max(9, round(11 * label_cex))
    corner_label_px <- max(8, round(10 * corner_cex))

    corner_box_style <- paste(
      "background: rgba(255,255,255,0.88);",
      "padding: 4px 6px;",
      "border: 1px solid #666;",
      "border-radius: 4px;",
      "font-size:", paste0(corner_label_px, "px;"),
      "line-height: 1.15;"
    )

    build_corner_html <- function(tag, lon, lat) {
      as.character(
        htmltools::tags$div(
          style = corner_box_style,
          htmltools::HTML(paste0(
            "<b>", tag, "</b><br/>",
            gsub("\n", "<br/>", htmltools::htmlEscape(format_coord_pair(lon, lat)))
          ))
        )
      )
    }

    title_html <- as.character(
      htmltools::tags$div(
        style = "background: rgba(255,255,255,0.92); padding: 6px 10px; border: 1px solid #666; border-radius: 4px; font-weight: 600;",
        paste("Project map:", project_name)
      )
    )

    basemap_source_html <- as.character(
      htmltools::tags$div(
        style = "background: rgba(255,255,255,0.92); padding: 4px 6px; border: 1px solid #666; border-radius: 4px; font-size: 10px; line-height: 1.15;",
        get_basemap_source_text(provider)
      )
    )

    projects <- unique(as.character(df$project_name))
    pal_values <- unname(map_project_palette(projects))
    pal <- leaflet::colorFactor(palette = pal_values, domain = projects)

    m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE, attributionControl = TRUE)) %>%
      leaflet::addProviderTiles(provider = provider) %>%
      leaflet::addCircleMarkers(
        data = df,
        lng = ~lon_num,
        lat = ~lat_num,
        radius = point_radius,
        stroke = TRUE,
        weight = 1,
        color = ~pal(project_name),
        fillColor = ~pal(project_name),
        fillOpacity = 0.95,
        popup = lapply(df$popup_html, htmltools::HTML)
      ) %>%
      leaflet::addLegend(
        position = "bottomleft",
        pal = pal,
        values = projects,
        title = "Projects",
        opacity = 1
      ) %>%
      leaflet::addControl(html = title_html, position = "topright") %>%
      leaflet::addControl(html = basemap_source_html, position = "topleft") %>%
      leaflet::addControl(
        html = as.character(htmltools::tags$div(
          style = "background: rgba(255,255,255,0.92); padding: 4px 6px; border: 1px solid #666; border-radius: 4px; font-size: 11px;",
          paste0("Samples: ", nrow(df))
        )),
        position = "bottomright"
      ) %>%
      leaflet::fitBounds(lng1 = xlim[1], lat1 = ylim[1], lng2 = xlim[2], lat2 = ylim[2])

    if (isTRUE(show_sample_labels)) {
      m <- m %>%
        leaflet::addLabelOnlyMarkers(
          lng = df$lon_num,
          lat = df$lat_num,
          label = df$label_text,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            direction = "right",
            offset = c(8, 0),
            style = list(
              "font-size" = paste0(sample_label_px, "px"),
              "font-weight" = "600",
              "color" = "#111111",
              "background" = "rgba(255,255,255,0.80)",
              "padding" = "1px 3px",
              "border-radius" = "2px"
            )
          )
        )
    }

    if (isTRUE(show_corner_coords)) {
      m <- m %>%
        leaflet::addControl(build_corner_html("NW", xlim[1], ylim[2]), position = "topleft") %>%
        leaflet::addControl(build_corner_html("NE", xlim[2], ylim[2]), position = "topright") %>%
        leaflet::addControl(build_corner_html("SW", xlim[1], ylim[1]), position = "bottomleft") %>%
        leaflet::addControl(build_corner_html("SE", xlim[2], ylim[1]), position = "bottomright")
    }

    m
  }

  advanced_map_dependency_check <- function(type = c("png", "pdf", "both")) {
    type <- match.arg(type)
    required <- c("htmlwidgets", "webshot2")
    if (type %in% c("pdf", "both")) required <- c(required, "magick")
    missing <- required[
      !vapply(required, requireNamespace, logical(1), quietly = TRUE)
    ]

    list(
      ok = length(missing) == 0,
      missing = missing
    )
  }

  advanced_map_dependency_message <- function(check) {
    if (isTRUE(check$ok)) return("")
    paste0(
      "Advanced basemap export is unavailable. Missing R package",
      if (length(check$missing) == 1) ": " else "s: ",
      paste(check$missing, collapse = ", "),
      ". Run source(\"install_main_dependencies.R\") in R/RStudio, ",
      "then restart the application."
    )
  }

  output$advanced_map_dependency_status <- renderUI({
    check <- advanced_map_dependency_check("both")
    if (isTRUE(check$ok)) return(NULL)

    tags$div(
      style = paste(
        "background:#f8d7da; border:1px solid #f5c2c7;",
        "color:#842029; padding:10px; border-radius:6px;",
        "margin:8px 0 12px 0;"
      ),
      tags$b("Advanced export is not ready."),
      tags$br(),
      paste0(
        "Missing R package",
        if (length(check$missing) == 1) ": " else "s: ",
        paste(check$missing, collapse = ", "),
        "."
      ),
      tags$br(),
      "Run once in R/RStudio: ",
      tags$code('source("install_main_dependencies.R")'),
      tags$br(),
      "Then restart Make Thin Section and try the export again."
    )
  })

  export_project_advanced_map <- function(df, file, type = c("png", "pdf")) {
    type <- match.arg(type)

    dependency_check <- advanced_map_dependency_check(type)
    if (!isTRUE(dependency_check$ok)) {
      dependency_message <- advanced_map_dependency_message(dependency_check)
      showNotification(
        dependency_message,
        type = "error",
        duration = NULL
      )
      stop(dependency_message)
    }

    map_mode <- input$project_map_mode %||% "main_only"
    proj <- if (identical(map_mode, "all_projects")) "All projects overview" else trimws(input$map_project_name %||% "Project")
    if (!nzchar(proj)) proj <- "Project"

    point_cex <- suppressWarnings(as.numeric(input$static_map_point_cex %||% 1.4))
    label_cex <- suppressWarnings(as.numeric(input$static_map_label_cex %||% 0.8))
    corner_cex <- suppressWarnings(as.numeric(input$static_map_corner_cex %||% 0.7))
    buffer_pct <- suppressWarnings(as.numeric(input$static_map_buffer_pct %||% 8))

    if (is.na(point_cex) || point_cex <= 0) point_cex <- 1.4
    if (is.na(label_cex) || label_cex <= 0) label_cex <- 0.8
    if (is.na(corner_cex) || corner_cex <= 0) corner_cex <- 0.7
    if (is.na(buffer_pct) || buffer_pct < 0) buffer_pct <- 8

    provider <- input$advanced_map_provider %||% "CartoDB.Positron"

    widget <- build_project_advanced_leaflet(
      df = df,
      project_name = proj,
      provider = provider,
      point_cex = point_cex,
      label_cex = label_cex,
      corner_cex = corner_cex,
      buffer_pct = buffer_pct,
      show_sample_labels = isTRUE(input$static_map_show_sample_labels),
      show_corner_coords = isTRUE(input$static_map_show_corner_coords)
    )

    tmp_dir <- tempfile(pattern = "project_map_widget_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    tmp_html <- file.path(tmp_dir, "map.html")
    tmp_png <- file.path(tmp_dir, "map.png")

    htmlwidgets::saveWidget(widget, file = tmp_html, selfcontained = FALSE, libdir = "map_files")

    # Delay gives the browser time to fetch web tiles and render labels.
    tmp_html_path <- normalizePath(tmp_html, winslash = "/", mustWork = TRUE)
    tmp_html_url <- paste0("file:///", utils::URLencode(tmp_html_path, reserved = FALSE))

    shot_error <- NULL
    tryCatch(
      webshot2::webshot(
        url = tmp_html_url,
        file = tmp_png,
        vwidth = 1800,
        vheight = 1400,
        delay = 4,
        zoom = 2
      ),
      error = function(e) {
        shot_error <<- e
      }
    )

    if (!file.exists(tmp_png) || isTRUE(file.info(tmp_png)$size <= 0)) {
      tryCatch(
        webshot2::webshot(
          url = tmp_html_path,
          file = tmp_png,
          vwidth = 1800,
          vheight = 1400,
          delay = 4,
          zoom = 2
        ),
        error = function(e) {
          shot_error <<- e
        }
      )
    }

    if (!file.exists(tmp_png) || isTRUE(file.info(tmp_png)$size <= 0)) {
      details <- if (is.null(shot_error)) "" else paste0(" Details: ", conditionMessage(shot_error))
      stop(paste0(
        "Advanced basemap export failed: screenshot PNG was not created. ",
        "Check that webshot2 can start Chrome/Chromium and that internet access is available for map tiles.",
        details
      ))
    }

    if (identical(type, "png")) {
      file.copy(tmp_png, file, overwrite = TRUE)
    } else {
      if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Advanced basemap PDF export requires the R package 'magick'. Install it with install.packages('magick').")
      }
      img <- magick::image_read(tmp_png)
      magick::image_write(img, path = file, format = "pdf")
    }

    invisible(file)
  }

  build_project_export_sf <- function(df, format = "kml", crs_epsg = 4326) {
    format <- tolower(trimws(format %||% "kml"))
    crs_epsg <- suppressWarnings(as.integer(crs_epsg %||% 4326))
    if (is.na(crs_epsg)) crs_epsg <- 4326

    # KML: rich metadata with full descriptions, always in WGS84
    if (identical(format, "kml")) {
      desc_text <- vapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        paste(
          paste0("Sample ID: ", clip_text(row$sample_id, 300)),
          paste0("Project: ", clip_text(row$project_name, 300)),
          paste0("Author: ", clip_text(row$author, 300)),
          paste0("Field Rock Name: ", clip_text(row$field_rock_name, 300)),
          paste0("Rock name: ", clip_text(row$rock_name, 300)),
          paste0("Grain size: ", clip_text(row$grain_size, 300)),
          paste0("Primary texture: ", clip_text(row$primary_texture, 300)),
          paste0("Recognized textures: ", clip_text(row$recognized_textures, 500)),
          paste0("Latitude: ", clip_text(row$lat, 300)),
          paste0("Longitude: ", clip_text(row$lon, 300)),
          paste0("Field description: ", row$field_desc %||% ""),
          paste0("Rock description: ", row$rock_desc %||% ""),
          sep = "\n"
        )
      }, character(1))

      out_df <- data.frame(
        Name = ifelse(nzchar(trimws(df$sample_id)), trimws(df$sample_id), "Sample"),
        Description = desc_text,
        Project = df$project_name,
        Author = df$author,
        Field_Rock_Name = df$field_rock_name,
        Rock_Name = df$rock_name,
        Grain_Size = df$grain_size,
        Primary_Texture = df$primary_texture,
        Recognized_Textures = df$recognized_textures,
        Latitude = df$lat,
        Longitude = df$lon,
        lat_num = df$lat_num,
        lon_num = df$lon_num,
        stringsAsFactors = FALSE
      )
      return(sf::st_as_sf(out_df, coords = c("lon_num", "lat_num"), crs = 4326, remove = FALSE))
    }

    # SHP: compact attribute table only (DBF-friendly field names and lengths)
    if (identical(format, "shp")) {
      out_df <- data.frame(
        samp_id = ifelse(nzchar(trimws(df$sample_id)), trimws(df$sample_id), "Sample"),
        project = clip_text(df$project_name, 80),
        author = clip_text(df$author, 80),
        fld_rock = clip_text(df$field_rock_name, 120),
        rock_name = clip_text(df$rock_name, 120),
        grain_sz = clip_text(df$grain_size, 80),
        prim_tex = clip_text(df$primary_texture, 80),
        rec_tex = clip_text(df$recognized_textures, 180),
        lat_txt = clip_text(df$lat, 40),
        lon_txt = clip_text(df$lon, 40),
        fld_desc = clip_text(df$field_desc, 240),
        rock_desc = clip_text(df$rock_desc, 240),
        lon_num = df$lon_num,
        lat_num = df$lat_num,
        stringsAsFactors = FALSE
      )
      sf_obj <- sf::st_as_sf(out_df, coords = c("lon_num", "lat_num"), crs = 4326, remove = FALSE)
      if (!identical(crs_epsg, 4326)) {
        sf_obj <- sf::st_transform(sf_obj, crs_epsg)
      }
      return(sf_obj)
    }

    # GPKG: full metadata preserved, selectable CRS
    out_df <- data.frame(
      sample_id = ifelse(nzchar(trimws(df$sample_id)), trimws(df$sample_id), "Sample"),
      project_name = df$project_name,
      author = df$author,
      field_rock_name = df$field_rock_name,
      rock_name = df$rock_name,
      grain_size = df$grain_size,
      primary_texture = df$primary_texture,
      recognized_textures = df$recognized_textures,
      lat_text = df$lat,
      lon_text = df$lon,
      field_desc = df$field_desc,
      rock_desc = df$rock_desc,
      lon_num = df$lon_num,
      lat_num = df$lat_num,
      stringsAsFactors = FALSE
    )
    sf_obj <- sf::st_as_sf(out_df, coords = c("lon_num", "lat_num"), crs = 4326, remove = FALSE)
    if (!identical(crs_epsg, 4326)) {
      sf_obj <- sf::st_transform(sf_obj, crs_epsg)
    }
    sf_obj
  }

  export_project_shp_zip <- function(df, crs_epsg = 4326, zip_path) {
    if (!requireNamespace("zip", quietly = TRUE)) {
      stop("Shapefile export requires the R package 'zip'. Please run install.packages('zip') and try again.")
    }
    sf_obj <- build_project_export_sf(df, format = "shp", crs_epsg = crs_epsg)
    shp_dir <- tempfile(pattern = "shp_export_")
    dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
    shp_base <- file.path(shp_dir, "project_points.shp")
    sf::st_write(sf_obj, shp_base, driver = "ESRI Shapefile", delete_dsn = TRUE, quiet = TRUE)
    shp_parts <- list.files(shp_dir, pattern = "^project_points\\.", full.names = TRUE)
    if (length(shp_parts) == 0) stop("Shapefile export failed: no SHP component files were created.")
    zip::zipr(zipfile = zip_path, files = basename(shp_parts), root = shp_dir, include_directories = FALSE)
    if (!file.exists(zip_path) || isTRUE(file.info(zip_path)$size <= 0)) stop("Shapefile export failed: ZIP archive was not created.")
    invisible(zip_path)
  }


  output$download_project_map_png <- downloadHandler(
    filename = function() {
      map_mode <- input$project_map_mode %||% "main_only"
      proj <- if (identical(map_mode, "all_projects")) "ALL_PROJECTS" else trimws(input$map_project_name %||% "project")
      proj <- gsub("[^A-Za-z0-9._-]+", "_", proj)
      mode <- tolower(trimws(input$static_map_mode %||% "static"))
      style_tag <- if (identical(mode, "advanced")) "advanced_basemap" else "ascetic_static"
      overlay_tag <- switch(map_mode, "all_projects" = "_all_projects", "main_overlay" = "_overview", "")
      paste0("project_map_", style_tag, overlay_tag, "_", proj, "_WGS84.png")
    },
    content = function(file) {
      df <- project_map_df()
      if (is.null(df) || nrow(df) == 0) {
        df <- build_project_map_df_current()
      }
      if (nrow(df) == 0) stop("No valid coordinates found for the selected project.")
      mode <- tolower(trimws(input$static_map_mode %||% "static"))
      if (identical(mode, "advanced")) {
        export_project_advanced_map(df, file, type = "png")
      } else {
        export_project_static_map(df, file, type = "png")
      }
    }
  )

  output$download_project_map_pdf <- downloadHandler(
    filename = function() {
      map_mode <- input$project_map_mode %||% "main_only"
      proj <- if (identical(map_mode, "all_projects")) "ALL_PROJECTS" else trimws(input$map_project_name %||% "project")
      proj <- gsub("[^A-Za-z0-9._-]+", "_", proj)
      mode <- tolower(trimws(input$static_map_mode %||% "static"))
      style_tag <- if (identical(mode, "advanced")) "advanced_basemap" else "ascetic_static"
      overlay_tag <- switch(map_mode, "all_projects" = "_all_projects", "main_overlay" = "_overview", "")
      paste0("project_map_", style_tag, overlay_tag, "_", proj, "_WGS84.pdf")
    },
    content = function(file) {
      df <- project_map_df()
      if (is.null(df) || nrow(df) == 0) {
        df <- build_project_map_df_current()
      }
      if (nrow(df) == 0) stop("No valid coordinates found for the selected project.")
      mode <- tolower(trimws(input$static_map_mode %||% "static"))
      if (identical(mode, "advanced")) {
        export_project_advanced_map(df, file, type = "pdf")
      } else {
        export_project_static_map(df, file, type = "pdf")
      }
    }
  )

  output$download_project_layer <- downloadHandler(
    filename = function() {
      proj <- trimws(input$map_project_name %||% "project")
      proj <- gsub("[^A-Za-z0-9._-]+", "_", proj)
      fmt <- tolower(trimws(input$map_export_format %||% "kml"))
      crs_epsg <- suppressWarnings(as.integer(input$map_export_crs %||% "4326"))
      if (is.na(crs_epsg)) crs_epsg <- 4326L
      crs_tag <- if (identical(fmt, "kml")) "EPSG4326" else paste0("EPSG", crs_epsg)
      ext <- if (identical(fmt, "kml")) ".kml" else if (identical(fmt, "gpkg")) ".gpkg" else ".zip"
      paste0("project_map_", proj, "_", crs_tag, ext)
    },
    content = function(file) {
      df <- build_project_map_df(input$map_project_name %||% "")
      if (nrow(df) == 0) stop("No valid coordinates found for the selected project.")

      fmt <- tolower(trimws(input$map_export_format %||% "kml"))
      crs_epsg <- suppressWarnings(as.integer(input$map_export_crs %||% "4326"))
      if (is.na(crs_epsg)) crs_epsg <- 4326
      sf_obj <- build_project_export_sf(df, format = fmt, crs_epsg = crs_epsg)

      if (identical(fmt, "kml")) {
        tmp <- tempfile(fileext = ".kml")
        sf::st_write(sf_obj, tmp, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
        file.copy(tmp, file, overwrite = TRUE)
      } else if (identical(fmt, "gpkg")) {
        tmp <- tempfile(fileext = ".gpkg")
        sf::st_write(sf_obj, tmp, layer = "project_points", driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
        file.copy(tmp, file, overwrite = TRUE)
      } else {
        if (!requireNamespace("zip", quietly = TRUE)) {
          stop("Shapefile export requires the R package 'zip'. Please run install.packages('zip') and try again.")
        }

        shp_dir <- tempfile(pattern = "shp_export_")
        dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)

        shp_base <- file.path(shp_dir, "project_points.shp")
        sf::st_write(
          sf_obj,
          shp_base,
          driver = "ESRI Shapefile",
          delete_dsn = TRUE,
          quiet = TRUE
        )

        shp_parts <- list.files(
          shp_dir,
          pattern = "^project_points\\.",
          full.names = TRUE
        )

        if (length(shp_parts) == 0) {
          stop("Shapefile export failed: no SHP component files were created.")
        }

        tmp_zip <- tempfile(fileext = ".zip")

        zip::zipr(
          zipfile = tmp_zip,
          files = basename(shp_parts),
          root = shp_dir,
          include_directories = FALSE
        )

        if (!file.exists(tmp_zip) || isTRUE(file.info(tmp_zip)$size <= 0)) {
          stop("Shapefile export failed: ZIP archive was not created.")
        }

        ok <- file.copy(tmp_zip, file, overwrite = TRUE)
        if (!isTRUE(ok)) {
          stop("Shapefile export failed: could not copy ZIP archive to the download file.")
        }
      }
    }
  )


observeEvent(input$save_settings_btn, {
  if (stop_if_read_only("saving settings")) return()
  jpeg_start <- suppressWarnings(as.numeric(input$jpeg_quality_default_setting %||% 85))
  jpeg_min <- suppressWarnings(as.numeric(input$jpeg_quality_min_setting %||% 60))
  if (is.na(jpeg_start) || is.na(jpeg_min) || jpeg_min > jpeg_start) {
    showNotification(
      "JPEG minimum quality cannot be greater than JPEG start quality.",
      type = "error",
      duration = 6
    )
    return()
  }
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  panel_bg <- normalize_hex_color(input$panel_bg_setting %||% "#ffffff", "#ffffff")
  updateTextInput(session, "panel_bg_setting", value = panel_bg)
  settings_set(con, "author", input$author_default %||% "")
  settings_set(con, "project", input$project_default %||% "")
  settings_set(con, "confirm_load", if (isTRUE(input$confirm_load_setting)) "TRUE" else "FALSE")
  settings_set(con, "panel_bg", panel_bg)
  settings_set(con, "docx_font_size", as.character(input$docx_font_size_setting %||% 10))

  settings_set(con, "max_sel_images", as.character(input$max_sel_images %||% 10))
  settings_set(con, "img_card_width", as.character(input$img_card_width_setting %||% 280))
  settings_set(con, "img_thumb_height", as.character(input$img_thumb_height_setting %||% 170))

settings_set(con, "max_image_mb", as.character(input$max_image_mb_setting %||% 4))
settings_set(con, "max_image_dim", as.character(input$max_image_dim_setting %||% 2500))
settings_set(con, "jpeg_quality_default", as.character(input$jpeg_quality_default_setting %||% 85))
settings_set(con, "jpeg_quality_min", as.character(input$jpeg_quality_min_setting %||% 60))
settings_set(
  con,
  "track_unused_image_files",
  if (isTRUE(input$track_unused_image_files_setting)) "TRUE" else "FALSE"
)
  # Update Shiny upload limit (raw upload can be larger than optimized size)
  max_req_mb <- suppressWarnings(as.numeric(input$max_image_mb_setting %||% 4))
  if (is.na(max_req_mb) || max_req_mb <= 0) max_req_mb <- 4
  max_req_mb <- max(25, max_req_mb * 10)  # generous buffer
  options(shiny.maxRequestSize = max_req_mb * 1024^2)
  # Apply image container display size immediately after saving settings
  card_w <- suppressWarnings(as.numeric(input$img_card_width_setting %||% 280))
  thumb_h <- suppressWarnings(as.numeric(input$img_thumb_height_setting %||% 170))
  if (!is.na(card_w) && card_w >= 180) {
    session$sendCustomMessage("setCssVar", list(name="--img-card-width", value=paste0(card_w, "px")))
  }
  if (!is.na(thumb_h) && thumb_h >= 100) {
    session$sendCustomMessage("setCssVar", list(name="--img-thumb-height", value=paste0(thumb_h, "px")))
  }
  session$sendCustomMessage("setCssVar", list(name="--panel-bg", value=panel_bg))

  # Also apply defaults to sidebar inputs (non-destructive if user already typed something)
  if (!nzchar(input$author %||% "")) updateTextInput(session, "author", value = input$author_default %||% "")
  if (!nzchar(input$project_name %||% "")) updateSelectizeInput(session, "project_name", choices = unique(c(list_projects(con), trimws(input$project_default %||% ""))), selected = trimws(input$project_default %||% ""), server = TRUE)
  saved_now <- current_settings_state()
  saved_now$panel_bg <- panel_bg
  saved_settings_state(saved_now)
  showNotification("All settings were saved to SQLite.", type = "message", duration = 3)
})
  
observeEvent(input$panel_bg_setting, {
  bg <- normalize_hex_color(input$panel_bg_setting %||% "#ffffff", "")
  if (!nzchar(bg)) return()
  session$sendCustomMessage("setCssVar", list(name="--panel-bg", value=bg))
}, ignoreInit = TRUE)
  # =========================
  # Library: biblio + minerals abbreviations
  # =========================
  biblio_db <- reactiveVal(NULL)
  minerals_map <- reactiveVal(NULL)
  mineral_choices_rv <- reactiveVal(character(0))
  genesis_choices_rv <- reactiveVal(character(0))
  grains_db <- reactiveVal(NULL)
  textures_db <- reactiveVal(NULL)

  refresh_genesis_choices <- function(selected = NULL, update_text = TRUE) {
    ch <- load_genesis_choices()

    if (!is.null(selected) && nzchar(trimws(selected))) {
      ch <- unique(c(selected, ch))
    }

    ch <- sort(unique(ch[!is.na(ch) & nzchar(trimws(ch))]))
    genesis_choices_rv(ch)

    # Plain selectInput is used only as an insertion helper.
    # The real editable value remains in textInput("m_gen").
    updateSelectInput(
      session,
      "m_gen_suggestion",
      choices = c("-- choose suggestion --" = "", stats::setNames(ch, ch)),
      selected = ""
    )

    if (isTRUE(update_text) && !is.null(selected)) {
      updateTextAreaInput(session, "m_gen", value = selected)
    }
  }

  morphology_choices_rv <- reactiveVal(character(0))

  refresh_morphology_choices <- function(mineral_name = NULL, selected = NULL, update_text = TRUE) {
    # In the current database policy mineral_composition$mineral stores FULL mineral names.
    # Therefore no abbreviation conversion is needed for the morphology lookup.
    mineral_name <- trimws(mineral_name %||% isolate(input$m_mineral %||% ""))

    ch <- load_morphology_choices_for_mineral(mineral_name)

    if (!is.null(selected) && nzchar(trimws(selected))) {
      ch <- unique(c(selected, ch))
    }

    ch <- sort(unique(ch[!is.na(ch) & nzchar(trimws(ch))]))
    morphology_choices_rv(ch)

    # Plain selectInput is used only as an insertion helper.
    # The real editable value remains in textAreaInput("m_morph").
    updateSelectInput(
      session,
      "m_morph_suggestion",
      choices = c("-- choose suggestion --" = "", stats::setNames(ch, ch)),
      selected = ""
    )

    if (isTRUE(update_text) && !is.null(selected)) {
      updateTextAreaInput(session, "m_morph", value = selected)
    }
  }


  # Insert selected suggestions into editable fields.
  # The actual values used by Add/Update remain input$m_morph and input$m_gen.
  observeEvent(input$m_gen_suggestion, {
    val <- input$m_gen_suggestion %||% ""
    if (!nzchar(trimws(val))) return()
    updateTextInput(session, "m_gen", value = val)
    updateSelectInput(session, "m_gen_suggestion", selected = "")
  }, ignoreInit = TRUE)

  observeEvent(input$m_morph_suggestion, {
    val <- input$m_morph_suggestion %||% ""
    if (!nzchar(trimws(val))) return()
    updateTextAreaInput(session, "m_morph", value = val)
    updateSelectInput(session, "m_morph_suggestion", selected = "")
  }, ignoreInit = TRUE)
  
  default_biblio_path <- file.path(getwd(), "biblio_for_mineral_descriptions.csv")
  default_minerals_path <- file.path(getwd(), "minerals.csv")
  default_grains_path <- file.path(getwd(), "grainsizes.csv")
  default_textures_path <- file.path(getwd(), "textures.csv")
  default_rocktype_path <- file.path(getwd(), "lookup_rocktype.csv")

  resolve_rocktype_csv_path <- function() {
    # V59.5: one external classifier file only: lookup_rocktype.csv.
    # Search order:
    # 1) app working directory
    # 2) SQLite DB folder
    checked <- character(0)

    candidates <- unique(c(
      file.path(getwd(), "lookup_rocktype.csv"),
      file.path(dirname(DB_PATH), "lookup_rocktype.csv")
    ))

    for (cand in candidates) {
      cand <- normalizePath(cand, winslash = "/", mustWork = FALSE)
      checked <- c(checked, cand)
      if (file.exists(cand)) {
        attr(cand, "checked") <- unique(checked)
        return(cand)
      }
    }

    out <- NA_character_
    attr(out, "checked") <- unique(checked)
    out
  }

  mapping_units_cache <- reactiveVal(data.frame())
  mapping_classifier_status_rv <- reactiveVal("Mapping classifier has not been loaded yet.")
  mapping_dropdown_status_rv <- reactiveVal("Suggested lithological unit dropdown: not loaded.")

  output$mapping_dropdown_status_ui <- renderUI({
    tags$p(class = "small-note", mapping_dropdown_status_rv() %||% "")
  })

  refresh_mapping_classifier_choices <- function(selected_classifier = NULL) {
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    cls <- list_mapping_classifiers(con)

    if (length(cls) == 0) {
      if (!nzchar(trimws(mapping_classifier_status_rv() %||% "")) || identical(mapping_classifier_status_rv(), "Mapping classifier has not been loaded yet.")) {
        mapping_classifier_status_rv("No mapping classifiers are currently loaded in SQLite.")
      }
      updateSelectInput(session, "mapping_classifier", choices = character(0), selected = character(0))
      updateSelectizeInput(
        session,
        "suggested_mapping_unit_id",
        choices = c("-- no classifier loaded --" = ""),
        selected = "",
        server = FALSE,
        options = list(create = FALSE, maxOptions = 5000)
      )
      mapping_dropdown_status_rv("Suggested lithological unit dropdown: 0 units loaded.")
      mapping_units_cache(data.frame())
      return(invisible(FALSE))
    }

    cur <- selected_classifier %||% isolate(input$mapping_classifier %||% "")
    if (!nzchar(trimws(cur)) || !(cur %in% cls)) cur <- cls[1]

    updateSelectInput(session, "mapping_classifier", choices = cls, selected = cur)
    refresh_mapping_unit_choices(selected_unit_id = isolate(input$suggested_mapping_unit_id %||% ""), classifier_override = cur)
    invisible(TRUE)
  }

  refresh_mapping_unit_choices <- function(selected_unit_id = NULL, classifier_override = NULL) {
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    classifier <- trimws(classifier_override %||% isolate(input$mapping_classifier %||% ""))
    df <- load_mapping_units(con, classifier)
    mapping_units_cache(df)

    choices <- c("-- not assigned --" = "")
    n_units <- 0L

    if (!is.null(df) && nrow(df) > 0) {
      labels <- as.character(df$display_label %||% "")
      if (length(labels) != nrow(df)) labels <- rep("", nrow(df))

      missing_label <- !nzchar(trimws(labels))
      labels[missing_label] <- as.character(df$unit_name[missing_label])
      labels <- trimws(labels)
      labels[!nzchar(labels)] <- paste0("unit_id ", as.character(df$unit_id[!nzchar(labels)]))

      vals <- as.character(df$unit_id)

      keep <- nzchar(vals) & nzchar(labels)
      vals <- vals[keep]
      labels <- labels[keep]

      # Avoid duplicated selectize values/labels.
      dup <- duplicated(vals)
      if (any(dup)) {
        vals <- vals[!dup]
        labels <- labels[!dup]
      }

      n_units <- length(vals)
      choices <- c(choices, stats::setNames(vals, labels))
    }

    selected <- as.character(selected_unit_id %||% "")
    if (!nzchar(selected) || !(selected %in% unname(choices))) selected <- ""

    # Important fix:
    # The mapping classifier normally has hundreds, not tens of thousands, of values.
    # Client-side selectize is more reliable here than server-side selectize,
    # and it avoids an empty visible dropdown after the SQLite import succeeds.
    updateSelectizeInput(
      session,
      "suggested_mapping_unit_id",
      choices = choices,
      selected = selected,
      server = FALSE,
      options = list(create = FALSE, persist = TRUE, maxOptions = 5000)
    )

    mapping_dropdown_status_rv(
      paste0(
        "Suggested lithological unit dropdown: ", n_units, " unit(s) loaded",
        if (nzchar(classifier)) paste0(" from '", classifier, "'.") else "."
      )
    )

    invisible(n_units)
  }

  set_mapping_unit_selection_from_id <- function(unit_id) {
    unit_id <- trimws(as.character(unit_id %||% ""))
    if (!nzchar(unit_id)) {
      refresh_mapping_classifier_choices()
      updateSelectizeInput(session, "suggested_mapping_unit_id", selected = "", server = FALSE)
      return(invisible(FALSE))
    }

    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    info <- tryCatch(
      DBI::dbGetQuery(con, "
        SELECT unit_id, classifier_name
        FROM mapping_units
        WHERE unit_id = :unit_id
        LIMIT 1
      ", params = list(unit_id = suppressWarnings(as.integer(unit_id)))),
      error = function(e) data.frame()
    )

    if (!is.null(info) && nrow(info) > 0) {
      classifier <- as.character(info$classifier_name[1] %||% "")
      refresh_mapping_classifier_choices(selected_classifier = classifier)
      refresh_mapping_unit_choices(selected_unit_id = unit_id, classifier_override = classifier)
    } else {
      refresh_mapping_classifier_choices()
      updateSelectizeInput(session, "suggested_mapping_unit_id", selected = "", server = FALSE)
    }

    invisible(TRUE)
  }

  output$mapping_units_table <- DT::renderDT({
    df <- mapping_units_cache()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Note = "No mapping units loaded."), rownames = FALSE, options = list(dom = "t")))
    }

    out <- df[, c("unit_id", "classifier_name", "source_id", "class_id", "unit_code", "unit_name", "shortname", "unit_description"), drop = FALSE]
    names(out) <- c("unit_id", "classifier", "source_id", "class_id", "unit_code", "unit_name", "shortname", "description")
    DT::datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })

  output$mapping_classifier_status <- renderText({
    mapping_classifier_status_rv()
  })

  observeEvent(input$mapping_classifier, {
    refresh_mapping_unit_choices(selected_unit_id = isolate(input$suggested_mapping_unit_id %||% ""))
  }, ignoreInit = TRUE)

  # IMPORTANT: selectize choices should RETURN FULL mineral name as value
  build_mineral_choices <- function(mm) {
    choices <- character(0)
    # minerals.csv can be either:
    #  - mineral_name (+ optional mineral_id)
    #  - OR a single column with mineral names
    if (!is.null(mm)) {
      # normalize column name for mineral list
      if (!("mineral_name" %in% names(mm))) {
        # try first column as mineral_name
        names(mm)[1] <- "mineral_name"
      }
      mm$mineral_name <- trimws(mm$mineral_name %||% "")
      mm <- mm[nzchar(mm$mineral_name), , drop = FALSE]
      # Preserve exact mineral names from minerals.csv (display + stored value)
      mm$mineral_title <- mm$mineral_name
      labs <- character(0)
      vals <- character(0)
      if ("mineral_id" %in% names(mm)) mm$mineral_id <- trimws(mm$mineral_id %||% "")
      for (i in seq_len(nrow(mm))) {
        nm <- mm$mineral_title[i] %||% ""
        ab <- if ("mineral_id" %in% names(mm)) (mm$mineral_id[i] %||% "") else ""
        ab <- trimws(ab)
        # Label can show abbreviation, but VALUE is ALWAYS full (Title Case) mineral name
        labs <- c(labs, if (nzchar(ab)) paste0(nm, " (", ab, ")") else nm)
        vals <- c(vals, nm)
      }
      choices <- c(choices, stats::setNames(vals, labs))
    }
    if (length(choices) == 0) return(character(0))
    key <- paste(names(choices), choices, sep = "||")
    choices[!duplicated(key)]
  }

  # Build a JS object literal from a named character vector (for selectize tooltips)
  
# Build a JS object literal (actually JSON) from a named character vector (for selectize tooltips).
# Using jsonlite avoids broken escaping when the CSV contains smart quotes or control chars.
js_obj_from_named <- function(x) {
  if (is.null(x) || length(x) == 0) return("{}")
  keys <- enc2utf8(as.character(names(x)))
  vals <- enc2utf8(as.character(unname(x)))
  keys[is.na(keys)] <- ""
  vals[is.na(vals)] <- ""
  obj <- as.list(vals)
  names(obj) <- keys
  jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")
}

load_library <- function(selection_override = list(), quiet = FALSE) {
    selected_value <- function(name, current_value = "") {
      if (name %in% names(selection_override)) {
        value <- selection_override[[name]]
      } else {
        value <- current_value
      }
      as.character(value %||% "")
    }

    # paths (uploaded or default)
    b_path <- NULL
    if (!is.null(input$biblio_file) && nrow(input$biblio_file) > 0) {
      b_path <- input$biblio_file$datapath[1]
    } else if (file.exists(default_biblio_path)) {
      b_path <- default_biblio_path
    }
    
    m_path <- NULL
    if (!is.null(input$minerals_file) && nrow(input$minerals_file) > 0) {
      m_path <- input$minerals_file$datapath[1]
    } else if (file.exists(default_minerals_path)) {
      m_path <- default_minerals_path
    }
    
    g_path <- NULL
    if (!is.null(input$grains_file) && nrow(input$grains_file) > 0) {
      g_path <- input$grains_file$datapath[1]
    } else if (file.exists(default_grains_path)) {
      g_path <- default_grains_path
    }
    t_path <- NULL
    if (!is.null(input$textures_file) && nrow(input$textures_file) > 0) {
      t_path <- input$textures_file$datapath[1]
    } else if (file.exists(default_textures_path)) {
      t_path <- default_textures_path
    }

    rt_path <- resolve_rocktype_csv_path()
    checked_paths <- attr(rt_path, "checked") %||% character(0)

    # V59.5: import mapping-unit / rock-type classifier from one file only:
    # lookup_rocktype.csv.
    selected_classifier <- selected_value(
      "mapping_classifier",
      isolate(input$mapping_classifier %||% "")
    )
    if (!nzchar(trimws(selected_classifier))) selected_classifier <- "ORIGINAL_LITHOLOGY_ENGLISH"
    selected_mapping_unit_id <- selected_value(
      "suggested_mapping_unit_id",
      isolate(input$suggested_mapping_unit_id %||% "")
    )

    imported_n <- 0L
    import_msg <- ""

    if (!isTRUE(READ_ONLY_MODE)) {
      con_rt <- db_connect()
      on.exit(dbDisconnect(con_rt), add = TRUE)
      db_init(con_rt)

      if (!is.na(rt_path) && nzchar(rt_path) && file.exists(rt_path)) {
        imported_n <- tryCatch(
          import_mapping_units_from_csv(con_rt, rt_path),
          error = function(e) {
            import_msg <<- paste("Mapping classifier import error:", e$message)
            showNotification(import_msg, type = "error", duration = 8)
            0L
          }
        )

        counts <- tryCatch(
          DBI::dbGetQuery(con_rt, "
            SELECT classifier_name, COUNT(*) AS n
            FROM mapping_units
            WHERE classifier_name IN ('ORIGINAL_LITHOLOGY_ENGLISH', 'LITHOLOGY_Canada_Geological_Survey, 1:5M scale')
              AND is_active = 1
            GROUP BY classifier_name
            ORDER BY classifier_name
          "),
          error = function(e) data.frame(classifier_name = character(0), n = integer(0))
        )

        counts_txt <- if (!is.null(counts) && nrow(counts) > 0) {
          paste(paste0(counts$classifier_name, ": ", counts$n), collapse = "\n")
        } else {
          "No standard classifier rows found in SQLite."
        }

        import_msg <- paste0(
          "lookup_rocktype.csv found: ", normalizePath(rt_path, winslash = "/", mustWork = FALSE), "\n",
          "Imported/updated rows from CSV: ", imported_n, "\n",
          "Active rows in SQLite:\n", counts_txt
        )

        if (!isTRUE(quiet) && !is.null(imported_n) && imported_n > 0) {
          showNotification(paste0("lookup_rocktype.csv loaded: ", imported_n, " unit/class pair(s)."), type = "message", duration = 5)
        }
      } else {
        import_msg <- paste0(
          "lookup_rocktype.csv was NOT found.\n",
          "Put lookup_rocktype.csv in the app folder or in the SQLite DB folder.\n\n",
          "Checked paths:\n",
          paste(unique(checked_paths), collapse = "\n")
        )
        if (!isTRUE(quiet)) {
          showNotification("lookup_rocktype.csv was not found. See Maintenance status for checked paths.", type = "warning", duration = 8)
        }
      }
    } else {
      import_msg <- "Read-only LAN mode: external CSV import into SQLite is disabled. Use local edit mode to load classifier."
    }

    mapping_classifier_status_rv(import_msg)
    refresh_mapping_classifier_choices(selected_classifier = selected_classifier)

    # Force a second client-side update after UI flush.
    session$onFlushed(function() {
      try(
        refresh_mapping_unit_choices(
          selected_unit_id = selected_mapping_unit_id,
          classifier_override = selected_classifier
        ),
        silent = TRUE
      )
    }, once = TRUE)

    # biblio
    bib <- NULL
    if (!is.null(b_path) && file.exists(b_path)) {
      bib <- tryCatch(read_csv_guess(b_path), error = function(e) NULL)
      if (!is.null(bib)) {
        names(bib) <- trimws(gsub("\ufeff", "", names(bib)))
      }
      if (!is.null(bib) && "Mineral" %in% names(bib)) {
        bib$Mineral_norm <- vapply(bib$Mineral, norm_mineral, character(1))
      }
    }
    biblio_db(bib)
    
    # minerals
    mm <- NULL
    if (!is.null(m_path) && file.exists(m_path)) {
      mm <- tryCatch(read_csv_guess(m_path), error = function(e) NULL)
      if (!is.null(mm)) {
        names(mm) <- trimws(gsub("\ufeff", "", names(mm)))
      }
      if (!is.null(mm)) {
        if ("mineral_name" %in% names(mm)) mm$mineral_name_norm <- vapply(mm$mineral_name, norm_mineral, character(1))
        if ("mineral_id" %in% names(mm))   mm$mineral_id_norm   <- vapply(mm$mineral_id,   norm_mineral, character(1))
      }
    }
    minerals_map(mm)
    # grainsizes
    gg <- NULL
    if (!is.null(g_path) && file.exists(g_path)) {
      gg <- tryCatch(read_csv_guess(g_path), error = function(e) NULL)
      if (!is.null(gg)) {
        names(gg) <- trimws(gsub("\ufeff", "", names(gg)))
      }
      if (!is.null(gg)) {
        if (!("grainsize_name" %in% names(gg))) names(gg)[1] <- "grainsize_name"
        if (!("Size_Range" %in% names(gg)) && ncol(gg) >= 2) names(gg)[2] <- "Size_Range"
        gg$grainsize_name <- trimws(gg$grainsize_name %||% "")
        gg <- gg[nzchar(gg$grainsize_name), , drop = FALSE]
        gg$grainsize_title <- tools::toTitleCase(tolower(gg$grainsize_name))
      }
    }
    grains_db(gg)
    # textures
    tt <- NULL
    if (!is.null(t_path) && file.exists(t_path)) {
      tt <- tryCatch(read_csv_guess(t_path), error = function(e) NULL)
      if (!is.null(tt)) {
        names(tt) <- trimws(gsub("\ufeff", "", names(tt)))
      }
      if (!is.null(tt)) {
        if (!("texture_name" %in% names(tt))) names(tt)[1] <- "texture_name"
        if (!("Explanation" %in% names(tt)) && ncol(tt) >= 2) names(tt)[2] <- "Explanation"
        tt$texture_name <- trimws(tt$texture_name %||% "")
        tt <- tt[nzchar(tt$texture_name), , drop = FALSE]
        tt$texture_title <- tools::toTitleCase(tolower(tt$texture_name))
      }
    }
    textures_db(tt)
    # update selectize choices
    ch <- build_mineral_choices(mm)
    mineral_choices_rv(ch)
    cur_m <- selected_value("m_mineral", isolate(input$m_mineral %||% ""))
    cur_l <- selected_value("lib_mineral", isolate(input$lib_mineral %||% ""))
    updateSelectizeInput(session, "m_mineral", choices = ch, selected = if (nzchar(cur_m)) cur_m else NULL, server = TRUE)
    updateSelectizeInput(session, "lib_mineral", choices = ch, selected = if (nzchar(cur_l)) cur_l else NULL, server = FALSE)
    updateSelectizeInput(session, "web_ref_minerals", choices = ch,
                         selected = isolate(input$web_ref_minerals %||% character(0)), server = TRUE)
# update grainsize / texture choices (tooltip on hover; store only the term)
gg2 <- grains_db()
if (!is.null(gg2) && "grainsize_title" %in% names(gg2)) {
  gg2$Size_Range <- if ("Size_Range" %in% names(gg2)) trimws(gg2$Size_Range %||% "") else ""
  g_vals <- sort(unique(gg2$grainsize_title))
  # Provide both label and value (avoid undefined item.text in custom render)
  g_choices <- c(stats::setNames("", ""), setNames(g_vals, g_vals))

  # tooltips map: value -> Size_Range
  g_tt <- gg2$Size_Range
  names(g_tt) <- gg2$grainsize_title
  g_tt <- g_tt[!duplicated(names(g_tt))]
  g_tt_js <- js_obj_from_named(g_tt)

  g_render <- I(paste0(
    "{",
    "option:function(item,escape){var tt=", g_tt_js, ";",
    "var v=item.value||''; var label=(item.text||item.value||'');",
    "var t=tt[v]||''; var title=t?('Size range: '+t):'';",
    "return '<div title=\"'+escape(title)+'\">'+escape(label)+'</div>';},",
    "item:function(item,escape){var tt=", g_tt_js, ";",
    "var v=item.value||''; var label=(item.text||item.value||'');",
    "var t=tt[v]||''; var title=t?('Size range: '+t):'';",
    "return '<div title=\"'+escape(title)+'\">'+escape(label)+'</div>';}",
    "}"
  ))

  cur_g <- selected_value("grain_size", isolate(input$grain_size %||% ""))
  updateSelectizeInput(
    session, "grain_size",
    choices = g_choices,
    selected = if (!is.null(cur_g) && nzchar(cur_g)) cur_g else NULL,
    server = TRUE,
    options = list(render = g_render, create = TRUE)
  )
}

tt2 <- textures_db()
if (!is.null(tt2) && "texture_title" %in% names(tt2)) {
  tt2$Explanation <- if ("Explanation" %in% names(tt2)) trimws(tt2$Explanation %||% "") else ""
  t_vals <- sort(unique(tt2$texture_title))
  # Provide both label and value (avoid undefined item.text in custom render)
  t_choices <- c(stats::setNames("", ""), setNames(t_vals, t_vals))

  # tooltips map: value -> Explanation
  t_tt <- tt2$Explanation
  names(t_tt) <- tt2$texture_title
  t_tt <- t_tt[!duplicated(names(t_tt))]
  t_tt_js <- js_obj_from_named(t_tt)

  t_render <- I(paste0(
    "{",
    "option:function(item,escape){var tt=", t_tt_js, ";",
    "var v=item.value||''; var label=(item.text||item.value||'');",
    "var t=tt[v]||''; var title=t?('Explanation: '+t):'';",
    "return '<div title=\"'+escape(title)+'\">'+escape(label)+'</div>';},",
    "item:function(item,escape){var tt=", t_tt_js, ";",
    "var v=item.value||''; var label=(item.text||item.value||'');",
    "var t=tt[v]||''; var title=t?('Explanation: '+t):'';",
    "return '<div title=\"'+escape(title)+'\">'+escape(label)+'</div>';}",
    "}"
  ))

  cur_t <- selected_value("texture", isolate(input$texture %||% ""))
  updateSelectizeInput(
    session, "texture",
    choices = t_choices,
    selected = if (!is.null(cur_t) && nzchar(cur_t)) cur_t else NULL,
    server = TRUE,
    options = list(render = t_render, create = TRUE)
  )
}
  }

  reload_reference_sources <- function(selection_override = list(), notify_user = FALSE) {
    load_library(
      selection_override = selection_override,
      quiet = !isTRUE(notify_user)
    )

    selected_value <- function(name, current_value = "") {
      if (name %in% names(selection_override)) {
        value <- selection_override[[name]]
      } else {
        value <- current_value
      }
      as.character(value %||% "")
    }

    refresh_genesis_choices(
      selected = selected_value("m_gen", isolate(input$m_gen %||% ""))
    )
    refresh_morphology_choices(
      mineral_name = selected_value("m_mineral", isolate(input$m_mineral %||% "")),
      selected = selected_value("m_morph", isolate(input$m_morph %||% ""))
    )

    if (isTRUE(notify_user)) {
      showNotification(
        "Reference libraries and mapping classifier reloaded.",
        type = "message",
        duration = 4
      )
    }
    invisible(TRUE)
  }

  schedule_reference_reload <- function(selection_override = list()) {
    session$onFlushed(
      function() {
        isolate(
          reload_reference_sources(
            selection_override = selection_override,
            notify_user = FALSE
          )
        )
      },
      once = TRUE
    )
    invisible(TRUE)
  }
  
  observeEvent(input$reload_library, {
    reload_reference_sources(notify_user = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(TRUE, {
    load_library()
    refresh_genesis_choices()
    refresh_morphology_choices()
  }, once = TRUE)
  
  # convert typed abbreviation -> full mineral_name; otherwise keep as typed
  to_full_mineral <- function(x) {
    x0 <- trimws(x %||% "")
    if (!nzchar(x0)) return("")
    mm <- minerals_map()
    if (is.null(mm) || nrow(mm) == 0) return(x0)
    # accept either "mineral_name" or first column as mineral list
    if (!("mineral_name" %in% names(mm))) names(mm)[1] <- "mineral_name"
    mm$mineral_name <- trimws(mm$mineral_name %||% "")
    mm <- mm[nzchar(mm$mineral_name), , drop = FALSE]
    mm$mineral_title <- mm$mineral_name
    mm$mineral_name_norm <- vapply(mm$mineral_name, norm_mineral, character(1))
    if ("mineral_id" %in% names(mm)) {
      mm$mineral_id_norm <- vapply(mm$mineral_id, norm_mineral, character(1))
    }
    xn <- norm_mineral(x0)
    # If typed abbreviation and mineral_id exists -> return official mineral name as written in minerals.csv
    if ("mineral_id_norm" %in% names(mm)) {
      hit <- mm[mm$mineral_id_norm == xn & nzchar(mm$mineral_title), , drop = FALSE]
      if (nrow(hit) > 0) return(trimws(hit$mineral_title[1]))
    }
    # If typed mineral name (any case) -> return official mineral name as written in minerals.csv
    hit2 <- mm[mm$mineral_name_norm == xn & nzchar(mm$mineral_title), , drop = FALSE]
    if (nrow(hit2) > 0) return(trimws(hit2$mineral_title[1]))
    # Otherwise (myrmekite, granophyre...) keep exactly as typed
    x0
  }
  is_custom_mineral_value <- function(x) {
    x0 <- trimws(x %||% "")
    if (!nzchar(x0)) return(FALSE)
    mm <- minerals_map()
    if (is.null(mm) || nrow(mm) == 0) return(TRUE)
    if (!("mineral_name" %in% names(mm))) names(mm)[1] <- "mineral_name"
    mm$mineral_name <- trimws(mm$mineral_name %||% "")
    mm <- mm[nzchar(mm$mineral_name), , drop = FALSE]
    mm$mineral_name_norm <- vapply(mm$mineral_name, norm_mineral, character(1))
    if ("mineral_id" %in% names(mm)) {
      mm$mineral_id <- trimws(mm$mineral_id %||% "")
      mm$mineral_id_norm <- vapply(mm$mineral_id, norm_mineral, character(1))
    }
    xn <- norm_mineral(x0)
    if (xn %in% (mm$mineral_name_norm %||% character(0))) return(FALSE)
    if ("mineral_id_norm" %in% names(mm) && xn %in% (mm$mineral_id_norm %||% character(0))) return(FALSE)
    TRUE
  }

  append_custom_mineral_to_csv <- function(mineral_name) {
    mineral_name <- trimws(mineral_name %||% "")
    if (!nzchar(mineral_name)) return(list(ok = FALSE, message = "Mineral name is empty."))

    csv_path <- default_minerals_path
    if (!file.exists(csv_path)) {
      return(list(ok = FALSE, message = "Local minerals.csv was not found in the app folder."))
    }

    mm <- tryCatch(read_csv_guess(csv_path), error = function(e) NULL)
    if (is.null(mm)) {
      return(list(ok = FALSE, message = "Could not read minerals.csv"))
    }

    names(mm) <- trimws(gsub("\ufeff", "", names(mm)))
    if (!("mineral_name" %in% names(mm))) names(mm)[1] <- "mineral_name"
    mm$mineral_name <- trimws(mm$mineral_name %||% "")

    existing_norm <- vapply(mm$mineral_name, norm_mineral, character(1))
    if (norm_mineral(mineral_name) %in% existing_norm) {
      return(list(ok = TRUE, message = "This mineral is already in minerals.csv"))
    }

    new_row <- as.list(rep("", ncol(mm)))
    names(new_row) <- names(mm)
    new_row[["mineral_name"]] <- mineral_name
    if ("mineral_name_norm" %in% names(new_row)) new_row[["mineral_name_norm"]] <- norm_mineral(mineral_name)

    mm2 <- rbind(mm, as.data.frame(new_row, stringsAsFactors = FALSE, check.names = FALSE))

    ok <- tryCatch({
      utils::write.csv(mm2, csv_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      TRUE
    }, error = function(e) {
      attr(e, "message_text") <- e$message
      FALSE
    })

    if (!ok) {
      return(list(ok = FALSE, message = "Could not update minerals.csv"))
    }

    list(ok = TRUE, message = paste0("Added to minerals.csv: ", mineral_name))
  }

  output$custom_mineral_controls <- renderUI({
    cur <- trimws(input$m_mineral %||% "")
    if (!nzchar(cur)) return(NULL)
    if (!is_custom_mineral_value(cur)) return(NULL)

    tags$div(
      style = "margin-top:6px; padding:6px 8px; background:#fff3cd; border:1px solid #ffe69c; border-radius:6px;",
      tags$div(
        class = "small-note",
        style = "margin-bottom:6px; color:#a15c00; font-weight:600;",
        paste0("Custom mineral name (not found in minerals.csv): ", cur)
      ),
      actionButton("add_custom_mineral_to_list", "Add this item to list of minerals", class = "btn btn-warning btn-sm")
    )
  })

  observeEvent(input$add_custom_mineral_to_list, {
    cur <- trimws(input$m_mineral %||% "")
    if (!nzchar(cur)) {
      showNotification("Mineral name is empty.", type = "warning")
      return()
    }

    res <- append_custom_mineral_to_csv(cur)
    if (!isTRUE(res$ok)) {
      showNotification(res$message %||% "Could not update minerals.csv", type = "error", duration = NULL)
      return()
    }

    showNotification(res$message %||% "Mineral added to minerals.csv", type = "message", duration = 4)
    load_library()

    ch <- isolate(mineral_choices_rv())
    if (nzchar(cur) && !(cur %in% ch)) {
      ch <- unique(c(cur, ch))
      mineral_choices_rv(ch)
    }
    updateSelectizeInput(session, "m_mineral", choices = ch, selected = cur, server = TRUE)
  }, ignoreInit = TRUE)

  # The Mineral Reference lookup is intentionally independent. Rock description
  # changes still refresh morphology, but they no longer overwrite lib_mineral.
  observeEvent(input$m_mineral, {
    refresh_morphology_choices(
      mineral_name = input$m_mineral %||% "",
      selected = isolate(input$m_morph %||% ""),
      update_text = FALSE
    )
  }, ignoreInit = TRUE)

  refresh_lib_mineral_choices <- function() {
    ch <- isolate(mineral_choices_rv())
    if (is.null(ch) || length(ch) == 0) {
      ch <- build_mineral_choices(isolate(minerals_map()))
      mineral_choices_rv(ch)
    }
    current <- isolate(input$lib_mineral %||% "")
    updateSelectizeInput(session, "lib_mineral", choices = ch,
                         selected = if (nzchar(current)) current else NULL, server = FALSE)
    updateSelectizeInput(session, "web_ref_minerals", choices = ch,
                         selected = isolate(input$web_ref_minerals %||% character(0)), server = TRUE)
    invisible(length(ch))
  }

  session$onFlushed(function() refresh_lib_mineral_choices(), once = TRUE)
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Mineral reference")) refresh_lib_mineral_choices()
  }, ignoreInit = TRUE)
  observeEvent(input$lib_reload_minerals, {
    load_library(quiet = TRUE)
    n <- refresh_lib_mineral_choices()
    showNotification(paste(n, "mineral choices loaded."), type = "message", duration = 3)
  }, ignoreInit = TRUE)
  output$lib_mineral_status_ui <- renderUI({
    n <- length(mineral_choices_rv())
    if (n > 0) {
      tags$span(style = "color:#2e7d32; font-size:0.88em;", paste(n, "mineral choices available"))
    } else {
      tags$span(style = "color:#a15c00; font-size:0.88em;", "Mineral list is empty — click Reload mineral list.")
    }
  })

  quote_web_term <- function(x) {
    x <- trimws(as.character(x %||% ""))
    x <- x[nzchar(x)]
    ifelse(grepl("\\s", x), paste0('"', gsub('"', "", x, fixed = TRUE), '"'), x)
  }

  web_reference_query <- reactive({
    topic <- quote_web_term(input$web_ref_topic %||% "")
    minerals <- quote_web_term(input$web_ref_minerals %||% character(0))
    rocks <- quote_web_term(input$web_ref_rocks %||% character(0))
    parts <- c(topic, minerals)
    if (length(rocks) == 1) {
      parts <- c(parts, rocks)
    } else if (length(rocks) > 1) {
      logic <- if (identical(input$web_ref_rock_logic, "AND")) " AND " else " OR "
      parts <- c(parts, paste0("(", paste(rocks, collapse = logic), ")"))
    }
    paste(parts[nzchar(parts)], collapse = " ")
  })

  output$web_ref_query_preview <- renderUI({
    q <- web_reference_query()
    tags$div(
      tags$b("Query preview:"),
      tags$div(class = "web-query-preview", if (nzchar(q)) q else "Enter at least one search term.")
    )
  })

  output$web_ref_search_links <- renderUI({
    q <- web_reference_query()
    if (!nzchar(q)) return(NULL)
    encoded <- utils::URLencode(q, reserved = TRUE)
    tagList(
      tags$div(style = "margin-top:12px; display:flex; flex-wrap:wrap; gap:8px;",
        tags$a(class = "btn btn-primary", target = "_blank", rel = "noopener noreferrer",
               href = paste0("https://scholar.google.com/scholar?q=", encoded), icon("graduation-cap"), " Google Scholar"),
        tags$a(class = "btn btn-info", target = "_blank", rel = "noopener noreferrer",
               href = paste0("https://search.crossref.org/?q=", encoded), icon("search"), " Crossref"),
        tags$a(class = "btn btn-default", target = "_blank", rel = "noopener noreferrer",
               href = paste0("https://www.google.com/search?q=", encoded), icon("globe"), " Web search"),
        tags$a(class = "btn btn-default", target = "_blank", rel = "noopener noreferrer",
               href = "https://www.mindat.org/nsearch.php", icon("database"), " Mindat mineral search")
      )
    )
  })
  
  # Mineral-reference aliases let multiple names reuse one bibliography entry.
  # Keys are normalized by norm_mineral(); values match biblio$Mineral.
  mineral_reference_aliases <- c(
    aegerine = "Acmite",
    aegirine = "Acmite",
    titanite = "Sphene",
    sphene = "Titanite",
    allanite = "Orthite",
    orthite = "Allanite",
    muscovite = "Sericite",
    sericite = "Muscovite",
    sillimanite = "Fibrolite",
    fibrolite = "Sillimanite"
  )

  # Expand a selected mineral to its official name and any bibliography alias.
  expand_terms <- function(q) {
    q0 <- trimws(q %||% "")
    if (!nzchar(q0)) return(character(0))
    alias_target <- unname(mineral_reference_aliases[norm_mineral(q0)])
    alias_target <- alias_target[!is.na(alias_target) & nzchar(alias_target)]
    unique(c(to_full_mineral(q0), alias_target))
  }
  output$lib_rocktype_ui <- renderUI({
    df <- biblio_db()
    if (is.null(df) || !"Rocks_type" %in% names(df)) {
      return(selectInput("lib_rocktype", "Rocks_type (optional)", choices = c("Any"=""), selected = ""))
    }
    rt <- sort(unique(trimws(df$Rocks_type)))
    rt <- rt[nzchar(rt)]
    selectInput("lib_rocktype", "Rocks_type (optional)", choices = c("Any"="", rt), selected = "")
  })
  
  lib_filtered <- reactive({
    df <- biblio_db()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    q <- input$lib_mineral %||% ""
    rock_q <- trimws(input$lib_rocktype %||% "")
    
    terms <- expand_terms(q)
    if (length(terms) == 0 && !nzchar(rock_q)) return(df[0, , drop = FALSE])
    
    out <- df
    
    if (nzchar(rock_q) && "Rocks_type" %in% names(out)) {
      out <- out[grepl(rock_q, out$Rocks_type, ignore.case = TRUE), , drop = FALSE]
    }
    
    if (length(terms) > 0 && "Mineral" %in% names(out)) {
      term_norms <- vapply(terms, norm_mineral, character(1))
      mask_exact <- if ("Mineral_norm" %in% names(out)) out$Mineral_norm %in% term_norms else rep(FALSE, nrow(out))
      
      mask_like <- rep(FALSE, nrow(out))
      for (t in terms) mask_like <- mask_like | grepl(t, out$Mineral, ignore.case = TRUE)
      
      out <- out[mask_exact | mask_like, , drop = FALSE]
    }
    
    lim <- as.integer(input$lib_limit %||% 20)
    if (is.na(lim) || lim < 1) lim <- 20
    if (nrow(out) > lim) out <- out[seq_len(lim), , drop = FALSE]
    out
  })
  
  output$lib_table <- renderDT({
    df <- lib_filtered()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "No matches (or library not loaded)"), options = list(dom="t")))
    }
    show_cols <- intersect(c("Mineral","Descriptions","Rocks_type","Reference"), names(df))
    datatable(
      df[, show_cols, drop = FALSE],
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8, dom = "tip", autoWidth = TRUE)
    )
  })
  
  # ---------------- Mineral composition table ----------------
  minerals <- reactiveVal(
    data.frame(
      `Mineral` = character(0),
      `Vol. %` = character(0),
      `Morphology, features` = character(0),
      `Genesis` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
  
  output$mineral_table <- renderDT({
    datatable(
      minerals(),
      selection = "single",
      editable = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom = "tip",
        autoWidth = TRUE,
        ordering = FALSE
      )
    )
  })
  mineral_table_proxy <- DT::dataTableProxy("mineral_table")

  observeEvent(input$mineral_table_rows_selected, {
    sel <- input$mineral_table_rows_selected
    if (length(sel) != 1) return()
    df <- minerals()
    if (nrow(df) < sel) return()

    selected_mineral <- df[sel, "Mineral"] %||% ""
    ch <- isolate(mineral_choices_rv())
    if (nzchar(selected_mineral) && !(selected_mineral %in% ch)) {
      ch <- unique(c(selected_mineral, ch))
    }

    updateSelectizeInput(session, "m_mineral", choices = ch, selected = selected_mineral, server = TRUE)
    updateTextInput(session, "m_vol",   value = df[sel, "Vol. %"] %||% "")
    refresh_morphology_choices(mineral_name = selected_mineral, selected = df[sel, "Morphology, features"] %||% "")
    refresh_genesis_choices(selected = df[sel, "Genesis"] %||% "")
  })
  
  observeEvent(input$add_mineral, {
    df <- minerals()
    
    new_row <- data.frame(
      `Mineral` = to_full_mineral(input$m_mineral),
      `Vol. %` = input$m_vol %||% "",
      `Morphology, features` = input$m_morph %||% "",
      `Genesis` = input$m_gen %||% "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    minerals(rbind(df, new_row))
    
    # keep input shown as FULL after add (if abbreviation was typed)
    updateSelectizeInput(session, "m_mineral", choices = isolate(mineral_choices_rv()), selected = new_row$`Mineral`, server = TRUE)
    refresh_genesis_choices(selected = new_row$`Genesis`)
    refresh_morphology_choices(mineral_name = new_row$`Mineral`, selected = new_row$`Morphology, features`)
  })
  
  observeEvent(input$update_mineral, {
    sel <- input$mineral_table_rows_selected
    if (length(sel) != 1) return()
    df <- minerals()
    if (nrow(df) < sel) return()
    
    full_m <- to_full_mineral(input$m_mineral)
    
    df[sel, "Mineral"] <- full_m
    df[sel, "Vol. %"] <- input$m_vol %||% ""
    df[sel, "Morphology, features"] <- input$m_morph %||% ""
    df[sel, "Genesis"] <- input$m_gen %||% ""
    minerals(df)

    ch <- isolate(mineral_choices_rv())
    if (nzchar(full_m) && !(full_m %in% ch)) {
      ch <- unique(c(full_m, ch))
    }
    
    updateSelectizeInput(session, "m_mineral", choices = ch, selected = full_m, server = TRUE)
    refresh_genesis_choices(selected = input$m_gen %||% "")
    refresh_morphology_choices(mineral_name = full_m, selected = input$m_morph %||% "")
  })
  
  observeEvent(input$del_row, {
    sel <- input$mineral_table_rows_selected
    if (length(sel) != 1) return()
    df <- minerals()
    if (nrow(df) == 0) return()
    minerals(df[-sel, , drop = FALSE])
  })
  observeEvent(input$move_mineral_up, {
    sel <- input$mineral_table_rows_selected
    if (length(sel) != 1) return()

    df <- minerals()
    if (nrow(df) < 2) return()
    if (sel <= 1) return()

    new_df <- df
    new_df[c(sel - 1, sel), ] <- new_df[c(sel, sel - 1), , drop = FALSE]

    minerals(new_df)
    DT::selectRows(mineral_table_proxy, sel - 1)
  })

  observeEvent(input$move_mineral_down, {
    sel <- input$mineral_table_rows_selected
    if (length(sel) != 1) return()

    df <- minerals()
    if (nrow(df) < 2) return()
    if (sel >= nrow(df)) return()

    new_df <- df
    new_df[c(sel, sel + 1), ] <- new_df[c(sel + 1, sel), , drop = FALSE]

    minerals(new_df)
    DT::selectRows(mineral_table_proxy, sel + 1)
  })
  observeEvent(input$clear_mineral, {
    updateSelectizeInput(session, "m_mineral", choices = isolate(mineral_choices_rv()), selected = "", server = TRUE)
    updateTextInput(session, "m_vol", value = "")
    refresh_morphology_choices(mineral_name = "", selected = "")
    refresh_genesis_choices(selected = "")
  })

  # ---------------- Texture components / textural history (V56) ----------------
  empty_texture_components_df <- function() {
    data.frame(
      `Texture` = character(0),
      `Importance` = character(0),
      `Domain` = character(0),
      `Interpretation` = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  texture_components <- reactiveVal(empty_texture_components_df())

  output$texture_history_status_ui <- renderUI({
    df <- texture_components()
    n <- if (is.null(df) || nrow(df) == 0) 0L else {
      keep <- nzchar(trimws(as.character(df[["Texture"]] %||% ""))) |
        nzchar(trimws(as.character(df[["Interpretation"]] %||% "")))
      sum(keep, na.rm = TRUE)
    }

    texture_names <- character(0)
    if (!is.null(df) && nrow(df) > 0 && "Texture" %in% names(df)) {
      texture_names <- trimws(as.character(df[["Texture"]]))
      texture_names <- texture_names[!is.na(texture_names) & nzchar(texture_names)]
      texture_names <- texture_names[!duplicated(tolower(texture_names))]
    }

    button_label <- if (n > 0) paste0("Edit textural history (", n, ")") else "Textural history..."
    button_class <- if (n > 0) "btn-info" else "btn-default"

    status_text <- if (n == 0) {
      "No recognized texture components yet."
    } else if (length(texture_names) > 0) {
      shown <- paste(head(texture_names, 3), collapse = "; ")
      extra <- if (length(texture_names) > 3) paste0("; +", length(texture_names) - 3, " more") else ""
      paste0("Recognized: ", shown, extra)
    } else {
      paste0(n, if (n == 1) " texture component saved." else " texture components saved.")
    }

    tags$div(
      style = "margin-top: -6px; margin-bottom: 8px;",
      actionButton("open_texture_editor", button_label, icon = icon("layer-group"), class = button_class),
      tags$div(class = "small-note", status_text)
    )
  })

  texture_component_choices <- function() {
    tt2 <- textures_db()
    choices <- if (!is.null(tt2) && "texture_title" %in% names(tt2)) {
      sort(unique(as.character(tt2$texture_title)))
    } else character(0)
    cur_primary <- trimws(input$texture %||% "")
    cur_tbl <- isolate(texture_components())
    cur_tbl_textures <- if (!is.null(cur_tbl) && nrow(cur_tbl) > 0 && "Texture" %in% names(cur_tbl)) {
      as.character(cur_tbl$Texture)
    } else character(0)
    unique(c(choices, cur_primary, cur_tbl_textures))
  }

  output$texture_components_table <- DT::renderDT({
    DT::datatable(
      texture_components(),
      selection = "single",
      editable = FALSE,
      rownames = FALSE,
      options = list(pageLength = 6, dom = "tip", autoWidth = TRUE, ordering = FALSE)
    )
  })

  texture_components_proxy <- DT::dataTableProxy("texture_components_table")

  observeEvent(input$open_texture_editor, {
    showModal(modalDialog(
      title = "Textural history / recognized texture components",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      ),
      tags$p(class = "small-note",
             "The main 4b field remains the Primary texture. Use this table only when a sample has several texture components or overprints."),
      fluidRow(
        column(4,
               selectizeInput(
                 "tc_texture", "Texture",
                 choices = texture_component_choices(),
                 selected = NULL,
                 options = list(create = TRUE, persist = TRUE, placeholder = "e.g., relict gabbroic / nematoblastic ...")
               )
        ),
        column(3,
               selectInput(
                 "tc_importance", "Importance",
                 choices = c("", "dominant", "common", "moderate", "minor", "local", "trace", "relict", "overprint"),
                 selected = ""
               )
        ),
        column(5,
               textInput("tc_domain", "Domain", placeholder = "e.g., whole thin section / amphibole-rich domains / veinlets")
        )
      ),
      textAreaInput(
        "tc_interpretation",
        "Interpretation",
        rows = 3,
        width = "100%",
        placeholder = "e.g., preserved igneous framework / metamorphic recrystallization / hydrothermal replacement"
      ),
      tags$div(
        style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:10px;",
        actionButton("tc_add", "Add row"),
        actionButton("tc_update", "Update selected"),
        actionButton("tc_delete", "Delete selected"),
        actionButton("tc_move_up", "Move up"),
        actionButton("tc_move_down", "Move down"),
        actionButton("tc_from_primary", "Create from primary texture")
      ),
      DT::DTOutput("texture_components_table")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$texture_components_table_rows_selected, {
    sel <- input$texture_components_table_rows_selected
    if (length(sel) != 1) return()
    df <- texture_components()
    if (nrow(df) < sel) return()
    ch <- texture_component_choices()
    tx <- as.character(df[sel, "Texture"] %||% "")
    if (nzchar(tx) && !(tx %in% ch)) ch <- unique(c(tx, ch))
    updateSelectizeInput(session, "tc_texture", choices = ch, selected = tx, server = TRUE)
    updateSelectInput(session, "tc_importance", selected = as.character(df[sel, "Importance"] %||% ""))
    updateTextInput(session, "tc_domain", value = as.character(df[sel, "Domain"] %||% ""))
    updateTextAreaInput(session, "tc_interpretation", value = as.character(df[sel, "Interpretation"] %||% ""))
  }, ignoreInit = TRUE)

  texture_component_new_row <- function(texture = NULL) {
    data.frame(
      `Texture` = trimws(texture %||% input$tc_texture %||% ""),
      `Importance` = trimws(input$tc_importance %||% ""),
      `Domain` = trimws(input$tc_domain %||% ""),
      `Interpretation` = trimws(input$tc_interpretation %||% ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  observeEvent(input$tc_add, {
    new_row <- texture_component_new_row()
    if (!nzchar(new_row$Texture) && !nzchar(new_row$Interpretation)) {
      showNotification("Texture component is empty.", type = "warning", duration = 3)
      return()
    }
    texture_components(rbind(texture_components(), new_row))
  }, ignoreInit = TRUE)

  observeEvent(input$tc_update, {
    sel <- input$texture_components_table_rows_selected
    if (length(sel) != 1) return()
    df <- texture_components()
    if (nrow(df) < sel) return()
    df[sel, ] <- texture_component_new_row()
    texture_components(df)
  }, ignoreInit = TRUE)

  observeEvent(input$tc_delete, {
    sel <- input$texture_components_table_rows_selected
    if (length(sel) != 1) return()
    df <- texture_components()
    if (nrow(df) == 0) return()
    texture_components(df[-sel, , drop = FALSE])
  }, ignoreInit = TRUE)

  observeEvent(input$tc_move_up, {
    sel <- input$texture_components_table_rows_selected
    if (length(sel) != 1 || sel <= 1) return()
    df <- texture_components()
    if (nrow(df) < 2) return()
    df[c(sel - 1, sel), ] <- df[c(sel, sel - 1), , drop = FALSE]
    texture_components(df)
    DT::selectRows(texture_components_proxy, sel - 1)
  }, ignoreInit = TRUE)

  observeEvent(input$tc_move_down, {
    sel <- input$texture_components_table_rows_selected
    if (length(sel) != 1) return()
    df <- texture_components()
    if (nrow(df) < 2 || sel >= nrow(df)) return()
    df[c(sel, sel + 1), ] <- df[c(sel + 1, sel), , drop = FALSE]
    texture_components(df)
    DT::selectRows(texture_components_proxy, sel + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$tc_from_primary, {
    primary <- trimws(input$texture %||% "")
    if (!nzchar(primary)) {
      showNotification("Primary texture field is empty.", type = "warning", duration = 3)
      return()
    }
    df <- texture_components()
    already <- nrow(df) > 0 && any(tolower(trimws(df$Texture)) == tolower(primary))
    if (already) {
      showNotification("Primary texture is already in the components table.", type = "message", duration = 3)
      return()
    }
    new_row <- data.frame(
      `Texture` = primary,
      `Importance` = "dominant",
      `Domain` = "whole thin section",
      `Interpretation` = "primary / main texture field",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    texture_components(rbind(df, new_row))
  }, ignoreInit = TRUE)
  
  # ---------------- Images ----------------
  img_dir <- file.path(tempdir(), "petro_shiny_images")
  if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)
  addResourcePath("petro_img", img_dir)
  addResourcePath("db_img", DB_IMAGES_DIR)


# Image optimization (to keep DB + exports light)
MAX_IMAGE_MB <- 4            # maximum size of stored image file (MB)
MAX_IMAGE_DIM <- 2500        # max width/height in pixels (long side); aspect preserved
JPEG_QUALITY_DEFAULT <- 85   # starting JPEG quality for re-encoding
JPEG_QUALITY_MIN <- 60       # don't go below this

max_image_bytes <- MAX_IMAGE_MB * 1024 * 1024

# Helpers to read image optimization settings from UI (fallback to defaults)
get_img_opt_params <- function(input) {
  max_mb <- suppressWarnings(as.numeric(input$max_image_mb_setting %||% MAX_IMAGE_MB))
  max_dim <- suppressWarnings(as.numeric(input$max_image_dim_setting %||% MAX_IMAGE_DIM))
  qd <- suppressWarnings(as.numeric(input$jpeg_quality_default_setting %||% JPEG_QUALITY_DEFAULT))
  qm <- suppressWarnings(as.numeric(input$jpeg_quality_min_setting %||% JPEG_QUALITY_MIN))
  # sanity
  if (is.na(max_mb) || max_mb <= 0) max_mb <- MAX_IMAGE_MB
  if (is.na(max_dim) || max_dim < 300) max_dim <- MAX_IMAGE_DIM
  if (is.na(qd) || qd < 1) qd <- JPEG_QUALITY_DEFAULT
  if (is.na(qm) || qm < 1) qm <- JPEG_QUALITY_MIN
  list(
    max_bytes = max_mb * 1024 * 1024,
    max_dim = max_dim,
    q_default = qd,
    q_min = qm
  )
}


optimize_image_to_jpeg <- function(in_path, out_base, max_bytes = max_image_bytes, max_dim = max_dim, q_default = JPEG_QUALITY_DEFAULT, q_min = JPEG_QUALITY_MIN) {
  # Returns list(path=..., ext="jpg", ok=TRUE/FALSE, msg=...)
  if (!file.exists(in_path)) return(list(ok = FALSE, msg = "Input file not found."))
  if (!requireNamespace("magick", quietly = TRUE)) {
    # No magick: just copy as-is (but still enforce size limit)
    out_path <- paste0(out_base, ".", tolower(file_ext(in_path) %||% "jpg"))
    file.copy(in_path, out_path, overwrite = TRUE)
    sz <- file.info(out_path)$size
    if (!is.na(sz) && sz > max_bytes) {
      unlink(out_path)
      return(list(ok = FALSE, msg = paste0("Image is too large (", format_bytes(sz), "). Install 'magick' or upload a smaller file.")))
    }
    return(list(ok = TRUE, path = out_path, ext = tolower(file_ext(out_path)), msg = "Copied without optimization (magick not installed)."))
  }

  # Read, flatten alpha, resize, then iteratively compress to fit size.
  img <- tryCatch(magick::image_read(in_path), error = function(e) NULL)
  if (is.null(img)) return(list(ok = FALSE, msg = "Failed to read image."))

  # Flatten alpha if any (clipboard PNG etc.)
  img <- tryCatch(magick::image_background(img, "white"), error = function(e) img)
  img <- tryCatch(magick::image_flatten(img), error = function(e) img)

  # Resize (keep aspect), only if larger than max_dim
  info <- tryCatch(magick::image_info(img), error = function(e) NULL)
  if (!is.null(info) && (info$width[1] > max_dim || info$height[1] > max_dim)) {
    img <- magick::image_resize(img, paste0(max_dim, "x", max_dim, ">"))
  }

  q <- q_default
  out_path <- paste0(out_base, ".jpg")

  write_jpg <- function(quality) {
    tryCatch({
      magick::image_write(img, path = out_path, format = "jpeg", quality = quality)
      TRUE
    }, error = function(e) FALSE)
  }

  if (!write_jpg(q)) return(list(ok = FALSE, msg = "Failed to write optimized JPEG."))
  sz <- file.info(out_path)$size

  # First: reduce quality
  while (!is.na(sz) && sz > max_bytes && q > q_min) {
    q <- max(q_min, q - 10)
    write_jpg(q)
    sz <- file.info(out_path)$size
  }

  # If still too large: resize progressively and re-encode
  shrink_iter <- 0
  while (!is.na(sz) && sz > max_bytes && shrink_iter < 4) {
    shrink_iter <- shrink_iter + 1
    img <- magick::image_resize(img, "85%")
    q <- min(80, q)  # keep reasonable quality
    write_jpg(q)
    sz <- file.info(out_path)$size
  }

  if (!is.na(sz) && sz > max_bytes) {
    unlink(out_path)
    return(list(ok = FALSE, msg = paste0("Image remains too large after optimization (", format_bytes(sz), ").")))
  }

  list(ok = TRUE, path = out_path, ext = "jpg", msg = paste0("Optimized to ", format_bytes(sz), " (q=", q, ")."))
}
  
  METADATA_SEPARATOR <- "|"
  BYTES_PER_UNIT <- 1024
  
  format_bytes <- function(sz) {
    if (is.null(sz)) return(NA_character_)
    sz <- suppressWarnings(as.numeric(sz))
    if (is.na(sz) || !is.finite(sz)) return(NA_character_)
    if (sz <= 0) return("0 B")
    units <- c("B","KB","MB","GB","TB")
    pow <- floor(log(sz, BYTES_PER_UNIT))
    pow <- max(0, min(length(units) - 1, pow))
    sprintf("%s %s", format(round(sz / (BYTES_PER_UNIT ^ pow), 1), trim = TRUE), units[pow + 1])
  }
  
  read_img_meta <- function(path) {
    info <- tryCatch(file.info(path), error = function(e) NULL)
    dims <- tryCatch(read_img_dims_px(path), error = function(e) c(NA_real_, NA_real_))
    list(
      width = dims[1],
      height = dims[2],
      size = if (!is.null(info)) as.numeric(info$size) else NA_real_,
      mtime = if (!is.null(info)) info$mtime else as.POSIXct(NA)
    )
  }
  
  format_img_meta <- function(meta) {
    if (is.null(meta)) return("Info unavailable")
    metadata_separator <- METADATA_SEPARATOR
    valid_dim <- function(x) {
      if (is.null(x)) return(FALSE)
      x <- suppressWarnings(as.numeric(x))
      is.finite(x) && x > 0
    }
    parts <- character(0)
    if (valid_dim(meta$width) && valid_dim(meta$height)) {
      parts <- c(parts, paste0(meta$width, "x", meta$height, " px"))
    }
    if (!is.null(meta$size) && !is.na(meta$size)) {
      fb <- format_bytes(meta$size)
      if (!is.na(fb)) parts <- c(parts, fb)
    }
    if (!is.null(meta$mtime) && !is.na(meta$mtime)) {
      parts <- c(parts, format(meta$mtime, "%Y-%m-%d %H:%M"))
    }
    if (length(parts) == 0) "Info unavailable" else paste(parts, collapse = paste0(" ", metadata_separator, " "))
  }
  
  img_pool <- reactiveVal(list())
  img_choices <- reactiveVal(character(0))

  limited_selected_image_ids <- function() {
    max_n <- suppressWarnings(as.integer(input$max_sel_images %||% 10))
    if (is.na(max_n) || max_n < 1) max_n <- 10
    utils::head(img_choices(), max_n)
  }
  loaded_caps <- reactiveVal(list())   # legacy: captions loaded from DB by slot
  caption_store <- reactiveVal(list()) # captions keyed by stable image id during the Shiny session
  loading_images <- reactiveVal(FALSE)  # blocks checkbox->selection sync during programmatic loads
  
  safe_name <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)
  
  clear_img_dir <- function() {
    old <- list.files(img_dir, full.names = TRUE)
    if (length(old) > 0) unlink(old, force = TRUE)
  }

  # ---- Caption preservation helpers ----
  # The old code used cap_1, cap_2, ... captions by display slot.
  # When the image list was rebuilt after upload / reorder / checkbox changes,
  # Shiny recreated textAreaInput() controls and their values could fall back
  # to loaded_caps() or blank values. This is why captions sometimes disappeared.
  #
  # New strategy:
  #   - keep a caption_store keyed by the stable image id;
  #   - before changing image pool/selection/order, copy current cap_ values into it;\n  #   - caption input IDs are based on image_id, not slot/order;
  #   - when rendering caption boxes, restore value from caption_store[[image_id]];
  #   - when saving to SQLite, read captions by image id, not only by slot.
  sync_captions_from_inputs <- function() {
    pool <- isolate(img_pool())
    sel_ids <- isolate(img_choices())
    if (length(pool) == 0 || length(sel_ids) == 0) return(invisible(FALSE))

    by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
    store <- isolate(caption_store())
    changed <- FALSE

    for (slot in seq_along(sel_ids)) {
      id <- sel_ids[slot]
      if (!id %in% names(by_id)) next

      # V49.2 fix:
      # Captions must be read from input IDs based on stable image_id, not display slot.
      # Slot IDs cap_1/cap_2 are kept only as a legacy fallback.
      cap_val <- input[[paste0("cap_", id)]]
      if (is.null(cap_val)) {
        cap_val <- input[[paste0("cap_", slot)]]
      }

      if (!is.null(cap_val)) {
        cap_val <- as.character(cap_val %||% "")
        old_val <- store[[id]] %||% ""
        if (!identical(old_val, cap_val)) {
          store[[id]] <- cap_val
          changed <- TRUE
        }
      }
    }

    if (isTRUE(changed)) caption_store(store)
    invisible(changed)
  }

  get_caption_for_image_id <- function(id, slot = NA_integer_) {
    store <- isolate(caption_store())
    if (!is.null(store[[id]])) return(as.character(store[[id]] %||% ""))

    # First fallback: current stable image-id caption input.
    cap_val <- input[[paste0("cap_", id)]]
    if (!is.null(cap_val)) return(as.character(cap_val %||% ""))

    # Second fallback: old slot-based caption input, only for compatibility.
    if (!is.na(slot)) {
      cap_val <- input[[paste0("cap_", slot)]]
      if (!is.null(cap_val)) return(as.character(cap_val %||% ""))
    }

    pool <- isolate(img_pool())
    by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
    if (id %in% names(by_id) && !is.null(by_id[[id]]$caption)) {
      return(as.character(by_id[[id]]$caption %||% ""))
    }

    ""
  }

  # Keep caption_store updated while the user types.
  observe({
    sync_captions_from_inputs()
  })
  
  observeEvent(input$clear_images, {
    if (stop_if_read_only("clearing the image container")) return()
    sync_captions_from_inputs()
    clear_img_dir()
    img_pool(list())
    img_choices(character(0))
    loaded_caps(list())
    caption_store(list())
  })
  
  pool_sorted <- reactive({
    pool <- img_pool()
    if (length(pool) == 0) return(pool)
    ord <- vapply(pool, function(x) as.numeric(x$order %||% 999999), numeric(1))
    nm  <- vapply(pool, function(x) tolower(x$name %||% ""), character(1))
    pool[order(ord, nm)]
  })
  
  observeEvent(input$img_upload, {
    if (stop_if_read_only("image upload")) return()
    req(input$img_upload)
    sync_captions_from_inputs()

    files <- input$img_upload

    # IMPORTANT: do not clear img_dir(), img_pool(), or img_choices() here.
    # Upload is now an APPEND operation. Use the explicit "Clear image container"
    # button when you want to replace the whole image set.
    pool <- img_pool()
    selected_ids <- img_choices()
    new_ids <- character(0)

    next_order <- if (length(pool) == 0) 1 else {
      max(vapply(pool, function(x) as.numeric(x$order %||% 0), numeric(1)), na.rm = TRUE) + 1
    }
    if (!is.finite(next_order) || is.na(next_order)) next_order <- 1

    for (i in seq_len(nrow(files))) {
      orig_name <- files$name[i]
      stored_base <- paste0(sprintf("%03d", next_order + i - 1), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", safe_name(orig_name))
      id <- make_img_id()

      # Optimize & normalize to JPEG to keep storage small
      stored_noext <- sub("\\.[^.]+$", "", stored_base)
      out_base <- file.path(img_dir, stored_noext)
      params <- get_img_opt_params(input)
      opt <- optimize_image_to_jpeg(
        files$datapath[i], out_base,
        max_bytes = params$max_bytes,
        max_dim = params$max_dim,
        q_default = params$q_default,
        q_min = params$q_min
      )

      if (!isTRUE(opt$ok)) {
        showNotification(paste0("Image skipped: ", orig_name, " — ", opt$msg), type = "error", duration = 8)
        next
      }

      dest <- opt$path
      final_ext <- opt$ext %||% "jpg"

      pool[[length(pool) + 1]] <- list(
        id = id,
        name = orig_name,
        ext = final_ext,
        url = paste0("petro_img/", basename(dest)),
        path = dest,
        is_converted = TRUE,
        origin = "upload",
        order = next_order + i - 1,
        caption = "",
        meta = read_img_meta(dest)
      )

      new_ids <- c(new_ids, id)
    }

    img_pool(pool)
    if (length(new_ids) > 0) {
      img_choices(unique(c(selected_ids, new_ids)))
    }
    showNotification("Image(s) added to container. Existing captions were preserved.", type = "message", duration = 4)
  }, ignoreInit = TRUE)
  
  observeEvent(input$clipboard_image, {
    if (stop_if_read_only("clipboard image import")) return()
    req(input$clipboard_image$data)
    sync_captions_from_inputs()
    
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      showNotification("Need base64enc for clipboard paste: install.packages('base64enc')",
                       type = "error", duration = 10)
      return()
    }
    
    data_url <- input$clipboard_image$data
    mime <- input$clipboard_image$type %||% "image/png"
    
    ext <- switch(
      tolower(mime),
      "image/png"  = "png",
      "image/jpeg" = "jpg",
      "image/jpg"  = "jpg",
      "image/webp" = "webp",
      "image/tiff" = "tiff",
      "png"
    )
    
    b64 <- sub("^data:.*;base64,", "", data_url)
    raw <- base64enc::base64decode(b64)
    
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    dest <- file.path(img_dir, paste0("clip_", stamp, "_", sprintf("%04d", sample.int(9999, 1)), ".", ext))
    writeBin(raw, dest)
    
    
# Optimize clipboard image to JPEG and enforce size limit
stored_noext <- paste0("clip_", stamp, "_", sprintf("%04d", sample.int(9999, 1)))
out_base <- file.path(img_dir, stored_noext)
params <- get_img_opt_params(input)
    opt <- optimize_image_to_jpeg(dest, out_base, max_bytes = params$max_bytes, max_dim = params$max_dim, q_default = params$q_default, q_min = params$q_min)
unlink(dest)
if (!isTRUE(opt$ok)) {
  showNotification(paste0("Clipboard image skipped — ", opt$msg), type = "error", duration = 8)
  return()
}
final_path <- opt$path
final_ext <- opt$ext %||% "jpg"

    pool <- img_pool()
    next_order <- if (length(pool) == 0) 1 else (max(vapply(pool, function(x) as.numeric(x$order %||% 0), numeric(1))) + 1)
    
    pool[[length(pool) + 1]] <- list(
      id = make_img_id(),
      name = paste0("Clipboard ", stamp),
      ext = final_ext,
      url = paste0("petro_img/", basename(final_path)),
      path = final_path,
      is_converted = TRUE,
      origin = "clipboard",
      order = next_order,
      caption = "",
      meta = read_img_meta(final_path)
    )
    
    img_pool(pool)
    showNotification("Image pasted into container.", type = "message", duration = 3)
  }, ignoreInit = TRUE)
  
  output$image_gallery_ui <- renderUI({
    pool <- pool_sorted()
    sel_ids <- img_choices()
    if (length(pool) == 0) return(tags$p(class = "small-note", "No images uploaded/pasted yet."))
    
    tags$div(
      class = "img-grid",
      lapply(pool, function(item) {
        id <- item$id
        chk_id <- paste0("chk_", id)
        ord_id <- paste0("ord_", id)
        meta_text <- htmltools::htmlEscape(format_img_meta(item$meta))
        
        tags$div(
          class = "img-card",
          tags$div(class = "img-thumb-wrap", tags$img(src = item$url, class = "img-thumb")),
          tags$div(class = "img-meta", meta_text),
          checkboxInput(
            inputId = chk_id,
            label = paste0(item$name, if (isTRUE(item$is_converted)) " (TIFF→PNG)" else ""),
            value = id %in% sel_ids
          ),
          numericInput(inputId = ord_id, label = "Order", value = as.numeric(item$order %||% 999), min = 1, step = 1)
        )
      })
    )
  })
  
  observe({
    if (isTRUE(loading_images())) return()
    sync_captions_from_inputs()
    pool <- img_pool()
    if (length(pool) == 0) return()
    changed <- FALSE
    for (k in seq_along(pool)) {
      id <- pool[[k]]$id
      ord_val <- input[[paste0("ord_", id)]]
      if (!is.null(ord_val) && !is.na(ord_val)) {
        ord_val <- as.numeric(ord_val)
        if (!identical(pool[[k]]$order, ord_val)) {
          pool[[k]]$order <- ord_val
          changed <- TRUE
        }
      }
    }
    if (changed) img_pool(pool)
  })
  
  observe({
    sync_captions_from_inputs()
    pool <- img_pool()
    if (length(pool) == 0) return()
    
    max_n <- as.integer(input$max_sel_images %||% 10)
    if (is.na(max_n) || max_n < 1) max_n <- 10
    
    selected_ids <- character(0)
    any_chk <- FALSE
    for (item in pool) {
      id <- item$id
      v <- input[[paste0("chk_", id)]]
      if (!is.null(v)) any_chk <- TRUE
      if (isTRUE(v)) selected_ids <- c(selected_ids, id)
    }
    # If checkbox inputs are not yet created (e.g., container tab not rendered),
    # do NOT override current selection.
    if (!any_chk) return()
    
    if (length(selected_ids) > 0) {
      by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
      ord <- vapply(selected_ids, function(id) as.numeric(by_id[[id]]$order %||% 999999), numeric(1))
      nm  <- vapply(selected_ids, function(id) tolower(by_id[[id]]$name %||% ""), character(1))
      selected_ids <- selected_ids[order(ord, nm)]
    }
    
    if (length(selected_ids) > max_n) selected_ids <- selected_ids[1:max_n]
    
    if (!identical(selected_ids, img_choices())) {
      img_choices(selected_ids)
      for (item in pool) {
        id <- item$id
        updateCheckboxInput(session, paste0("chk_", id), value = id %in% selected_ids)
      }
    }
  })

  # Enforce the selected-image limit even when images are appended, loaded from
  # SQLite, or when the user lowers the limit before opening the container tab.
  observe({
    max_n <- suppressWarnings(as.integer(input$max_sel_images %||% 10))
    if (is.na(max_n) || max_n < 1) max_n <- 10

    selected <- img_choices()
    if (length(selected) <= max_n) return()

    keep <- selected[seq_len(max_n)]
    img_choices(keep)
    pool <- img_pool()
    for (item in pool) {
      updateCheckboxInput(session, paste0("chk_", item$id), value = item$id %in% keep)
    }
    showNotification(
      paste0("Selected images were limited to ", max_n, "."),
      type = "warning",
      duration = 4
    )
  })
  
  output$selected_images_ui <- renderUI({
    pool <- img_pool()
    sel_ids <- img_choices()
    if (length(pool) == 0 || length(sel_ids) == 0) {
      return(tags$p(class = "small-note",
                    "No images selected yet. Go to “Image container” tab and tick images."))
    }
    by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
    
    lapply(seq_along(sel_ids), function(slot) {
      id <- sel_ids[slot]
      if (!id %in% names(by_id)) return(NULL)
      item <- by_id[[id]]
      tagList(
        tags$div(class = "field-label", paste0("Image ", slot, " (Order ", item$order, "): ", item$name)),
        tags$div(class = "img-selected-wrap", tags$img(src = item$url, class = "img-selected")),
        textAreaInput(paste0("cap_", id), "Caption",
                      value = get_caption_for_image_id(id, slot),
                      placeholder = "Add a caption to the photo...", rows = 2, width = "100%"),
        tags$hr()
      )
    })
  })
  

# --- Image path helpers (supports mixed absolute/relative file_path values) ---
normalize_slashes <- function(p) {
  if (is.null(p)) return(NA_character_)
  p <- as.character(p)
  p <- trimws(p)
  p <- gsub("\\", "/", p, fixed = TRUE)
  p
}

is_abs_path <- function(p) {
  if (is.null(p) || !nzchar(p)) return(FALSE)
  p <- normalize_slashes(p)
  grepl("^[A-Za-z]:/", p) || startsWith(p, "/")
}

resolve_image_path <- function(p) {
  p <- normalize_slashes(p)
  if (is.null(p) || !nzchar(p)) return(NA_character_)

  # Absolute path stored in DB
  if (is_abs_path(p)) {
    if (file.exists(p)) return(p)
    # maybe bad slashes; try normalizePath
    cand <- tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
    if (file.exists(cand)) return(cand)
  }

  # Relative path stored in DB (preferred)
  cand1 <- file.path(dirname(DB_PATH), p)
  if (file.exists(cand1)) return(cand1)

  # Sometimes DB stores path relative to images dir (e.g. "01.jpg")
  cand2 <- file.path(DB_IMAGES_DIR, p)
  if (file.exists(cand2)) return(cand2)

  cand3 <- file.path(DB_IMAGES_DIR, basename(p))
  if (file.exists(cand3)) return(cand3)

  NA_character_
}
  # ---------------- DOCX Export via template ----------------
  add_heading <- function(doc, text, level = 1) {
    fs <- switch(as.character(level), "1" = 16, "2" = 13, "3" = 11, 11)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(text, prop = officer::fp_text(bold = TRUE, font.size = fs)))
    )
    officer::body_add_par(doc, "", style = "Normal")
  }
  
  add_label_value <- function(doc, label, value) {
    officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(paste0(label, ": "), prop = officer::fp_text(bold = TRUE)),
        officer::ftext(value %||% "")
      )
    )
  }
  
  get_max_image_width_in <- function(default_width = 9.6, margin_left = 0.7, margin_right = 0.7) {
    max(3, default_width - margin_left - margin_right)
  }
  
  read_img_dims_px <- function(path) {
    if (!requireNamespace("magick", quietly = TRUE)) return(c(NA_real_, NA_real_))
    im <- magick::image_read(path)
    info <- magick::image_info(im)[1, ]
    c(as.numeric(info$width), as.numeric(info$height))
  }
  
  add_img_autoscale <- function(doc, path, max_w_in) {
    dims <- read_img_dims_px(path)
    if (any(is.na(dims))) {
      w <- max_w_in
      h <- max(2, w * 0.6)
    } else {
      w_px <- dims[1]; h_px <- dims[2]
      w <- max_w_in
      h <- (h_px / w_px) * w
    }
    officer::body_add_img(doc, src = path, width = w, height = h)
  }
  
  get_caption_style <- function(doc) {
    si <- tryCatch(officer::styles_info(doc), error = function(e) NULL)
    if (is.null(si) || !"style_name" %in% names(si)) return("Normal")
    if ("caption" %in% si$style_name) return("caption")
    if ("Caption" %in% si$style_name) return("Caption")
    "Normal"
  }
  
  add_caption_par <- function(doc, fig_no, caption_text, style_name) {
    full <- paste0("Figure ", fig_no, ": ", sanitize_text(caption_text %||% ""))
    officer::body_add_par(doc, full, style = style_name)
  }
  
  safe_replace_header_body <- function(doc, old_value, new_value) {
    doc <- tryCatch(officer::headers_replace_all_text(doc, old_value = old_value, new_value = new_value),
                    error = function(e) doc)
    doc <- tryCatch(officer::body_replace_all_text(doc, old_value = old_value, new_value = new_value, only_at_cursor = FALSE),
                    error = function(e) doc)
    doc
  }
  
  build_docx_from_template <- function(file_path) {
    # V49.3: DOCX export uses get_caption_for_image_id(), so captions follow image_id.
    tpl <- file.path(getwd(), "petro_template.docx")
    if (!file.exists(tpl)) stop("Template not found: petro_template.docx must be in the same folder as app.R")
    
    sid      <- sanitize_text(input$sample_id %||% "")
    project  <- sanitize_text(input$project_name %||% "")
    author   <- sanitize_text(input$author %||% "")
    date_str <- format(Sys.Date(), "%Y-%m-%d")
    
    doc <- officer::read_docx(path = tpl)
    
    doc <- safe_replace_header_body(doc, "<<SAMPLE_ID>>", sid)
    doc <- safe_replace_header_body(doc, "<<PROJECT>>",  project)
    doc <- safe_replace_header_body(doc, "<<AUTHOR>>",   author)
    doc <- safe_replace_header_body(doc, "<<DATE>>",     date_str)
    
    caption_style <- get_caption_style(doc)
    
    val_size <- suppressWarnings(as.numeric(input$docx_font_size_setting %||% 10))
    if (is.na(val_size) || val_size < 7) val_size <- 10
    fp_val <- officer::fp_text(font.size = val_size)

    # All user-entered report values use the configured font size. Headings,
    # labels, table typography and image captions retain their dedicated styles.
    add_label_value <- function(doc, label, value) {
      officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(
            paste0(sanitize_text(label %||% ""), ": "),
            prop = officer::fp_text(bold = TRUE)
          ),
          officer::ftext(sanitize_text(value %||% ""), prop = fp_val)
        )
      )
    }

    add_val_par <- function(doc, text) {
      t <- sanitize_text(text %||% "")
      officer::body_add_fpar(doc, officer::fpar(officer::ftext(t, prop = fp_val)))
    }


add_field_block <- function(doc, label, value, label_style = "Normal", value_style = "Normal") {
  label <- sanitize_text(label %||% "")
  value <- sanitize_text(value %||% "")

  # Label and value must be consecutive (no blank line between them),
  # and exactly one blank line AFTER the value.
  doc <- officer::body_add_par(doc, label, style = label_style)
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(value, prop = fp_val)))
  doc <- officer::body_add_par(doc, "", style = value_style)
  doc
}

    get_selected_lithology_info <- function() {
      unit_id <- suppressWarnings(as.integer(input$suggested_mapping_unit_id %||% NA_integer_))
      out <- list(
        classifier = sanitize_text(input$mapping_classifier %||% ""),
        lithology = "",
        classification = "",
        display = "",
        confidence = sanitize_text(input$mapping_unit_confidence %||% ""),
        comment = sanitize_text(mapping_unit_comment_rv() %||% "")
      )

      if (is.na(unit_id)) {
        out$display <- "Not assigned"
        return(out)
      }

      con_mu <- tryCatch(db_connect(), error = function(e) NULL)
      if (is.null(con_mu)) {
        out$display <- paste0("unit_id ", unit_id)
        return(out)
      }
      on.exit(try(dbDisconnect(con_mu), silent = TRUE), add = TRUE)
      db_init(con_mu)

      mu <- tryCatch(
        DBI::dbGetQuery(con_mu, "
          SELECT unit_id, classifier_name, source_id, class_id, unit_code, unit_name,
                 shortname, unit_description, source_reference, notes, is_active
          FROM mapping_units
          WHERE unit_id = :unit_id
          LIMIT 1
        ", params = list(unit_id = unit_id)),
        error = function(e) data.frame()
      )

      if (!is.null(mu) && nrow(mu) > 0) {
        out$classifier <- sanitize_text(mu$classifier_name[1] %||% out$classifier)
        out$lithology <- sanitize_text(mu$shortname[1] %||% "")
        out$classification <- sanitize_text(mu$unit_description[1] %||% mu$class_id[1] %||% "")
        out$display <- sanitize_text(mapping_unit_label(
          mu$unit_code[1] %||% "",
          mu$unit_name[1] %||% "",
          mu$shortname[1] %||% "",
          mu$unit_description[1] %||% "",
          mu$class_id[1] %||% ""
        ))
      } else {
        out$display <- paste0("unit_id ", unit_id)
      }

      if (!nzchar(trimws(out$display))) out$display <- "Not assigned"
      out
    }

    add_lithology_interpretation_docx <- function(doc) {
      li <- get_selected_lithology_info()

      doc <- add_heading(doc, "4c) Lithology interpretation", level = 2)
      doc <- add_label_value(doc, "Classifier", li$classifier)
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- add_label_value(doc, "Suggested lithological unit", li$display)
      doc <- officer::body_add_par(doc, "", style = "Normal")

      if (nzchar(trimws(li$lithology))) {
        doc <- add_label_value(doc, "Lithology", li$lithology)
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }
      if (nzchar(trimws(li$classification))) {
        doc <- add_label_value(doc, "Lithology classification", li$classification)
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }

      doc <- add_label_value(doc, "Confidence", li$confidence)
      doc <- officer::body_add_par(doc, "", style = "Normal")

      doc <- add_field_block(doc, "Comment / basis", li$comment)
      doc <- add_field_block(doc, "Classification source", lithology_classification_source)
      doc
    }

    doc <- add_heading(doc, "Petrographic Description", level = 1)
    
    doc <- add_field_block(doc, "1) Sample ID", sid)
    
    doc <- add_field_block(doc, "Project", project)
    
    doc <- add_field_block(doc, "Author", author)
    
    doc <- add_field_block(doc, "2a) Field Rock Name (field identification)", input$field_rock_name %||% "")
    doc <- add_field_block(doc, "2b) Field description", input$field_desc %||% "")
    
    doc <- add_heading(doc, "3) Coordinates", level = 2)
    doc <- add_label_value(doc, "Latitude",  sanitize_text(input$lat %||% ""))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- add_label_value(doc, "Longitude", sanitize_text(input$lon %||% ""))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    
    doc <- add_field_block(doc, "4) Rock name (microscope identification)", input$rock_name %||% "")
    
    doc <- add_field_block(doc, "4a) Grain size", input$grain_size %||% "")
    doc <- add_field_block(doc, "4b) Primary texture", input$texture %||% "")

    tdf <- texture_components()
    if (!is.null(tdf) && nrow(tdf) > 0) {
      doc <- add_heading(doc, "Textural history / recognized texture components", level = 2)
      tdf[] <- lapply(tdf, function(col) sanitize_text(as.character(col)))
      ft_tex <- flextable::flextable(tdf)
      ft_tex <- flextable::theme_zebra(ft_tex)
      ft_tex <- flextable::fontsize(ft_tex, size = 9, part = "all")
      ft_tex <- flextable::padding(ft_tex, padding = 3, part = "all")
      ft_tex <- flextable::align(ft_tex, align = "left", part = "all")
      ft_tex <- flextable::valign(ft_tex, valign = "top", part = "all")
      if ("Texture" %in% colnames(tdf))        ft_tex <- flextable::width(ft_tex, j = "Texture", width = 1.5)
      if ("Importance" %in% colnames(tdf))     ft_tex <- flextable::width(ft_tex, j = "Importance", width = 1.0)
      if ("Domain" %in% colnames(tdf))         ft_tex <- flextable::width(ft_tex, j = "Domain", width = 2.0)
      if ("Interpretation" %in% colnames(tdf)) ft_tex <- flextable::width(ft_tex, j = "Interpretation", width = 3.5)
      ft_tex <- flextable::set_table_properties(ft_tex, layout = "fixed", width = 1.0)
      doc <- flextable::body_add_flextable(doc, value = ft_tex)
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }

    # DOCX order fix:
    # recognized texture components belong to 4b) Primary texture,
    # so the table is exported before 4c) Lithology interpretation.
    doc <- add_lithology_interpretation_docx(doc)

    doc <- add_heading(doc, "5) Mineral composition", level = 2)
    df <- minerals()
    if (nrow(df) == 0) {
      doc <- officer::body_add_par(doc, "No mineral table data.", style = "Normal")
    } else {
      df[] <- lapply(df, function(col) sanitize_text(as.character(col)))
      ft <- flextable::flextable(df)
      ft <- flextable::theme_zebra(ft)
      ft <- flextable::fontsize(ft, size = 9, part = "all")
      ft <- flextable::padding(ft, padding = 3, part = "all")
      ft <- flextable::align(ft, align = "left", part = "all")
      ft <- flextable::valign(ft, valign = "top", part = "all")
      cn <- colnames(df)
      if ("Mineral" %in% cn)               ft <- flextable::width(ft, j = "Mineral", width = 1.2)
      if ("Vol. %" %in% cn)               ft <- flextable::width(ft, j = "Vol. %", width = 0.9)
      if ("Morphology, features" %in% cn) ft <- flextable::width(ft, j = "Morphology, features", width = 4.8)
      if ("Genesis" %in% cn)              ft <- flextable::width(ft, j = "Genesis", width = 2.0)
      ft <- flextable::set_table_properties(ft, layout = "fixed", width = 1.0)
      doc <- flextable::body_add_flextable(doc, value = ft)
      doc <- officer::body_add_par(doc, "", style = "Normal")
    }
    
    doc <- add_heading(doc, "6) Rock description (microscope)", level = 2)
    doc <- add_val_par(doc, input$rock_desc %||% "")
    
    doc <- add_heading(doc, "Images", level = 2)
    
    pool <- img_pool()
    sel_ids <- limited_selected_image_ids()
    
    if (length(sel_ids) == 0) {
      doc <- officer::body_add_par(doc, "No images selected.", style = "Normal")
    } else {
      # V49.3 DOCX caption fix:
      # Captions are now keyed by stable image_id, not by display slot.
      # Before exporting, synchronize current UI values into caption_store.
      sync_captions_from_inputs()

      max_w_in <- get_max_image_width_in()
      by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
      fig_no <- 0
      for (slot in seq_along(sel_ids)) {
        id <- sel_ids[slot]
        if (!id %in% names(by_id)) next
        item <- by_id[[id]]
        if (!is.null(item$path) && file.exists(item$path)) {
          fig_no <- fig_no + 1
          doc <- add_img_autoscale(doc, item$path, max_w_in = max_w_in)

          # Use the same stable caption logic as Save sample to SQLite.
          # Do NOT read input[[paste0("cap_", slot)]] here; after the V49.2 fix
          # visible caption inputs are named cap_<image_id>.
          cap <- sanitize_text(get_caption_for_image_id(id, slot) %||% "")

          # Fallback only if the image really has no caption.
          if (!nzchar(cap)) cap <- sanitize_text(item$name)

          doc <- add_caption_par(doc, fig_no, cap, style_name = caption_style)
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
      }
    }
    
    print(doc, target = file_path)
  }
  
  # =========================
# Save / Load sample to/from SQLite
# =========================
get_defaults_from_db <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  list(
    author = settings_get(con, "author", ""),
    project = settings_get(con, "project", ""),
    confirm_load = tolower(settings_get(con, "confirm_load", "TRUE")) %in% c("true","1","yes","y"),
    panel_bg = normalize_hex_color(settings_get(con, "panel_bg", settings_get(con, "panel_color", "#ffffff")), "#ffffff"),
    docx_font_size = settings_get(con, "docx_font_size", "10")
  )
}
copy_image_to_db_folder <- function(sample_id, src_path, order_idx) {
  if (isTRUE(READ_ONLY_MODE)) stop("Read-only LAN mode: copying images into the database folder is disabled.")
  if (is.null(src_path) || !file.exists(src_path)) return(NULL)
  dest_dir <- file.path(DB_IMAGES_DIR, sample_id)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(src_path))
  if (!nzchar(ext)) ext <- "png"
  base <- paste0(sprintf("%02d", order_idx), "_", gsub("[^A-Za-z0-9._-]+", "_", basename(src_path)))
  dest <- file.path(dest_dir, base)
  file.copy(src_path, dest, overwrite = TRUE)
  normalizePath(dest, winslash = "/", mustWork = FALSE)
}

pending_removed_images <- reactiveVal(NULL)

selected_image_fingerprints <- function() {
  pool <- img_pool()
  selected_ids <- limited_selected_image_ids()
  if (length(pool) == 0 || length(selected_ids) == 0) {
    return(list(hashes = character(0), paths = character(0)))
  }

  by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))
  selected_items <- lapply(selected_ids, function(id) by_id[[id]])
  selected_items <- Filter(Negate(is.null), selected_items)
  selected_paths <- vapply(selected_items, function(item) {
    path <- item$path %||% ""
    if (!nzchar(path) || !file.exists(path)) return(NA_character_)
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1))
  selected_paths <- unique(selected_paths[!is.na(selected_paths) & nzchar(selected_paths)])
  selected_hashes <- unique(vapply(selected_paths, function(path) {
    tryCatch(as.character(tools::md5sum(path)), error = function(e) NA_character_)
  }, character(1)))
  selected_hashes <- selected_hashes[!is.na(selected_hashes) & nzchar(selected_hashes)]

  list(hashes = selected_hashes, paths = selected_paths)
}

find_unselected_saved_images <- function() {
  sid <- trimws(input$sample_id %||% "")
  if (!nzchar(sid)) return(data.frame())

  defs <- tryCatch(get_defaults_from_db(), error = function(e) list(project = ""))
  proj_nm <- trimws(input$project_name %||% "")
  if (!nzchar(proj_nm)) proj_nm <- trimws(defs$project %||% "")
  if (!nzchar(proj_nm)) return(data.frame())

  con <- tryCatch(db_connect(), error = function(e) NULL)
  if (is.null(con)) return(data.frame())
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- get_project_id(con, proj_nm)
  if (is.na(pid)) return(data.frame())

  existing <- tryCatch(
    dbGetQuery(
      con,
      "SELECT id, file_path FROM images WHERE project_id = :pid AND sample_id = :sid ORDER BY img_order, id",
      params = list(pid = pid, sid = sid)
    ),
    error = function(e) data.frame()
  )
  if (is.null(existing) || nrow(existing) == 0) return(data.frame())

  selected <- selected_image_fingerprints()
  candidates <- lapply(seq_len(nrow(existing)), function(i) {
    stored_path <- as.character(existing$file_path[i] %||% "")
    abs_path <- tryCatch(resolve_image_path(stored_path), error = function(e) NA_character_)
    if (is.na(abs_path) || !file.exists(abs_path)) return(NULL)
    abs_path <- normalizePath(abs_path, winslash = "/", mustWork = FALSE)
    file_hash <- tryCatch(as.character(tools::md5sum(abs_path)), error = function(e) NA_character_)
    still_selected <- abs_path %in% selected$paths ||
      (!is.na(file_hash) && nzchar(file_hash) && file_hash %in% selected$hashes)
    if (isTRUE(still_selected)) return(NULL)

    data.frame(
      image_id = existing$id[i],
      file_path = stored_path,
      abs_path = abs_path,
      file_name = basename(abs_path),
      candidate_kind = "previously_saved",
      stringsAsFactors = FALSE
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0) return(data.frame())
  do.call(rbind, candidates)
}

find_unused_sample_folder_images <- function() {
  sid <- trimws(input$sample_id %||% "")
  if (!nzchar(sid)) return(data.frame())

  defs <- tryCatch(get_defaults_from_db(), error = function(e) list(project = ""))
  proj_nm <- trimws(input$project_name %||% "")
  if (!nzchar(proj_nm)) proj_nm <- trimws(defs$project %||% "")
  if (!nzchar(proj_nm)) return(data.frame())

  sample_dir <- file.path(DB_IMAGES_DIR, safe_name(proj_nm), safe_name(sid))
  if (!dir.exists(sample_dir)) return(data.frame())

  folder_files <- list.files(
    sample_dir,
    pattern = "\\.(jpg|jpeg|png|tif|tiff|bmp|webp)$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )
  folder_files <- folder_files[file.exists(folder_files) & !dir.exists(folder_files)]
  if (length(folder_files) == 0) return(data.frame())

  selected <- selected_image_fingerprints()
  candidates <- lapply(folder_files, function(path) {
    abs_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    file_hash <- tryCatch(as.character(tools::md5sum(abs_path)), error = function(e) NA_character_)
    still_selected <- abs_path %in% selected$paths ||
      (!is.na(file_hash) && nzchar(file_hash) && file_hash %in% selected$hashes)
    if (isTRUE(still_selected)) return(NULL)

    data.frame(
      image_id = NA_integer_,
      file_path = abs_path,
      abs_path = abs_path,
      file_name = basename(abs_path),
      candidate_kind = "sample_folder",
      stringsAsFactors = FALSE
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0) return(data.frame())
  do.call(rbind, candidates)
}

combine_image_archive_candidates <- function(...) {
  candidate_sets <- Filter(
    function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
    list(...)
  )
  if (length(candidate_sets) == 0) return(data.frame())

  combined <- do.call(rbind, candidate_sets)
  normalized_paths <- tolower(vapply(combined$abs_path, function(path) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1)))
  combined[!duplicated(normalized_paths), , drop = FALSE]
}

path_is_inside <- function(path, parent) {
  path <- tolower(normalizePath(path, winslash = "/", mustWork = FALSE))
  parent <- tolower(normalizePath(parent, winslash = "/", mustWork = FALSE))
  startsWith(path, paste0(sub("/+$", "", parent), "/"))
}

archive_unselected_image_files <- function(con, candidates, project_name, sample_id) {
  if (is.null(candidates) || nrow(candidates) == 0) {
    return(list(moved = 0L, skipped = 0L, failed = 0L))
  }

  expected_sample_dir <- file.path(DB_IMAGES_DIR, safe_name(project_name), safe_name(sample_id))
  referenced <- tryCatch(
    dbGetQuery(con, "SELECT file_path FROM images WHERE file_path IS NOT NULL AND TRIM(file_path) <> ''"),
    error = function(e) data.frame(file_path = character(0))
  )
  referenced_abs <- character(0)
  if (!is.null(referenced) && nrow(referenced) > 0) {
    referenced_abs <- unique(vapply(referenced$file_path, function(path) {
      resolved <- tryCatch(resolve_image_path(path), error = function(e) NA_character_)
      if (is.na(resolved) || !file.exists(resolved)) return(NA_character_)
      tolower(normalizePath(resolved, winslash = "/", mustWork = FALSE))
    }, character(1)))
    referenced_abs <- referenced_abs[!is.na(referenced_abs)]
  }

  archive_dir <- file.path(
    DB_IMAGES_DIR,
    "_archive",
    safe_name(project_name),
    safe_name(sample_id),
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )
  moved <- 0L
  skipped <- 0L
  failed <- 0L

  for (i in seq_len(nrow(candidates))) {
    source_path <- as.character(candidates$abs_path[i] %||% "")
    if (!nzchar(source_path) || !file.exists(source_path) ||
        !path_is_inside(source_path, expected_sample_dir)) {
      skipped <- skipped + 1L
      next
    }

    normalized_source <- tolower(normalizePath(source_path, winslash = "/", mustWork = FALSE))
    if (normalized_source %in% referenced_abs) {
      skipped <- skipped + 1L
      next
    }

    if (!dir.exists(archive_dir)) {
      dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
    }
    destination <- file.path(archive_dir, basename(source_path))
    if (file.exists(destination)) {
      destination <- file.path(
        archive_dir,
        paste0(
          tools::file_path_sans_ext(basename(source_path)),
          "_",
          sprintf("%03d", i),
          ".",
          tools::file_ext(source_path)
        )
      )
    }

    moved_ok <- tryCatch(file.rename(source_path, destination), error = function(e) FALSE)
    if (!isTRUE(moved_ok)) {
      copied <- tryCatch(file.copy(source_path, destination, overwrite = FALSE), error = function(e) FALSE)
      moved_ok <- isTRUE(copied) && isTRUE(tryCatch(file.remove(source_path), error = function(e) FALSE))
    }

    if (isTRUE(moved_ok)) moved <- moved + 1L else failed <- failed + 1L
  }

  list(moved = moved, skipped = skipped, failed = failed)
}

save_sample_sqlite <- function(removed_image_action = c("keep", "archive"), removed_candidates = NULL) {
  removed_image_action <- match.arg(removed_image_action)
  if (stop_if_read_only("saving to SQLite")) return(invisible(FALSE))
  sync_captions_from_inputs()
  sid <- trimws(input$sample_id %||% "")
  if (!nzchar(sid)) {
    showNotification("Sample ID is empty.", type = "error")
    return()
  }
  defs <- get_defaults_from_db()
  author  <- trimws(input$author %||% "")
  proj_nm <- trimws(input$project_name %||% "")
  if (!nzchar(author))  author  <- defs$author
  if (!nzchar(proj_nm)) proj_nm <- defs$project
  proj_nm <- trimws(proj_nm)
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- ensure_project_id(con, proj_nm)
  dbExecute(con, "
    INSERT INTO samples (
      project_id, sample_id, author, date,
      field_rock_name, field_desc, lat, lon, rock_name,
      grain_size, texture, rock_desc,
      suggested_mapping_unit_id, mapping_unit_confidence, mapping_unit_comment
    ) VALUES (
      :project_id, :sample_id, :author, :date,
      :field_rock_name, :field_desc, :lat, :lon, :rock_name,
      :grain_size, :texture, :rock_desc,
      :suggested_mapping_unit_id, :mapping_unit_confidence, :mapping_unit_comment
    )
    ON CONFLICT(project_id, sample_id) DO UPDATE SET
      author=excluded.author,
      date=excluded.date,
      field_rock_name=excluded.field_rock_name,
      field_desc=excluded.field_desc,
      lat=excluded.lat,
      lon=excluded.lon,
      rock_name=excluded.rock_name,
      grain_size=excluded.grain_size,
      texture=excluded.texture,
      rock_desc=excluded.rock_desc,
      suggested_mapping_unit_id=excluded.suggested_mapping_unit_id,
      mapping_unit_confidence=excluded.mapping_unit_confidence,
      mapping_unit_comment=excluded.mapping_unit_comment
  ", params = list(
    project_id = pid,
    sample_id  = sid,
    author     = author,
    date       = format(Sys.Date(), "%Y-%m-%d"),
    field_rock_name = input$field_rock_name %||% "",
    field_desc = input$field_desc %||% "",
    lat        = input$lat %||% "",
    lon        = input$lon %||% "",
    rock_name  = input$rock_name %||% "",
    grain_size = input$grain_size %||% "",
    texture    = input$texture %||% "",
    rock_desc  = input$rock_desc %||% "",
    suggested_mapping_unit_id = {
      mu <- suppressWarnings(as.integer(input$suggested_mapping_unit_id %||% NA_integer_))
      if (is.na(mu)) NA_integer_ else mu
    },
    mapping_unit_confidence = input$mapping_unit_confidence %||% "",
    mapping_unit_comment = mapping_unit_comment_rv() %||% ""
  ))
  dbExecute(con, "DELETE FROM mineral_composition WHERE project_id = :pid AND sample_id = :sid",
            params = list(pid = pid, sid = sid))
  mdf <- minerals()
  if (!is.null(mdf) && nrow(mdf) > 0) {
    minerals_df <- data.frame(
      project_id  = pid,
      sample_id   = sid,
      row_order   = seq_len(nrow(mdf)),
      mineral     = as.character(mdf[["Mineral"]]),
      vol_text    = as.character(mdf[["Vol. %"]]),
      morphology  = as.character(mdf[["Morphology, features"]]),
      genesis     = as.character(mdf[["Genesis"]]),
      stringsAsFactors = FALSE
    )
    dbWriteTable(con, "mineral_composition", minerals_df, append = TRUE)
  }

  # V56: replace recognized texture components for this sample.
  dbExecute(con, "DELETE FROM sample_textures WHERE project_id = :pid AND sample_id = :sid",
            params = list(pid = pid, sid = sid))
  tdf <- texture_components()
  if (!is.null(tdf) && nrow(tdf) > 0) {
    keep <- rep(TRUE, nrow(tdf))
    keep <- keep & (nzchar(trimws(as.character(tdf[["Texture"]]))) |
                    nzchar(trimws(as.character(tdf[["Interpretation"]]))))
    tdf <- tdf[keep, , drop = FALSE]
    if (nrow(tdf) > 0) {
      textures_df <- data.frame(
        project_id = pid,
        sample_id = sid,
        row_order = seq_len(nrow(tdf)),
        texture = as.character(tdf[["Texture"]]),
        importance = as.character(tdf[["Importance"]]),
        domain = as.character(tdf[["Domain"]]),
        interpretation = as.character(tdf[["Interpretation"]]),
        stringsAsFactors = FALSE
      )
      dbWriteTable(con, "sample_textures", textures_df, append = TRUE)
    }
  }
  
  # --- Images: avoid cloning on repeated Save ---
  # Replace image rows for this sample (we will re-insert in the current order)
  # IMPORTANT: avoid creating image clones on repeated saves.
  # Strategy:
  #   - read existing image rows BEFORE deleting
  #   - build hash -> existing relative path map (MD5 of file contents)
  #   - when saving, if an incoming image has the same hash as an existing one,
  #     reuse the existing file_path (no copy)
  existing_imgs <- tryCatch(
    dbGetQuery(con, "SELECT file_path FROM images WHERE project_id = :pid AND sample_id = :sid",
               params = list(pid = pid, sid = sid)),
    error = function(e) data.frame()
  )

  existing_hash_to_rel <- list()
  if (!is.null(existing_imgs) && nrow(existing_imgs) > 0) {
    for (fp in unique(existing_imgs$file_path)) {
      abs_fp <- tryCatch(resolve_image_path(fp), error = function(e) NA_character_)
      if (!is.na(abs_fp) && file.exists(abs_fp)) {
        h <- tryCatch(as.character(tools::md5sum(abs_fp)), error = function(e) NA_character_)
        if (!is.na(h) && !nzchar(existing_hash_to_rel[[h]] %||% "")) {
          existing_hash_to_rel[[h]] <- fp
        } else if (!is.na(h) && is.null(existing_hash_to_rel[[h]])) {
          existing_hash_to_rel[[h]] <- fp
        }
      }
    }
  }

  dbExecute(con, "DELETE FROM images WHERE project_id = :pid AND sample_id = :sid",
            params = list(pid = pid, sid = sid))

  pool <- img_pool()
  sel_ids <- limited_selected_image_ids()

  if (length(sel_ids) > 0 && length(pool) > 0) {
    by_id <- setNames(pool, vapply(pool, function(x) x$id, character(1)))

    proj_dir <- file.path(DB_IMAGES_DIR, safe_name(proj_nm), safe_name(sid))
    if (!dir.exists(proj_dir)) dir.create(proj_dir, recursive = TRUE)

    rows <- list()

    clean_to_jpg <- function(filename) {
      # Remove stacked extensions like .jpg.jpg or .png.jpg etc, then add .jpg
      base <- basename(filename)
      stem <- sub("(\\.(jpg|jpeg|png|tif|tiff|bmp|webp))+$", "", base, ignore.case = TRUE)
      paste0(stem, ".jpg")
    }

    for (i in seq_along(sel_ids)) {
      id <- sel_ids[i]
      if (!id %in% names(by_id)) next
      item <- by_id[[id]]

      if (is.null(item$path) || !file.exists(item$path)) next
      src_abs <- normalizePath(item$path, winslash = "/", mustWork = FALSE)

      # Hash of incoming file (content-based de-duplication)
      src_hash <- tryCatch(as.character(tools::md5sum(src_abs)), error = function(e) NA_character_)

      # If we already have the same image stored for this sample (same hash), reuse it.
      reused_rel <- NULL
      if (!is.na(src_hash) && !is.null(existing_hash_to_rel[[src_hash]])) {
        reused_rel <- existing_hash_to_rel[[src_hash]]
        reused_abs <- tryCatch(resolve_image_path(reused_rel), error = function(e) NA_character_)
        if (!is.na(reused_abs) && file.exists(reused_abs)) {
          rel_path <- normalize_slashes(reused_rel)
          rows[[length(rows) + 1]] <- data.frame(
            project_id = pid,
            sample_id  = sid,
            img_order  = i,
            file_path  = rel_path,
            caption    = get_caption_for_image_id(id, i),
            stringsAsFactors = FALSE
          )
          next
        }
      }

      # Otherwise copy into the DB images folder (one-time)
      basefile <- clean_to_jpg(src_abs)
      dest_abs <- file.path(proj_dir, paste0(sprintf("%02d", i), "_", basefile))
      dest_abs <- normalizePath(dest_abs, winslash = "/", mustWork = FALSE)

      # If source is already the destination path, keep as-is
      if (!(normalizePath(src_abs, winslash = "/", mustWork = FALSE) ==
            normalizePath(dest_abs, winslash = "/", mustWork = FALSE))) {

        # If destination exists (different file), make a unique name
        if (file.exists(dest_abs)) {
          stem <- tools::file_path_sans_ext(basefile)
          dest_abs <- file.path(
            proj_dir,
            paste0(sprintf("%02d", i), "_", stem, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
          )
          dest_abs <- normalizePath(dest_abs, winslash = "/", mustWork = FALSE)
        }

        file.copy(src_abs, dest_abs, overwrite = TRUE)
      }

      rel_path <- file.path("images", safe_name(proj_nm), safe_name(sid), basename(dest_abs))
      rel_path <- chartr("\\", "/", rel_path)

      rows[[length(rows) + 1]] <- data.frame(
        project_id = pid,
        sample_id  = sid,
        img_order  = i,
        file_path  = rel_path,
        caption    = get_caption_for_image_id(id, i),
        stringsAsFactors = FALSE
      )
    }

    if (length(rows) > 0) {
      dbWriteTable(con, "images", do.call(rbind, rows), append = TRUE)
    }
  }

  archive_result <- list(moved = 0L, skipped = 0L, failed = 0L)
  if (identical(removed_image_action, "archive") &&
      !is.null(removed_candidates) && nrow(removed_candidates) > 0) {
    archive_result <- archive_unselected_image_files(
      con,
      removed_candidates,
      project_name = proj_nm,
      sample_id = sid
    )
  }

  refresh_genesis_choices(selected = input$m_gen %||% "")
  refresh_morphology_choices(
    mineral_name = input$m_mineral %||% "",
    selected = input$m_morph %||% ""
  )
  if (identical(removed_image_action, "archive")) {
    archive_message <- paste0(
      "Saved to SQLite. ",
      archive_result$moved,
      " unused image file(s) moved to archive."
    )
    if (archive_result$skipped > 0 || archive_result$failed > 0) {
      archive_message <- paste0(
        archive_message,
        " Skipped: ", archive_result$skipped,
        "; failed: ", archive_result$failed, "."
      )
    }
    showNotification(
      archive_message,
      type = if (archive_result$failed > 0) "warning" else "message",
      duration = 7
    )
  } else {
    showNotification("Saved to SQLite.", type = "message")
  }
  last_sample_snapshot(snapshot_current_form())
  refresh_sample_list()
  invisible(TRUE)
}
load_sample_sqlite <- function(project_name, sid) {
  proj_nm <- trimws(project_name %||% "")
  sid <- trimws(sid %||% "")
  if (!nzchar(proj_nm) || !nzchar(sid)) return()
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- get_project_id(con, proj_nm)
  if (is.na(pid)) {
    showNotification("Project not found in DB.", type = "error")
    return()
  }
  samp <- tryCatch(dbGetQuery(con, "SELECT * FROM samples WHERE project_id = :pid AND sample_id = :sid",
                              params = list(pid = pid, sid = sid)),
                   error = function(e) NULL)
  if (is.null(samp) || nrow(samp) == 0) {
    showNotification("Sample not found in DB.", type = "error")
    return()
  }
  row <- samp[1, , drop = FALSE]

  # Get canonical project name from DB (avoid any case/format mismatch)
  proj_loaded <- tryCatch({
    pr <- dbGetQuery(con, "SELECT project_name FROM projects WHERE project_id = :pid", params = list(pid = pid))
    if (!is.null(pr) && nrow(pr) > 0) trimws(pr$project_name[1]) else proj_nm
  }, error = function(e) proj_nm)
  proj_loaded <- trimws(proj_loaded %||% proj_nm)


  # Robust getter for DB values (handles NULL/NA/factor and minor formatting issues)
  safe_get <- function(col) {
    if (!(col %in% names(row))) return("")
    v <- row[[col]][1]
    if (is.null(v) || length(v) == 0 || is.na(v)) return("")
    v <- as.character(v)
    v <- trimws(v)
    v
  }

  updateTextInput(session, "sample_id", value = sid)
  projs <- unique(c(list_projects(con), proj_loaded))
  updateSelectizeInput(session, "project_name", choices = projs, selected = proj_loaded, server = TRUE)
  # Re-apply Project selection after UI flush (prevents any later refresh from resetting to first choice)
  session$onFlushed(function() {
    try(updateSelectizeInput(session, "project_name", choices = projs, selected = proj_loaded, server = TRUE), silent = TRUE)
  }, once = TRUE)
  updateSelectInput(session, "load_project_name", selected = proj_loaded)
  updateTextInput(session, "author", value = safe_get("author"))
  session$onFlushed(function() {
    try(updateTextInput(session, "author", value = safe_get("author")), silent = TRUE)
  }, once = TRUE)
  updateTextInput(session, "field_rock_name", value = safe_get("field_rock_name"))
  updateTextAreaInput(session, "field_desc", value = safe_get("field_desc"))
  tryCatch({
    coord_get <- function(col) {
      v <- safe_get(col)
      if (!nzchar(v)) return("")
      gsub(",", ".", v, fixed = TRUE)
    }
    updateTextInput(session, "lat", value = coord_get("lat"))
    updateTextInput(session, "lon", value = coord_get("lon"))
  }, error = function(e) {
    showNotification(paste0("Load coords error: ", e$message), type = "error", duration = NULL)
  })
  updateTextInput(session, "rock_name", value = safe_get("rock_name"))
  g_val <- to_title_case(safe_get("grain_size"))
  t_val <- to_title_case(safe_get("texture"))
  gg2 <- grains_db()
  g_choices <- if (!is.null(gg2) && "grainsize_title" %in% names(gg2)) sort(unique(gg2$grainsize_title)) else character(0)
  if (nzchar(g_val) && !(g_val %in% g_choices)) g_choices <- unique(c(g_choices, g_val))
  updateSelectizeInput(session, "grain_size", choices = g_choices, selected = if (nzchar(g_val)) g_val else NULL, server = TRUE)
  tt2 <- textures_db()
  t_choices <- if (!is.null(tt2) && "texture_title" %in% names(tt2)) sort(unique(tt2$texture_title)) else character(0)
  if (nzchar(t_val) && !(t_val %in% t_choices)) t_choices <- unique(c(t_choices, t_val))
  updateSelectizeInput(session, "texture", choices = t_choices, selected = if (nzchar(t_val)) t_val else NULL, server = TRUE)
  updateTextAreaInput(session, "rock_desc", value = safe_get("rock_desc"))

  # V59: mapping-unit / rock-type interpretation.
  loaded_mapping_unit_id <- safe_get("suggested_mapping_unit_id")
  set_mapping_unit_selection_from_id(loaded_mapping_unit_id)
  updateSelectInput(session, "mapping_unit_confidence", selected = safe_get("mapping_unit_confidence"))
  mapping_unit_comment_rv(safe_get("mapping_unit_comment"))

  mdf <- tryCatch(dbGetQuery(con, "SELECT row_order, mineral, vol_text, morphology, genesis FROM mineral_composition WHERE project_id = :pid AND sample_id = :sid ORDER BY row_order",
                             params = list(pid = pid, sid = sid)),
                  error = function(e) NULL)
  if (!is.null(mdf) && nrow(mdf) > 0) {
    minerals(data.frame(`Mineral`=mdf$mineral, `Vol. %`=mdf$vol_text, `Morphology, features`=mdf$morphology, `Genesis`=mdf$genesis,
                        check.names=FALSE, stringsAsFactors=FALSE))
  } else {
    minerals(data.frame(`Mineral`=character(0),`Vol. %`=character(0),`Morphology, features`=character(0),`Genesis`=character(0),
                        check.names=FALSE, stringsAsFactors=FALSE))
  }

  tdf <- tryCatch(dbGetQuery(con, "SELECT row_order, texture, importance, domain, interpretation FROM sample_textures WHERE project_id = :pid AND sample_id = :sid ORDER BY row_order",
                             params = list(pid = pid, sid = sid)),
                  error = function(e) NULL)
  if (!is.null(tdf) && nrow(tdf) > 0) {
    texture_components(data.frame(
      `Texture` = tdf$texture,
      `Importance` = tdf$importance,
      `Domain` = tdf$domain,
      `Interpretation` = tdf$interpretation,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  } else {
    texture_components(empty_texture_components_df())
  }

  imgs <- tryCatch(dbGetQuery(con, "SELECT img_order, file_path, caption FROM images WHERE project_id = :pid AND sample_id = :sid ORDER BY img_order",
                              params = list(pid = pid, sid = sid)),
                   error = function(e) NULL)
  clear_img_dir()
  img_pool(list())
  img_choices(character(0))
  loaded_caps(list())
  caption_store(list())
  if (!is.null(imgs) && nrow(imgs) > 0) {
    pool <- list(); sel <- character(0); caps <- list()
    for (i in seq_len(nrow(imgs))) {
      fp <- as.character(imgs$file_path[i] %||% "")
      abs_fp <- resolve_image_path(fp)
      if (!nzchar(fp) || is.na(abs_fp) || !file.exists(abs_fp)) next
      nm <- basename(abs_fp)
      dest <- file.path(img_dir, nm)
      file.copy(abs_fp, dest, overwrite = TRUE)
      id <- make_img_id()
      pool[[length(pool)+1]] <- list(
        id=id, name=nm, ext=tolower(tools::file_ext(dest)),
        url=paste0("petro_img/", basename(dest)),
        path=dest, is_converted=FALSE, origin="db",
        order=as.integer(imgs$img_order[i] %||% i),
        caption=as.character(imgs$caption[i] %||% ""),
        meta=read_img_meta(dest)
      )
      sel <- c(sel, id)
      caps[[length(caps)+1]] <- as.character(imgs$caption[i] %||% "")
    }
    img_pool(pool); img_choices(sel); loaded_caps(caps); caption_store(stats::setNames(caps, sel))
  }
  refresh_genesis_choices(selected = input$m_gen %||% "")
  refresh_morphology_choices(
    mineral_name = input$m_mineral %||% "",
    selected = input$m_morph %||% ""
  )
  showNotification("Loaded from SQLite.", type = "message")
  last_sample_snapshot(snapshot_current_form())
}
observeEvent(input$save_sample_btn, {
  if (stop_if_read_only("saving to SQLite")) return()
  folder_check_enabled <- isTRUE(input$track_unused_image_files_setting)
  saved_candidates <- find_unselected_saved_images()
  folder_candidates <- if (folder_check_enabled) {
    find_unused_sample_folder_images()
  } else {
    data.frame()
  }
  candidates <- combine_image_archive_candidates(saved_candidates, folder_candidates)
  if (is.null(candidates) || nrow(candidates) == 0) {
    pending_removed_images(NULL)
    save_sample_sqlite()
    return()
  }

  pending_removed_images(candidates)
  n_removed <- nrow(candidates)
  shown_names <- utils::head(unique(candidates$file_name), 8)
  remaining_n <- max(0L, length(unique(candidates$file_name)) - length(shown_names))
  folder_files_found <- folder_check_enabled &&
    any(candidates$candidate_kind %in% "sample_folder")

  showModal(modalDialog(
    title = if (folder_files_found) "Unused image files found" else "Previously saved images are no longer selected",
    if (folder_files_found) {
      tags$p(
        tags$b(paste0(
          n_removed,
          " image file",
          if (n_removed == 1) " in the current sample folder is" else "s in the current sample folder are",
          " not used by the current image selection."
        )),
        " Move ", if (n_removed == 1) "it" else "them", " to the archive?"
      )
    } else {
      tags$p(
        tags$b(paste0(n_removed, " previously saved image", if (n_removed == 1) " is" else "s are", " no longer selected.")),
        " Move ", if (n_removed == 1) "its file" else "their files", " to the archive?"
      )
    },
    tags$p(
      class = "small-note",
      "The files are not deleted. They are moved from the active sample folder to ",
      tags$code("images/_archive/<Project>/<Sample>/<YYYYMMDD_HHMMSS>/"),
      ". This path records the original project, sample, and archive time; files can be restored manually."
    ),
    tags$ul(lapply(shown_names, tags$li)),
    if (remaining_n > 0) tags$p(class = "small-note", paste0("… and ", remaining_n, " more.")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        "save_keep_removed_files_btn",
        "Keep files and save",
        class = "btn settings-secondary-btn"
      ),
      actionButton(
        "save_archive_removed_files_btn",
        "Move to archive and save",
        class = "btn btn-primary"
      )
    ),
    easyClose = TRUE
  ))
})

observeEvent(input$save_keep_removed_files_btn, {
  candidates <- pending_removed_images()
  removeModal()
  pending_removed_images(NULL)
  save_sample_sqlite(removed_image_action = "keep", removed_candidates = candidates)
}, ignoreInit = TRUE)

observeEvent(input$save_archive_removed_files_btn, {
  candidates <- pending_removed_images()
  removeModal()
  pending_removed_images(NULL)
  save_sample_sqlite(removed_image_action = "archive", removed_candidates = candidates)
}, ignoreInit = TRUE)



observeEvent(input$load_sample_btn, {
  sid <- input$load_sample_id %||% ""
  defs <- get_defaults_from_db()
  if (isTRUE(defs$confirm_load)) {
    showModal(modalDialog(
      title = "Load sample from SQLite?",
      paste0("Load: ", (input$load_project_name %||% ""), " :: ", sid, " (this will overwrite current panel values)."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_load_yes", "Load")
      ),
      easyClose = TRUE
    ))
  } else {
    load_sample_sqlite(input$load_project_name %||% "", sid)
  }
}, ignoreInit = TRUE)

# =========================
# Delete sample from SQLite (with confirmation)
# =========================
delete_sample_sqlite <- function(project_name, sid) {
  if (stop_if_read_only("deleting samples from SQLite")) return(invisible(FALSE))
  proj_nm <- trimws(project_name %||% "")
  sid <- trimws(sid %||% "")
  if (!nzchar(proj_nm) || !nzchar(sid)) {
    showNotification("Select Project and Sample ID to delete.", type = "error")
    return(invisible(FALSE))
  }
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- get_project_id(con, proj_nm)
  if (is.na(pid)) {
    showNotification("Project not found in DB.", type = "error")
    return(invisible(FALSE))
  }

  # Collect image file paths before deleting rows
  imgs <- tryCatch(
    dbGetQuery(con, "SELECT file_path FROM images WHERE project_id = :pid AND sample_id = :sid",
               params = list(pid = pid, sid = sid)),
    error = function(e) NULL
  )

  # Delete dependent rows first
  tryCatch({
    dbExecute(con, "DELETE FROM mineral_composition WHERE project_id = :pid AND sample_id = :sid",
              params = list(pid = pid, sid = sid))
    dbExecute(con, "DELETE FROM sample_textures WHERE project_id = :pid AND sample_id = :sid",
              params = list(pid = pid, sid = sid))
    dbExecute(con, "DELETE FROM images WHERE project_id = :pid AND sample_id = :sid",
              params = list(pid = pid, sid = sid))
    dbExecute(con, "DELETE FROM samples WHERE project_id = :pid AND sample_id = :sid",
              params = list(pid = pid, sid = sid))
  }, error = function(e) {
    showNotification(paste0("Delete error: ", e$message), type = "error", duration = NULL)
    return(invisible(FALSE))
  })

  # Best-effort delete image files on disk (if any)
  if (!is.null(imgs) && nrow(imgs) > 0) {
    fps <- unique(as.character(imgs$file_path))
    for (fp in fps) {
      if (nzchar(fp) && file.exists(fp)) {
        try(file.remove(fp), silent = TRUE)
      }
    }
  }

  showNotification("Deleted sample from SQLite.", type = "message")
  invisible(TRUE)
}

observeEvent(input$delete_sample_btn, {
  if (stop_if_read_only("deleting samples from SQLite")) return()
  proj <- input$load_project_name %||% ""
  sid  <- input$load_sample_id %||% ""
  if (!nzchar(trimws(proj)) || !nzchar(trimws(sid))) {
    showNotification("Select Project and Sample ID first.", type = "error")
    return()
  }

  # Fetch rock name for confirmation display
  rock_nm <- ""
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- get_project_id(con, proj)
  if (!is.na(pid)) {
    rr <- tryCatch(
      dbGetQuery(con, "SELECT rock_name FROM samples WHERE project_id = :pid AND sample_id = :sid",
                 params = list(pid = pid, sid = sid)),
      error = function(e) NULL
    )
    if (!is.null(rr) && nrow(rr) > 0) rock_nm <- as.character(rr$rock_name[1] %||% "")
  }

  showModal(modalDialog(
    title = "Delete sample from SQLite?",
    tags$p("Are you sure you want to delete this sample:"),
    tags$ul(
      tags$li(tags$b("Project: "), proj),
      tags$li(tags$b("Sample ID: "), sid),
      tags$li(tags$b("Rock name (microscope identification): "), ifelse(nzchar(rock_nm), rock_nm, "(empty)"))
    ),
    tags$p(style = "color:#b00020;", "This action cannot be undone."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_delete_yes", "Delete", class = "btn-danger")
    ),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)

observeEvent(input$confirm_delete_yes, {
  if (stop_if_read_only("deleting samples from SQLite")) { removeModal(); return() }
  removeModal()
  ok <- delete_sample_sqlite(input$load_project_name %||% "", input$load_sample_id %||% "")
  if (isTRUE(ok)) {
    refresh_sample_list()
    # Clear selectors to reduce accidental repeats
    updateSelectInput(session, "load_sample_id", selected = character(0))
  }
}, ignoreInit = TRUE)
observeEvent(input$confirm_load_yes, {
  removeModal()
  sid <- input$load_sample_id %||% ""
  load_sample_sqlite(input$load_project_name %||% "", sid)
}, ignoreInit = TRUE)

# ---- Project management (SQLite) ----
delete_project_sqlite <- function(project_name) {
  if (stop_if_read_only("deleting projects from SQLite")) return(FALSE)
  proj_nm <- trimws(project_name %||% "")
  if (!nzchar(proj_nm)) return(FALSE)

  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)

  pid <- get_project_id(con, proj_nm)
  if (is.na(pid)) {
    showNotification("Project not found in DB.", type = "error")
    return(FALSE)
  }

  # Collect image files to remove
  imgs <- tryCatch(
    dbGetQuery(con, "SELECT file_path FROM images WHERE project_id = :pid", params = list(pid = pid)),
    error = function(e) data.frame(file_path = character(0))
  )
  fps <- unique(as.character(imgs$file_path %||% character(0)))

  ok <- FALSE
  tryCatch({
    dbBegin(con)
    # Delete child rows first
    dbExecute(con, "DELETE FROM mineral_composition WHERE project_id = :pid", params = list(pid = pid))
    dbExecute(con, "DELETE FROM sample_textures WHERE project_id = :pid", params = list(pid = pid))
    dbExecute(con, "DELETE FROM images WHERE project_id = :pid", params = list(pid = pid))
    dbExecute(con, "DELETE FROM samples WHERE project_id = :pid", params = list(pid = pid))
    dbExecute(con, "DELETE FROM projects WHERE project_id = :pid", params = list(pid = pid))
    dbCommit(con)
    ok <- TRUE
  }, error = function(e) {
    try(dbRollback(con), silent = TRUE)
    showNotification(paste("Delete project error:", e$message), type = "error", duration = NULL)
    ok <- FALSE
  })

  # Remove image files (best-effort)
  if (isTRUE(ok) && length(fps) > 0) {
    for (fp in fps) {
      if (nzchar(fp) && file.exists(fp)) {
        try(file.remove(fp), silent = TRUE)
      }
    }
    # Remove the project directory if empty / exists
    proj_dir <- file.path(DB_IMAGES_DIR, safe_name(proj_nm))
    if (dir.exists(proj_dir)) {
      try(unlink(proj_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
  }

  if (isTRUE(ok)) showNotification("Deleted project (and all its samples) from SQLite.", type = "message")
  ok
}

observeEvent(input$create_project_btn, {
  if (stop_if_read_only("creating projects in SQLite")) return()
  showModal(modalDialog(
    title = "Create project",
    textInput("new_project_name_modal", "Project name", value = "", placeholder = "e.g., Takutu 2025"),
    tags$p(class="small-note", "Project names are stored exactly as typed/selected. Imported names with underscores are preserved."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_create_project_yes", "Create", class="btn-success")
    ),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)

observeEvent(input$confirm_create_project_yes, {
  if (stop_if_read_only("creating projects in SQLite")) { removeModal(); return() }
  pn <- trimws(input$new_project_name_modal %||% "")
  removeModal()

  if (!nzchar(pn)) {
    showNotification("Project name is empty.", type = "error")
    return()
  }

  tryCatch({
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    db_init(con)

    exists_df <- dbGetQuery(
      con,
      "SELECT project_id FROM projects WHERE project_name = :pn",
      params = list(pn = pn)
    )

    if (nrow(exists_df) > 0) {
      showNotification(
        paste("Project already exists:", pn),
        type = "warning",
        duration = 5
      )
    } else {
      dbExecute(
        con,
        "INSERT INTO projects (project_name) VALUES (:pn)",
        params = list(pn = pn)
      )
      showNotification(
        paste("Project created:", pn),
        type = "message",
        duration = 5
      )
    }

    refresh_sample_list()
    updateSelectInput(session, "load_project_name", selected = pn)
    updateSelectizeInput(session, "project_name", choices = list_projects(con), selected = pn, server = TRUE)
  }, error = function(e) {
    showNotification(
      paste("Create project error:", e$message),
      type = "error",
      duration = NULL
    )
  })
}, ignoreInit = TRUE)

observeEvent(input$delete_project_btn, {
  if (stop_if_read_only("deleting projects from SQLite")) return()
  proj <- input$load_project_name %||% ""
  if (!nzchar(trimws(proj))) {
    showNotification("Select Project (from DB) first.", type = "error")
    return()
  }

  # Count samples in this project for confirmation
  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  db_init(con)
  pid <- get_project_id(con, proj)
  n_samp <- 0
  if (!is.na(pid)) {
    rr <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) AS n FROM samples WHERE project_id = :pid", params = list(pid = pid)),
                   error = function(e) NULL)
    if (!is.null(rr) && nrow(rr) > 0) n_samp <- as.integer(rr$n[1] %||% 0)
  }

  showModal(modalDialog(
    title = "Delete project?",
    tags$p("Do you really want to delete this project and all related samples?"),
    tags$ul(
      tags$li(tags$b("Project: "), proj),
      tags$li(tags$b("Samples in project: "), n_samp)
    ),
    tags$p(style="color:#b00020;", "Warning: all project samples, mineral tables, texture components, and images will be deleted."),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_delete_project_yes", "Delete project", class="btn-danger")
    ),
    easyClose = TRUE
  ))
}, ignoreInit = TRUE)

observeEvent(input$confirm_delete_project_yes, {
  if (stop_if_read_only("deleting projects from SQLite")) { removeModal(); return() }
  removeModal()
  ok <- delete_project_sqlite(input$load_project_name %||% "")
  if (isTRUE(ok)) {
    refresh_sample_list()
    updateSelectInput(session, "load_project_name", selected = character(0))
    updateSelectInput(session, "load_sample_id", selected = character(0))
  }
}, ignoreInit = TRUE)

docx_download_filename <- function() {
  sid <- gsub("[^A-Za-z0-9._-]+", "_", input$sample_id %||% "sample")
  paste0("petro_report_", sid, ".docx")
}

docx_download_content <- function(file) {
  build_docx_from_template(file)
}

output$download_docx <- downloadHandler(
  filename = docx_download_filename,
  content = docx_download_content
)

output$download_docx_database_tools <- downloadHandler(
  filename = docx_download_filename,
  content = docx_download_content
)



  # ---- Dynamic titles (browser tab + in-app header) ----
  output$dynamic_header <- renderUI({
    proj <- trimws(input$project_name %||% "")
    sid  <- trimws(input$sample_id %||% "")
    base <- "Microscope Rock Description Panel"
    title_ui <- if (nzchar(proj) && nzchar(sid)) {
      tags$div(style="margin: 6px 0 10px 0; font-weight: 700; font-size: 18px; border-bottom: 1px solid #cfcfcf; padding-bottom: 6px;",
               paste0(base, " | Project – ", proj, " | Sample – ", sid))
    } else if (nzchar(proj)) {
      tags$div(style="margin: 6px 0 10px 0; font-weight: 700; font-size: 18px; border-bottom: 1px solid #cfcfcf; padding-bottom: 6px;",
               paste0(base, " | Project – ", proj))
    } else {
      tags$div(style="margin: 6px 0 10px 0; font-weight: 700; font-size: 18px; border-bottom: 1px solid #cfcfcf; padding-bottom: 6px;", base)
    }
    tagList(title_ui, read_only_banner_ui())
  })

  observeEvent(list(input$project_name, input$sample_id), {
    proj <- trimws(input$project_name %||% "")
    sid  <- trimws(input$sample_id %||% "")
    base <- "Microscope Rock Description Panel"
    title <- if (nzchar(proj) && nzchar(sid)) {
      paste0(base, " | Project – ", proj, " | Sample – ", sid)
    } else if (nzchar(proj)) {
      paste0(base, " | Project – ", proj)
    } else {
      base
    }
    session$onFlushed(function() {
      session$sendCustomMessage("set_doc_title", list(title = title))
    }, once = TRUE)
  }, ignoreInit = FALSE)

}
shinyApp(ui, server)
