local({
# Create the public demo SQLite database for Make_Thin_Section.
#
# The demo contains two projects:
# 1) regional geology of the Isle of Skye, Scotland.
# 2) Oldoinyo Lengai xenolith / natrocarbonatite examples from Tanzania.
#
# Skye geological background source:
# Bell, B.R. and Harris, J.W. (1986), An excursion guide to the geology
# of the Isle of Skye, as hosted on BGS Earthwise:
# https://earthwise.bgs.ac.uk/index.php/Pre-Palaeogene_rocks_of_the_Isle_of_Skye
#
# Lengai geological background source:
# Carmody, L. (2012), Geochemical characteristics of carbonatite-related
# volcanism and sub-volcanic metasomatism at Oldoinyo Lengai, Tanzania.
# PhD thesis, University College London.
#
# The demo records below are rewritten educational examples. They are not copied
# field descriptions or measured petrographic descriptions from the source.

required <- c("DBI", "RSQLite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required package(s): ", paste(missing, collapse = ", "),
       ". Install them before creating the demo database.")
}

source_paths <- vapply(sys.frames(), function(x) if(is.null(x$ofile)) "" else as.character(x$ofile[[1]]), character(1))
source_paths <- source_paths[nzchar(source_paths)]
script_file <- if(length(source_paths)) tail(source_paths,1) else NULL
if (is.null(script_file) || !nzchar(script_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else NULL
}

app_dir <- if (!is.null(script_file) && nzchar(script_file)) {
  normalizePath(dirname(script_file), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

db_path <- file.path(app_dir, "Make_Thin_Section_demo.sqlite")
images_root <- file.path(app_dir, "images")
project_name <- "Isle_of_Skye_demo"
project_dir <- file.path(images_root, project_name)
lengai_project_name <- "Oldoinyo_Lengai_demo"
lengai_project_dir <- file.path(images_root, lengai_project_name)
source_url <- "https://earthwise.bgs.ac.uk/index.php/Pre-Palaeogene_rocks_of_the_Isle_of_Skye"
source_note <- paste(
  "Educational demo inspired by Bell and Harris (1986), An excursion guide to the geology of the Isle of Skye,",
  "hosted on BGS Earthwise. Demo petrographic descriptions are rewritten examples, not copied source text.",
  source_url
)
lengai_source_ref <- paste(
  "Carmody, L. (2012). Geochemical characteristics of carbonatite-related volcanism and",
  "sub-volcanic metasomatism at Oldoinyo Lengai, Tanzania. PhD thesis, University College London."
)
lengai_source_note <- paste(
  "Educational demo inspired by Carmody (2012), especially Chapter 4 petrography of the",
  "Oldoinyo Lengai xenolith suite and natrocarbonatite examples.",
  "Demo descriptions are rewritten examples, not copied thesis text.",
  lengai_source_ref
)

safe_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

if (dir.exists(project_dir) || dir.exists(lengai_project_dir) || file.exists(db_path)) stop("Demo already exists. This generator will not overwrite existing data.")

invisible(safe_dir(project_dir))
invisible(safe_dir(lengai_project_dir))


con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

DBI::dbExecute(con, "
  CREATE TABLE settings (
    key TEXT PRIMARY KEY,
    value TEXT
  );
")

DBI::dbExecute(con, "
  CREATE TABLE projects (
    project_id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_name TEXT NOT NULL UNIQUE,
    created_at TEXT DEFAULT (date('now')),
    notes TEXT
  );
")

DBI::dbExecute(con, "
  CREATE TABLE samples (
    project_id INTEGER NOT NULL,
    sample_id TEXT NOT NULL,
    author TEXT,
    date TEXT,
    field_rock_name TEXT,
    field_desc TEXT,
    lat TEXT,
    lon TEXT,
    rock_name TEXT,
    grain_size TEXT,
    texture TEXT,
    rock_desc TEXT,
    suggested_mapping_unit_id INTEGER,
    mapping_unit_confidence TEXT,
    mapping_unit_comment TEXT,
    PRIMARY KEY (project_id, sample_id),
    FOREIGN KEY (project_id) REFERENCES projects(project_id)
  );
")

DBI::dbExecute(con, "
  CREATE TABLE mapping_units (
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

DBI::dbExecute(con, "
  CREATE TABLE mineral_composition (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL,
    sample_id TEXT NOT NULL,
    row_order INTEGER,
    mineral TEXT,
    vol_text TEXT,
    morphology TEXT,
    genesis TEXT,
    FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
  );
")

DBI::dbExecute(con, "
  CREATE TABLE images (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL,
    sample_id TEXT NOT NULL,
    img_order INTEGER,
    file_path TEXT,
    caption TEXT,
    FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
  );
")

DBI::dbExecute(con, "
  CREATE TABLE sample_textures (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project_id INTEGER NOT NULL,
    sample_id TEXT NOT NULL,
    row_order INTEGER,
    texture TEXT,
    importance TEXT,
    domain TEXT,
    interpretation TEXT,
    FOREIGN KEY (project_id, sample_id) REFERENCES samples(project_id, sample_id)
  );
")

DBI::dbExecute(con, "CREATE INDEX idx_mapping_units_classifier ON mapping_units(classifier_name, unit_name);")

settings <- data.frame(
  key = c(
    "author", "project", "confirm_load", "panel_bg", "docx_font_size",
    "max_sel_images", "img_card_width", "img_thumb_height",
    "max_image_mb", "max_image_dim", "jpeg_quality_default", "jpeg_quality_min"
  ),
  value = c(
    "Demo User", project_name, "TRUE", "#ffffff", "10",
    "10", "280", "170", "4", "2500", "85", "60"
  ),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(con, "settings", settings, append = TRUE)

DBI::dbExecute(con, "
  INSERT INTO projects(project_id, project_name, created_at, notes)
  VALUES (1, :project_name, '2026-07-06', :notes)
", params = list(project_name = project_name, notes = source_note))

DBI::dbExecute(con, "
  INSERT INTO projects(project_id, project_name, created_at, notes)
  VALUES (2, :project_name, '2026-07-06', :notes)
", params = list(project_name = lengai_project_name, notes = lengai_source_note))

mapping_units <- data.frame(
  unit_id = 1:6,
  classifier_name = rep("LITHOLOGY_Canada_Geological_Survey", 6),
  source_id = paste0("SKYE-DEMO-", sprintf("%03d", 1:6)),
  class_id = c("metamorphic", "sedimentary", "metamorphic", "sedimentary", "sedimentary", "sedimentary"),
  unit_code = c("LEW-GNS", "TOR-ARK", "MOI-PSM", "CAM-DOL", "TRI-RED", "JUR-CAL"),
  unit_name = c(
    "Lewisian gneiss",
    "Torridonian arkose",
    "Moine psammite",
    "Cambro-Ordovician dolostone",
    "Triassic red beds",
    "Jurassic calcareous sandstone"
  ),
  shortname = c("gneiss", "arkose", "psammite", "dolostone", "red beds", "calcareous sandstone"),
  unit_description = c(
    "Basement gneiss with quartz-feldspar and mafic bands.",
    "Proterozoic feldspathic sandstone / arkose derived from older basement sources.",
    "Deformed psammitic to semipelitic metasedimentary rock.",
    "Dolostone and carbonate strata within the Cambro-Ordovician succession.",
    "Continental red-bed sandstone, marl and local conglomerate.",
    "Shallow-marine calcareous sandstone and shale association."
  ),
  source_reference = rep("Bell and Harris 1986 via BGS Earthwise; rewritten demo classification.", 6),
  notes = rep(source_note, 6),
  is_active = rep(1L, 6),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(con, "mapping_units", mapping_units, append = TRUE)

lengai_mapping_units <- data.frame(
  unit_id = 7:12,
  classifier_name = rep("LITHOLOGY_Canada_Geological_Survey", 6),
  source_id = paste0("LENGAI-DEMO-", sprintf("%03d", 1:6)),
  class_id = c("metasomatic", "plutonic", "plutonic", "plutonic", "metasomatic", "volcanic"),
  unit_code = c("FEN-PYX", "IJO", "OL-MI", "SYN", "FEN", "NAT-CARB"),
  unit_name = c(
    "fenitised pyroxenite",
    "ijolite",
    "olivine-mica ijolite",
    "nepheline syenite",
    "fenite",
    "natrocarbonatite"
  ),
  shortname = c("fenitised pyroxenite", "ijolite", "olivine-mica ijolite", "syenite", "fenite", "natrocarbonatite"),
  unit_description = c(
    "Ultramafic cumulate-like xenolith overprinted by alkaline metasomatism.",
    "Nepheline- and pyroxene-rich plutonic equivalent of nephelinite.",
    "Ijolitic rock containing olivine and mica megacrysts / xenocrysts.",
    "Alkali feldspar-nepheline-pyroxene plutonic rock with accessory phases.",
    "Alkali-metasomatised quartzofeldspathic rock associated with alkaline-carbonatite systems.",
    "Sodium-rich carbonate lava dominated by nyerereite and gregoryite."
  ),
  source_reference = rep("Carmody 2012, UCL PhD thesis; rewritten demo classification.", 6),
  notes = rep(lengai_source_note, 6),
  is_active = rep(1L, 6),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(con, "mapping_units", lengai_mapping_units, append = TRUE)

samples <- data.frame(
  project_id = rep(1L, 6),
  sample_id = c("SKYE-LEW-001", "SKYE-TOR-001", "SKYE-MOI-001", "SKYE-CAM-001", "SKYE-TRI-001", "SKYE-JUR-001"),
  author = rep("Demo User", 6),
  date = rep("2026-07-06", 6),
  field_rock_name = c(
    "Lewisian gneiss",
    "Torridonian arkose",
    "Moine psammite",
    "dolostone",
    "Triassic red sandstone and marl",
    "Jurassic calcareous sandstone"
  ),
  field_desc = c(
    paste("Sleat Peninsula educational record. Coastal basement exposure represented as banded gneiss with pale quartz-feldspar layers and darker mafic domains.", source_note),
    paste("Sleat Peninsula educational record. Feldspathic sandstone / arkose example representing coarse Proterozoic clastic strata associated with the Torridonian succession.", source_note),
    paste("Tarskavaig area educational record. Deformed psammitic metasedimentary rock used to demonstrate foliation, recrystallization and structural overprint fields.", source_note),
    paste("Strath district educational record. Dark dolostone / carbonate example representing Cambro-Ordovician carbonate strata near Broadford and Ben Suardal.", source_note),
    paste("Strath district educational record. Red-bed sample representing Triassic continental sandstone, marl and local conglomeratic facies.", source_note),
    paste("Strathaird / Broadford educational record. Calcareous sandstone example representing Jurassic shallow-marine sedimentary rocks on Skye.", source_note)
  ),
  lat = c("57.1440", "57.1170", "57.1225", "57.2350", "57.2400", "57.1800"),
  lon = c("-5.8230", "-5.9000", "-5.9580", "-5.9600", "-5.8850", "-6.0900"),
  rock_name = c(
    "biotite-hornblende gneiss",
    "feldspathic sandstone",
    "quartz-mica psammite",
    "cherty dolostone",
    "hematitic sandstone",
    "calcareous sandstone"
  ),
  grain_size = c("medium-grained", "medium- to coarse-grained", "fine- to medium-grained", "fine-grained", "fine- to medium-grained", "medium-grained"),
  texture = c("gneissose", "clastic", "schistose", "granoblastic", "clastic", "bioclastic"),
  rock_desc = c(
    "Banded metamorphic rock composed of quartz, plagioclase, alkali feldspar, biotite and amphibole. Pale domains are quartz-feldspar rich, whereas darker layers contain aligned biotite and amphibole. The fabric is interpreted as a gneissose metamorphic texture suitable for demonstrating mineral table, texture history and lithological assignment.",
    "Framework-supported feldspathic sandstone with quartz and feldspar grains, subordinate lithic fragments and a fine matrix. Feldspars are partly altered to sericite and clay minerals. The record demonstrates how sedimentary rocks can be entered into the same petrographic database as metamorphic samples.",
    "Fine- to medium-grained metasedimentary rock dominated by quartz and mica. Quartz-rich layers alternate with mica-rich laminae, producing a weak schistose to psammitic fabric. Mica alignment and local grain-size reduction are used here to demonstrate deformation-related texture entries.",
    "Fine-grained carbonate rock with dolomite-dominated mosaic texture, scattered opaque material and chert nodules represented by silica-rich domains. The sample demonstrates carbonate description fields, non-silicate mineral composition and uncertain lithological interpretation.",
    "Red, iron-stained clastic rock composed mainly of quartz and feldspar grains with hematitic pigment and local clay-rich matrix. The example is designed to demonstrate continental red-bed descriptions and the separation between field name, microscope name and mapping-unit interpretation.",
    "Calcareous sandstone with quartz grains, carbonate cement and minor fossil-fragment / shell-like allochems. The example demonstrates a shallow-marine sedimentary sample with mixed siliciclastic and carbonate components."
  ),
  suggested_mapping_unit_id = 1:6,
  mapping_unit_confidence = c("High", "Medium", "Medium", "Medium", "High", "Medium"),
  mapping_unit_comment = paste(
    "Demo assignment based on regional geology summarized from Bell and Harris (1986) via BGS Earthwise.",
    "Text rewritten for Make_Thin_Section demonstration; not a copied field description.",
    source_url
  ),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(con, "samples", samples, append = TRUE)

mineral_rows <- list(
  "SKYE-LEW-001" = list(
    c("quartz", "30", "Anhedral grains in pale gneissic bands.", "metamorphic / recrystallized"),
    c("plagioclase", "25", "Subhedral to anhedral feldspar in felsic layers.", "metamorphic"),
    c("biotite", "20", "Brown flakes aligned with foliation.", "metamorphic"),
    c("hornblende", "15", "Green prismatic grains in mafic bands.", "metamorphic"),
    c("opaque minerals", "5", "Fine disseminated grains.", "accessory")
  ),
  "SKYE-TOR-001" = list(
    c("quartz", "45", "Subrounded to angular framework grains.", "detrital"),
    c("K-feldspar", "25", "Altered feldspar grains with dusty appearance.", "detrital"),
    c("plagioclase", "15", "Partly sericitized feldspar grains.", "detrital"),
    c("clay minerals", "10", "Fine matrix and alteration products.", "diagenetic / alteration")
  ),
  "SKYE-MOI-001" = list(
    c("quartz", "55", "Elongate grains and ribbons in psammitic layers.", "metamorphic"),
    c("muscovite", "20", "Aligned mica flakes defining foliation.", "metamorphic"),
    c("biotite", "15", "Fine flakes in mica-rich laminae.", "metamorphic"),
    c("chlorite", "5", "Green secondary mica-like aggregates.", "retrograde alteration")
  ),
  "SKYE-CAM-001" = list(
    c("dolomite", "70", "Fine crystalline carbonate mosaic.", "sedimentary / diagenetic"),
    c("quartz", "10", "Microcrystalline silica in chert-like domains.", "diagenetic"),
    c("calcite", "10", "Patchy carbonate cement and veinlets.", "diagenetic"),
    c("opaque minerals", "5", "Very fine disseminated grains.", "accessory")
  ),
  "SKYE-TRI-001" = list(
    c("quartz", "50", "Subrounded framework grains.", "detrital"),
    c("feldspar", "15", "Altered feldspar grains.", "detrital"),
    c("hematite", "10", "Red pigment and grain coatings.", "diagenetic"),
    c("clay minerals", "20", "Fine matrix between framework grains.", "diagenetic / matrix")
  ),
  "SKYE-JUR-001" = list(
    c("quartz", "40", "Rounded to subrounded framework grains.", "detrital"),
    c("calcite", "30", "Carbonate cement and allochem fragments.", "diagenetic / biogenic"),
    c("feldspar", "10", "Minor detrital feldspar grains.", "detrital"),
    c("opaque minerals", "5", "Small accessory grains.", "accessory")
  )
)

mineral_df <- do.call(rbind, lapply(names(mineral_rows), function(sid) {
  rows <- mineral_rows[[sid]]
  data.frame(
    project_id = 1L,
    sample_id = sid,
    row_order = seq_along(rows),
    mineral = vapply(rows, `[`, character(1), 1),
    vol_text = vapply(rows, `[`, character(1), 2),
    morphology = vapply(rows, `[`, character(1), 3),
    genesis = vapply(rows, `[`, character(1), 4),
    stringsAsFactors = FALSE
  )
}))
DBI::dbWriteTable(con, "mineral_composition", mineral_df, append = TRUE)

texture_rows <- list(
  "SKYE-LEW-001" = list(c("gneissose", "primary", "whole thin section", "Alternating felsic and mafic bands define the main fabric."), c("mineral lineation", "secondary", "mafic domains", "Aligned biotite and amphibole demonstrate directional fabric.")),
  "SKYE-TOR-001" = list(c("clastic", "primary", "framework", "Quartz and feldspar grains preserve sedimentary framework texture."), c("matrix-supported patches", "secondary", "fine domains", "Clay-rich matrix records alteration and diagenesis.")),
  "SKYE-MOI-001" = list(c("schistose", "primary", "mica-rich laminae", "Mica alignment records metamorphic deformation."), c("granoblastic", "secondary", "quartz-rich layers", "Quartz-rich layers show recrystallized mosaic domains.")),
  "SKYE-CAM-001" = list(c("granoblastic", "primary", "carbonate mosaic", "Fine carbonate mosaic represents recrystallized dolostone texture."), c("nodular", "secondary", "silica-rich domains", "Chert-like domains are entered as separate texture components.")),
  "SKYE-TRI-001" = list(c("clastic", "primary", "framework grains", "Detrital grain-supported texture dominates the sample."), c("hematitic coating", "secondary", "grain boundaries", "Red iron oxide pigment highlights diagenetic overprint.")),
  "SKYE-JUR-001" = list(c("bioclastic", "primary", "carbonate-rich domains", "Shell-like carbonate fragments demonstrate mixed siliciclastic-carbonate texture."), c("cement-supported", "secondary", "pores and grain contacts", "Calcite cement records diagenetic modification."))
)

texture_df <- do.call(rbind, lapply(names(texture_rows), function(sid) {
  rows <- texture_rows[[sid]]
  data.frame(
    project_id = 1L,
    sample_id = sid,
    row_order = seq_along(rows),
    texture = vapply(rows, `[`, character(1), 1),
    importance = vapply(rows, `[`, character(1), 2),
    domain = vapply(rows, `[`, character(1), 3),
    interpretation = vapply(rows, `[`, character(1), 4),
    stringsAsFactors = FALSE
  )
}))
DBI::dbWriteTable(con, "sample_textures", texture_df, append = TRUE)

lengai_samples <- data.frame(
  project_id = rep(2L, 6),
  sample_id = c("LENGAI-OLX15", "LENGAI-OLX02", "LENGAI-OLX10", "LENGAI-OLX05", "LENGAI-OLX17B", "LENGAI-OLX07"),
  author = rep("Demo User", 6),
  date = rep("2026-07-06", 6),
  field_rock_name = c(
    "fenitised pyroxenite xenolith",
    "ijolite xenolith",
    "olivine-mica ijolite xenolith",
    "syenite xenolith",
    "fenite xenolith",
    "natrocarbonatite block"
  ),
  field_desc = c(
    paste("Educational Oldoinyo Lengai xenolith record. Rounded green crystalline core with a finer nephelinitic volcanic coating; designed to demonstrate metasomatic overprint fields.", lengai_source_note),
    paste("Educational Oldoinyo Lengai xenolith record. Coarse nepheline-pyroxene plutonic rock used to demonstrate ijolitic mineralogy and accessory phases.", lengai_source_note),
    paste("Educational Oldoinyo Lengai xenolith record. Ijolitic host carrying olivine and phlogopite/biotite mica components, suitable for recording disequilibrium textures.", lengai_source_note),
    paste("Educational Oldoinyo Lengai xenolith record. Alkali feldspar-nepheline-pyroxene syenitic example with accessory titanite, wollastonite and opaque grains.", lengai_source_note),
    paste("Educational Oldoinyo Lengai xenolith record. Fenitised quartzofeldspathic rock demonstrating alkali metasomatism and preservation of relict protolith features.", lengai_source_note),
    paste("Educational Oldoinyo Lengai natrocarbonatite record. Sodium-rich carbonate material with rapid alteration potential and quench-like carbonate textures.", lengai_source_note)
  ),
  lat = c("-2.7640", "-2.7625", "-2.7632", "-2.7618", "-2.7660", "-2.7608"),
  lon = c("35.9140", "35.9130", "35.9155", "35.9120", "35.9160", "35.9148"),
  rock_name = c(
    "fenitised pyroxenite",
    "melanite-bearing ijolite",
    "olivine-mica ijolite",
    "nepheline syenite",
    "medium-grade fenite",
    "natrocarbonatite"
  ),
  grain_size = c("coarse-grained", "coarse-grained", "medium- to coarse-grained", "medium-grained", "medium-grained", "fine-grained"),
  texture = c("equigranular", "glomeroporphyritic", "xenocrystic", "porphyritic", "veined", "quench"),
  rock_desc = c(
    "Coarse-grained xenolith dominated by alkali pyroxene and alkali feldspar with minor nepheline and accessory titanite, apatite and opaque grains. The mineral assemblage and replacement-style textures are entered here as a fenitised ultramafic cumulate / pyroxenite example affected by alkaline metasomatic fluids.",
    "Coarse plutonic rock dominated by nepheline and alkali pyroxene with accessory melanite garnet, apatite, titanite and opaque grains. Large pyroxene and nepheline crystals are set in finer material of similar composition, demonstrating the mineral-table workflow for silica-undersaturated alkaline rocks.",
    "Ijolitic rock with olivine and mica megacryst / xenocryst components in a nepheline-pyroxene-feldspar matrix. Mica rims around olivine and mica crystals are recorded as disequilibrium reaction textures and as a separate texture-history component.",
    "Alkali feldspar-nepheline-pyroxene rock with titanite, wollastonite and opaque accessory phases. Porphyritic domains and local glassy / remelted patches are represented as a syenitic xenolith example from the Lengai suite.",
    "Alkali-metasomatised quartzofeldspathic rock dominated by feldspar and pyroxene with minor nepheline, quartz and accessory phases. Vein-like feldspathic domains and residual quartz are used to demonstrate modal versus cryptic metasomatic interpretation.",
    "Fine-grained sodium-rich carbonate rock represented by nyerereite- and gregoryite-rich components with minor sylvite, fluorite, apatite and sulphide. Acicular carbonate groundmass is recorded as a quench texture analogous in form to spinifex-like crystallisation."
  ),
  suggested_mapping_unit_id = 7:12,
  mapping_unit_confidence = c("Medium", "High", "High", "Medium", "High", "High"),
  mapping_unit_comment = paste(
    "Demo assignment based on Carmody (2012), Chapter 4 petrography of Oldoinyo Lengai xenolithic material.",
    "Text rewritten for Make_Thin_Section demonstration; not copied thesis text.",
    lengai_source_ref
  ),
  stringsAsFactors = FALSE
)
DBI::dbWriteTable(con, "samples", lengai_samples, append = TRUE)

lengai_mineral_rows <- list(
  "LENGAI-OLX15" = list(
    c("aegirine-augite", "45", "Zoned subhedral pyroxene grains with green-brown pleochroism.", "primary / metasomatic"),
    c("alkali feldspar", "35", "Equant grains with simple twinning and low-order interference colours.", "metasomatic"),
    c("nepheline", "10", "Minor feldspathoid phase, more abundant in the volcanic coating.", "igneous"),
    c("titanite", "3", "Small accessory grains.", "accessory"),
    c("apatite", "2", "Accessory crystals in the crystalline core.", "accessory")
  ),
  "LENGAI-OLX02" = list(
    c("nepheline", "45", "Euhedral grains, locally enclosing apatite needles.", "igneous"),
    c("alkali pyroxene", "40", "Zoned glomeroporphyritic crystals.", "igneous"),
    c("melanite garnet", "2", "Well-formed accessory garnet crystals.", "accessory"),
    c("apatite", "2", "Inclusions and interstitial accessory crystals.", "accessory"),
    c("Fe sulphide", "1", "Small rounded opaque grains.", "early accessory")
  ),
  "LENGAI-OLX10" = list(
    c("alkali pyroxene", "30", "Zoned grains in ijolitic groundmass.", "igneous"),
    c("alkali feldspar", "25", "Matrix feldspar associated with nepheline and pyroxene.", "igneous"),
    c("phlogopite/biotite", "25", "Large mica crystals and reaction rims.", "xenocrystic / reaction"),
    c("olivine", "15", "Megacrystic grains mantled by mica.", "xenocrystic"),
    c("melanite garnet", "5", "Small euhedral accessory phenocrysts.", "accessory")
  ),
  "LENGAI-OLX05" = list(
    c("alkali feldspar", "45", "Phenocrysts and poikilitic groundmass grains.", "igneous"),
    c("nepheline", "25", "Feldspathoid phase in syenitic assemblage.", "igneous"),
    c("alkali pyroxene", "20", "Early phenocrysts and groundmass grains.", "igneous"),
    c("titanite", "3", "Accessory grains, locally with reaction rims.", "accessory"),
    c("wollastonite", "3", "Calcium silicate accessory phase.", "accessory")
  ),
  "LENGAI-OLX17B" = list(
    c("alkali feldspar", "55", "Subhedral grains and feldspathic vein-like domains.", "metasomatic"),
    c("alkali pyroxene", "25", "Small oblate grains along feldspar boundaries.", "metasomatic"),
    c("quartz", "5", "Relict grains interpreted as inherited from the protolith.", "relict"),
    c("nepheline", "5", "Minor feldspathoid phase.", "metasomatic"),
    c("apatite", "3", "Accessory crystals.", "accessory")
  ),
  "LENGAI-OLX07" = list(
    c("nyerereite", "55", "Lath-shaped phenocrysts and acicular groundmass crystals.", "igneous carbonate"),
    c("gregoryite", "30", "Rounded carbonate crystals in fine groundmass.", "igneous carbonate"),
    c("sylvite", "5", "Minor evaporite-like chloride phase.", "late / accessory"),
    c("fluorite", "3", "Minor accessory grains.", "accessory"),
    c("apatite", "3", "Accessory phosphate crystals.", "accessory")
  )
)

lengai_mineral_df <- do.call(rbind, lapply(names(lengai_mineral_rows), function(sid) {
  rows <- lengai_mineral_rows[[sid]]
  data.frame(
    project_id = 2L,
    sample_id = sid,
    row_order = seq_along(rows),
    mineral = vapply(rows, `[`, character(1), 1),
    vol_text = vapply(rows, `[`, character(1), 2),
    morphology = vapply(rows, `[`, character(1), 3),
    genesis = vapply(rows, `[`, character(1), 4),
    stringsAsFactors = FALSE
  )
}))
DBI::dbWriteTable(con, "mineral_composition", lengai_mineral_df, append = TRUE)

lengai_texture_rows <- list(
  "LENGAI-OLX15" = list(c("equigranular", "primary", "xenolith core", "Coarse pyroxene-feldspar framework records crystalline cumulate-like texture."), c("reaction / replacement", "secondary", "pyroxene and pseudomorph domains", "Metasomatic replacement records fenitisation of the original ultramafic assemblage.")),
  "LENGAI-OLX02" = list(c("glomeroporphyritic", "primary", "pyroxene clusters", "Clustered zoned pyroxene and nepheline illustrate ijolitic crystallisation."), c("poikilitic inclusions", "secondary", "nepheline grains", "Apatite inclusions document early accessory crystallisation.")),
  "LENGAI-OLX10" = list(c("xenocrystic", "primary", "olivine and mica megacrysts", "Large crystals are interpreted as incorporated into the ijolitic host."), c("reaction rim", "secondary", "olivine and mica margins", "Mica rims record disequilibrium with the surrounding host.")),
  "LENGAI-OLX05" = list(c("porphyritic", "primary", "feldspar-pyroxene framework", "Large crystals occur within finer syenitic material."), c("remelted / glassy patches", "secondary", "intergranular domains", "Local melt pockets demonstrate late thermal disturbance.")),
  "LENGAI-OLX17B" = list(c("veined", "primary", "feldspathic domains", "Alkali feldspar-rich domains record metasomatic fluid pathways."), c("relict quartz", "secondary", "quartz-bearing patches", "Quartz is interpreted as inherited from the protolith.")),
  "LENGAI-OLX07" = list(c("quench", "primary", "carbonate groundmass", "Acicular nyerereite records rapid carbonate crystallisation."), c("alteration rind", "secondary", "outer surface and cavities", "Natrocarbonatite alteration reflects hydration and atmospheric exposure."))
)

lengai_texture_df <- do.call(rbind, lapply(names(lengai_texture_rows), function(sid) {
  rows <- lengai_texture_rows[[sid]]
  data.frame(
    project_id = 2L,
    sample_id = sid,
    row_order = seq_along(rows),
    texture = vapply(rows, `[`, character(1), 1),
    importance = vapply(rows, `[`, character(1), 2),
    domain = vapply(rows, `[`, character(1), 3),
    interpretation = vapply(rows, `[`, character(1), 4),
    stringsAsFactors = FALSE
  )
}))
DBI::dbWriteTable(con, "sample_textures", lengai_texture_df, append = TRUE)

make_demo_image <- function(path, title, palette, banded = FALSE) {
  safe_dir(dirname(path))
  grDevices::jpeg(path, width = 1400, height = 900, quality = 92)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mar = c(0, 0, 2, 0), bg = "#f7f7f2")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  rect(0, 0, 1, 1, col = palette[1], border = NA)
  set.seed(abs(sum(utf8ToInt(title))))
  if (isTRUE(banded)) {
    for (i in seq_len(10)) {
      y0 <- (i - 1) / 10
      polygon(c(0, 1, 1, 0), c(y0, y0 + runif(1, 0.04, 0.09), y0 + 0.11, y0 + runif(1, 0.02, 0.07)),
              col = adjustcolor(sample(palette, 1), 0.65), border = NA)
    }
  }
  for (i in seq_len(130)) {
    x <- runif(1)
    y <- runif(1)
    r <- runif(1, 0.012, 0.075)
    col <- adjustcolor(sample(palette, 1), alpha.f = runif(1, 0.45, 0.85))
    theta <- seq(0, 2 * pi, length.out = 60)
    xs <- x + r * runif(1, 0.6, 1.9) * cos(theta)
    ys <- y + r * runif(1, 0.4, 1.5) * sin(theta)
    polygon(xs, ys, col = col, border = adjustcolor("#333333", 0.13))
  }
  mtext(title, side = 3, line = 0.3, cex = 1.35, font = 2)
}

image_specs <- data.frame(
  sample_id = samples$sample_id,
  img_order = rep(1L, nrow(samples)),
  file_name = paste0("01_", samples$sample_id, "_demo.jpg"),
  caption = paste0(
    "Synthetic educational image for ", samples$sample_id,
    ". Geological theme inspired by Bell and Harris (1986) via BGS Earthwise; image generated for demo use."
  ),
  stringsAsFactors = FALSE
)

palettes <- list(
  "SKYE-LEW-001" = c("#c8c4b5", "#2f3431", "#6f766a", "#d8d7c6", "#415044"),
  "SKYE-TOR-001" = c("#b88755", "#d8b27b", "#8f5f39", "#f0d6a6", "#6e4b32"),
  "SKYE-MOI-001" = c("#9b9488", "#d6d0c2", "#5a554f", "#b8aa95", "#73675d"),
  "SKYE-CAM-001" = c("#3d3d3f", "#9b9c91", "#d7d5c5", "#111214", "#6f756d"),
  "SKYE-TRI-001" = c("#8b3428", "#c85f3d", "#e0b072", "#5f2c25", "#b74932"),
  "SKYE-JUR-001" = c("#c9c0a7", "#efe7d5", "#8a826f", "#d7ccb1", "#6d665b")
)

for (i in seq_len(nrow(image_specs))) {
  sid <- image_specs$sample_id[i]
  file_abs <- file.path(project_dir, sid, image_specs$file_name[i])
  make_demo_image(
    file_abs,
    paste(sid, "synthetic thin-section view"),
    palettes[[sid]],
    banded = sid %in% c("SKYE-LEW-001", "SKYE-MOI-001")
  )
  rel <- file.path("images", project_name, sid, image_specs$file_name[i])
  DBI::dbExecute(con, "
    INSERT INTO images(project_id, sample_id, img_order, file_path, caption)
    VALUES (1, :sample_id, :img_order, :file_path, :caption)
  ", params = list(
    sample_id = sid,
    img_order = image_specs$img_order[i],
    file_path = chartr("\\", "/", rel),
    caption = image_specs$caption[i]
  ))
}

lengai_image_specs <- data.frame(
  sample_id = lengai_samples$sample_id,
  img_order = rep(1L, nrow(lengai_samples)),
  file_name = paste0("01_", lengai_samples$sample_id, "_demo.jpg"),
  caption = paste0(
    "Synthetic educational image for ", lengai_samples$sample_id,
    ". Geological theme inspired by Carmody (2012), UCL PhD thesis; image generated for demo use."
  ),
  stringsAsFactors = FALSE
)

lengai_palettes <- list(
  "LENGAI-OLX15" = c("#44664f", "#8aa17b", "#d0c7a5", "#263b31", "#6b7f62"),
  "LENGAI-OLX02" = c("#7c8f70", "#d6d2b6", "#3f5441", "#b2a878", "#1f2a25"),
  "LENGAI-OLX10" = c("#6a5b35", "#b38d4a", "#2f3e2c", "#c7b27a", "#7d3f2c"),
  "LENGAI-OLX05" = c("#c4baa0", "#8f886e", "#ddd7c4", "#4f5148", "#aa9b75"),
  "LENGAI-OLX17B" = c("#d2c6a8", "#9fb097", "#515b4d", "#ece0bd", "#7b806f"),
  "LENGAI-OLX07" = c("#f0eee4", "#cabf95", "#6e6a72", "#d9d3ff", "#9f9668")
)

for (i in seq_len(nrow(lengai_image_specs))) {
  sid <- lengai_image_specs$sample_id[i]
  file_abs <- file.path(lengai_project_dir, sid, lengai_image_specs$file_name[i])
  make_demo_image(
    file_abs,
    paste(sid, "synthetic thin-section view"),
    lengai_palettes[[sid]],
    banded = sid %in% c("LENGAI-OLX17B")
  )
  rel <- file.path("images", lengai_project_name, sid, lengai_image_specs$file_name[i])
  DBI::dbExecute(con, "
    INSERT INTO images(project_id, sample_id, img_order, file_path, caption)
    VALUES (2, :sample_id, :img_order, :file_path, :caption)
  ", params = list(
    sample_id = sid,
    img_order = lengai_image_specs$img_order[i],
    file_path = chartr("\\", "/", rel),
    caption = lengai_image_specs$caption[i]
  ))
}

cat("Demo SQLite database created:\n", normalizePath(db_path, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Demo projects:\n", project_name, "\n", lengai_project_name, "\n", sep = "")
cat("Source attribution:\n", source_url, "\n", lengai_source_ref, "\n", sep = "")
cat("Demo images folder:\n", normalizePath(images_root, winslash = "/", mustWork = TRUE), "\n", sep = "")
})
