# Make_Thin_Section

Make_Thin_Section is a local Shiny application for maintaining a structured database of petrographic thin-section descriptions.

The application provides a practical interface for building and using a petrographic-description database: field metadata, microscopic rock description, mineral composition, image captions, lithological interpretation, QAPF plotting, Word report generation, and project-level GIS export are handled from one Shiny panel.

Author: Sergei Abramov  
Contact: westabra@gmail.com  
Current public app file: `app_Make_Thin_Section_version_62_mineral_reference_web_search.R`

## Why Use Make_Thin_Section?

Petrographic descriptions are often scattered across notebooks, Word files, spreadsheets, old reports, image folders, and personal field archives. Valuable observations may exist, but they are difficult to search, compare, map, revise, or reuse. Make_Thin_Section was created to turn that scattered petrographic memory into a structured, living database.

The app is deliberately flexible. It can store new thin-section descriptions written directly at the microscope, but it can also absorb historical descriptions imported from older documents, spreadsheets, and project archives. Because the database is SQLite and the surrounding workflow is written in R, users can bring the full R ecosystem to the problem: parsing tables, cleaning old sample names, harmonizing lithology terms, importing coordinates, linking images, checking vocabularies, and exporting clean datasets for maps, reports, and publication.

Make_Thin_Section is not limited to one geological style or one institution's template. A single database can hold regional mapping descriptions, academic thin-section notes, re-logged legacy samples, teaching collections, unusual volcanic rocks, sedimentary suites, metamorphic assemblages, and new project work. The demo database intentionally includes both the Isle of Skye and Oldoinyo Lengai to show that the same interface can handle ordinary, historical, and highly specialized petrographic material.

The goal is simple: keep the petrographic description, mineral table, texture interpretation, images, coordinates, lithological classification, Word report, and GIS export pathway together. Once descriptions are in this form, they stop being isolated text and become reusable geological data.

## What The App Does

- Creates structured thin-section descriptions.
- Stores samples by project and sample ID in a local SQLite database.
- Provides a user-friendly interface for building a reusable petrographic-description database.
- Keeps mineral composition as editable table rows.
- Uses `minerals.csv` as the selectable list of mineral names and petrographic reference terms.
- Provides an independent local lookup that matches the selected name from `minerals.csv` against descriptions in `biblio_for_mineral_descriptions.csv`.
- Uses the Whitney and Evans abbreviation list when preparing mineral names for the project petrographic-summary workbook.
- Stores, orders, captions, compresses, and reuses sample images.
- Exports petrographic descriptions to DOCX using `petro_template.docx`.
- Provides a QAPF diagram tool for manual modal values.
- Links samples to lithological / mapping-unit classifications from `lookup_rocktype.csv`.
- Displays project samples on a Leaflet map.
- Exports project point layers as KML, Shapefile ZIP, or GeoPackage.
- Exports clean static project maps as PNG or PDF.
- Exports a project petrographic-summary workbook from Database tools.
- Includes a separate Image Fragmentator tool for petrographic image segmentation experiments.

## Intended Users

This app is intended for geologists, petrographers, students, and researchers who need a local, reproducible way to describe thin sections and keep sample descriptions, images, and report exports together.

It is not an automatic mineral-identification system. Mineral names, textures, interpretations, and lithological assignments remain expert-controlled user inputs.

## Main Workflow

1. Start the app.
2. Create a project in Database tools, or select an existing project in the left panel.
3. Enter the sample ID and basic field metadata. You can also start from Database tools with `Add New Sample Description`; this clears the sample-specific fields, keeps Project and Author, opens the description form, and focuses the Sample ID field.
4. Add coordinates manually, or use the coordinate calculator for PSAD56 / UTM to WGS84 conversion.
5. Enter the microscope rock name, grain size, and primary texture.
6. Add mineral-composition rows in the Rock description tab.
7. Write the microscopic rock description.
8. Upload or paste images in the Image container tab.
9. Select image order and captions for the sample report.
10. Optionally assign a lithological unit and confidence level.
11. Save the sample to SQLite.
12. Generate a DOCX petrographic report.
13. Use the Project map / Google Earth tab to review sample locations and export GIS layers or static maps.

The left panel is the central sample form. The tabs on the right provide supporting work areas: mineral table, database loading tools, QAPF, reference lookup, map/GIS export, settings, the built-in user guide, and image management.

## Repository Files

Keep these files in the same folder unless you also update paths in the R code:

- `app_Make_Thin_Section_version_62_mineral_reference_web_search.R` - main public Shiny application.
- `mineral_reference_web_resources.csv` - editable curated catalogue used by the Mineral Reference resource hub.
- `run_demo.R` - demo launcher. This is the file most new users should run first.
- `install_main_dependencies.R` - installs missing packages required by the main application and its export tools.
- `install_fragmentator_dependencies.R` - installs missing packages required by Image Fragmentator, including `EBImage`.
- `create_demo_sqlite.R` - developer / maintenance script for rebuilding the demo database and demo images.
- `Make_Thin_Section_demo.sqlite` - small two-project demonstration database stored next to the app.
- `images/` - image library folder used by the demo database and by local SQLite databases stored in this app folder.
- `image_fragmentator_V6.R` - standalone image-segmentation helper app.
- `petro_template.docx` - Word template used for DOCX export.
- `minerals.csv` - selectable mineral names and petrographic reference terms used by the composition editor and local Mineral Reference lookup. The distributed file contains names only.
- `biblio_for_mineral_descriptions.csv` - sourced reference descriptions classified as `general`, `general PPL`, or `general XPL`. It may also contain clearly identified microtextures and alteration mixtures that are not independent mineral species.
- `grainsizes.csv` - grain-size vocabulary.
- `textures.csv` - texture vocabulary and interpretation support.
- `lookup_rocktype.csv` - lithology / mapping-unit classifier source.
- `rock_name_list.csv` - optional supporting rock-name vocabulary; version 62 does not load it automatically.
- `White et. al. Abbreviations for names of rock-forming minerals.csv` - abbreviation source required by the petrographic-summary exporter and reproduced on its `mineral_abbreviations` sheet.
- `export_petrographic_summary_from_SQLite_v2_3.R` - standalone/reference copy of the project-summary exporter. Version 62 contains the operational exporter in the main application file and does not source this external script at runtime.

## Requirements

Install a recent version of R. Open R or RStudio in the application folder and
run the supplied dependency installer once:

```r
source("install_main_dependencies.R")
```

The installer downloads only packages that are missing. It installs the core
application packages:

```text
shiny, DT, officer, flextable, magick, base64enc,
sf, leaflet, DBI, RSQLite, dplyr, stringr, readr, openxlsx, tibble, markdown
```

It also installs packages used by advanced map and Shapefile export:

```text
htmlwidgets, webshot2, zip
```

Image Fragmentator has a separate installer because its Bioconductor dependency
is not required for normal use of the main application:

```r
source("install_fragmentator_dependencies.R")
```

This installs `shiny`, `magick`, `png`, `BiocManager`, and `EBImage`. Packages
that are already installed are not downloaded again.

The main app checks Fragmentator dependencies before launching it. If something
is missing, it shows an installation dialog instead of starting a broken
Fragmentator session.

On Windows, the `sf` and `magick` packages may require system libraries depending on the R distribution. Installing recent CRAN binaries is usually the simplest route.

## How To Run

Open R or RStudio in the application folder and run:

```r
shiny::runApp("app_Make_Thin_Section_version_62_mineral_reference_web_search.R")
```

Run this command from the application folder. The distribution does not
currently use the standard filename `app.R`, so call the versioned application
file explicitly. The app expects its CSV files and `petro_template.docx` to be
available from the working directory.

## How To Run The Demo

The repository includes a small demo SQLite database next to the app:

```text
Make_Thin_Section_demo.sqlite
```

It contains two demo projects:

- `Isle_of_Skye_demo` - six educational records based on the pre-Palaeogene geology of the Isle of Skye, Scotland.
- `Oldoinyo_Lengai_demo` - six educational records based on xenolith and natrocarbonatite examples from Oldoinyo Lengai, Tanzania.

The demo database includes sample records, mineral tables, texture components, coordinates, lithological assignments, and selected synthetic images. It is designed to show how the app handles switching between independent petrographic projects.

The Skye project uses the BGS Earthwise page below as geological background:

[Pre-Palaeogene rocks of the Isle of Skye](https://earthwise.bgs.ac.uk/index.php/Pre-Palaeogene_rocks_of_the_Isle_of_Skye)

The Oldoinyo Lengai project uses the following thesis as geological background:

Carmody, L. (2012). *Geochemical characteristics of carbonatite-related volcanism and sub-volcanic metasomatism at Oldoinyo Lengai, Tanzania*. PhD thesis, University College London.

The demo petrographic descriptions are rewritten educational examples. They are not copied field descriptions or measured thin-section descriptions from the sources.

For a normal first-time demo, run only:

```r
source("run_demo.R")
```

`run_demo.R` sets `PETRO_DB_PATH` to `Make_Thin_Section_demo.sqlite` and starts the Shiny app.

Do not run `create_demo_sqlite.R` for normal demo use. That script deletes and rebuilds the demo SQLite database and demo images. Use it only if the demo database has been removed, corrupted, or intentionally changed.

If you need to rebuild the demo from scratch, use this order:

```r
source("create_demo_sqlite.R")
source("run_demo.R")
```

or:

```r
Sys.setenv(PETRO_DB_PATH = file.path(getwd(), "Make_Thin_Section_demo.sqlite"))
shiny::runApp("app_Make_Thin_Section_version_62_mineral_reference_web_search.R")
```

## Data Storage

By default, the application stores data in:

```text
C:/GCDkit.data/petro_db/petro_v3.sqlite
```

The application-managed image library and working export directory are located beside that database:

```text
C:/GCDkit.data/petro_db/images/
C:/GCDkit.data/petro_db/exports/
```

Browser downloads are not automatically written into these folders. DOCX reports,
project petrographic-summary XLSX workbooks, and other download-handler results are
delivered through the browser, which controls the final save location. In particular,
the petrographic-summary exporter does not need write access to the SQLite directory.

On first use, the app creates the SQLite database schema and the required folders if they do not already exist.

To use another database location without editing the app code, set the `PETRO_DB_PATH` environment variable before launching:

```r
Sys.setenv(PETRO_DB_PATH = "C:/path/to/your/petro_demo.sqlite")
shiny::runApp("app_Make_Thin_Section_version_62_mineral_reference_web_search.R")
```

If `PETRO_DB_PATH` is not set, the app uses the default path shown above.

The included `Make_Thin_Section_demo.sqlite` is an example of this approach: it is stored next to the Shiny script and can be launched through `run_demo.R`.

## Read-Only Mode

The app supports a read-only LAN mode through the environment variable `SHINY_READ_ONLY`.

```r
Sys.setenv(SHINY_READ_ONLY = "TRUE")
shiny::runApp("app_Make_Thin_Section_version_62_mineral_reference_web_search.R")
```

In read-only mode, the app opens the existing SQLite database without allowing write operations such as saving samples, creating projects, deleting records, or importing classifier rows. Project petrographic-summary reports remain available: the database is opened with a physical SQLite read-only connection and the generated workbook is delivered through the browser without writing beside the database.

## Main Interface Areas

### Sample Metadata

The left panel contains the primary sample record: sample ID, field rock name, field notes, coordinates, microscope rock name, grain size, primary texture, project, author, and lithological interpretation.

### Rock Description

This tab contains the mineral-composition editor, the microscopic description field, and the selected image/caption preview used for report export.

### Database Tools

This tab is used to create projects, load saved samples, delete samples or projects, start a new sample description, generate a Word report for the displayed sample, and export project-level petrographic summaries.

#### Add New Sample Description

Click the blue `Add New Sample Description` button to open a blank sample form.
The action clears sample-specific fields but keeps the current Project and Author,
opens the Rock description tab, and places the cursor in `Sample ID`. Enter the new
sample number, complete the description, and click `Save sample to SQLite` in the
left panel when the record is ready. Opening the form alone does not create a row in
SQLite.

#### Petrographic Summary XLSX

Select a project in `Project (from DB)` and click `Export petrographic summary (XLSX)`. A progress window reports the current stage while the database is opened, tables are read, mineral records are classified, and the workbook is assembled. When generation finishes, the browser handles the save location. The application does not write the report into the application folder or beside the SQLite database.

The suggested filename includes the project name:

```text
Project_Name_petrographic_summary_v2_3.xlsx
```

The workbook contains three sheets in this order:

1. `summary` — sample ID, coordinates, rock name, suggested lithological unit, grouped mineralogy, alteration, veins, weathering, and calculated Q, A, P, and AI values.
2. `unclassified_records` — detailed mineral-composition records whose `Genesis` value is empty or does not match the Primary, Alteration, Veins, or Weathering classification rules.
3. `mineral_abbreviations` — the complete source list from `White et. al. Abbreviations for names of rock-forming minerals.csv`.

Mineral abbreviations affect display names only. If a mineral or material is not present in the White et al. list, its full name is retained in title case. Assignment to a mineralogy group is controlled by the `Genesis` field, not by the presence of an abbreviation. Unclassified material is also included in `Primary mineralogy` with the note `[Genesis unclassified; see unclassified_records]`, while its full source record remains on the `unclassified_records` sheet.

Below the table on the `summary` sheet, the workbook explains how entries marked
`[Genesis unclassified; see unclassified_records]` are handled and directs the user
to the `unclassified_records` sheet for full details. A second footnote identifies
the source of the mineral abbreviations:

Whitney, D. L., & Evans, B. W. (2010). *Abbreviations for names of rock-forming
minerals*. *American Mineralogist*, *95*(1), 185-187.

The summary exporter used by version 62 is embedded in
`app_Make_Thin_Section_version_62_mineral_reference_web_search.R`. The external
`export_petrographic_summary_from_SQLite_v2_3.R` file is not required at runtime.

#### Single Sample Word Report

After selecting a project and sample, click `Load from SQLite`, then use `Generate DOCX` under `Single Sample Report → Word document`. This creates the same Word report as the `Generate DOCX` button in the main sample panel; both buttons export the sample currently displayed in the panel.

### QAPF

This tab plots manual Q, A, P, and F values on a Streckeisen-style QAPF diagram. It supports plutonic and volcanic diagram modes and can export the diagram as an image.

The QAPF field-coordinate tables were adapted from the QAPF implementation in
GCDkit. The surrounding Shiny interface, data preparation, point-in-polygon
classification, plotting workflow, and export logic are implemented in this
project. GCDkit is distributed under `GPL (>= 2)`. Please cite Janousek, Farrow
and Erban (2006) when the QAPF functionality is used in published work. See
`THIRD_PARTY_NOTICES.md` for details.

### Mineral Reference

The local `Lookup Mineral` list is loaded from `minerals.csv`. Selecting an item
searches for matching records in `biblio_for_mineral_descriptions.csv` and displays
the available `general`, `general PPL`, and `general XPL` descriptions together with
their sources. Supported synonym pairs can resolve to the same bibliography records.

Mineral Reference is intentionally independent from the Mineral field in the Rock
description tab. It does not overwrite, append to, or otherwise modify the sample
description; users can copy and paste any useful text manually.

The same tab also provides an online research area that builds links to external
mineralogical and bibliographic resources. Opening those links does not modify the
local CSV library or the SQLite database.

### Project Map / Google Earth

This tab maps project samples with valid coordinates and exports:

- KML for Google Earth.
- ESRI Shapefile as ZIP.
- GeoPackage.
- Static map PNG.
- Static map PDF.

GIS layer export uses the selected Main Project. Overlay and all-project modes are for on-screen comparison and static map export.

### Settings

This tab stores defaults in SQLite, including default author, default project, DOCX field font size, image display size, and image-compression limits. It also reloads CSV libraries and mapping classifiers.

### Help / User Guide

This tab displays `README.md` directly inside the Shiny application as a readable user guide. The `markdown` package provides formatted headings, lists, links, and code examples. If that package is unavailable, the app remains usable and displays the guide as plain text.

### Image Container

This tab manages images before saving the sample. Images can be uploaded from files or pasted from the clipboard. The user controls image order and captions.

## DOCX Export

DOCX reports are generated from `petro_template.docx`. The exported report includes:

- sample metadata;
- field description;
- petrographic description;
- texture history / texture components;
- lithological interpretation;
- mineral-composition table;
- selected images and captions.

Do not rename or remove `petro_template.docx` unless the code is updated accordingly.

## Image Fragmentator

The Image Fragmentator is a separate Shiny tool launched from the Image container tab. It is intended for experimental texture and structure segmentation, not mineral identification.

Current methods include:

- grayscale threshold segmentation;
- k-means RGB clustering;
- marker-controlled watershed segmentation.

Fragmentator exports PNG figures and CSV statistics manually. It does not automatically write results into the Make_Thin_Section SQLite database.

## Publication Notes

This project has been developed and tested primarily in one petrographic workflow. External users may need to adjust:

- `DB_PATH`;
- local controlled-vocabulary CSV files;
- the DOCX template;
- map export settings;
- package installation for `sf`, `magick`, `webshot2`, and `EBImage`.

Possible additions for future releases include a citation file
(`CITATION.cff`), example screenshots, example DOCX and GIS exports, and a
launcher script for non-demo local work.

## Current Limitations

- The default database path is configured in the R script, but it can be overridden with `PETRO_DB_PATH`.
- The first screen is powerful but dense; new users should follow the workflow section above.
- Some features assume Windows-style paths and local desktop use.
- Advanced basemap export depends on web rendering, installed screenshot tools, and internet availability for map tiles.
- The app is not a validated analytical instrument; petrographic interpretation remains the responsibility of the user.

## Suggested Citation

Abramov, S. (2025-2026). Make_Thin_Section: a Shiny application for petrographic thin-section description, image management, DOCX reporting, and project GIS export.

## License

Copyright (C) 2025-2026 Sergei Abramov.

Make_Thin_Section is free software licensed under the GNU General Public
License, version 3 or (at your option) any later version
(`GPL-3.0-or-later`). See `LICENSE` for the complete license text.

This license permits use, study, modification, and redistribution, including
commercial use, subject to the terms of the GNU GPL. Distributed modified
versions must remain under a GPL-compatible license and must make the
corresponding source code available as required by the license.

Third-party acknowledgements and licensing information are provided in
`THIRD_PARTY_NOTICES.md`.

## Acknowledgements And External Code

The numerical field-coordinate tables used by the QAPF tab were adapted from
the plutonic and volcanic QAPF implementations in GCDkit, which is distributed
under `GPL (>= 2)`. The GCDkit project is available at
[www.gcdkit.org](https://www.gcdkit.org/).

Janousek, V.; Farrow, C. M.; Erban, V. (2006). Interpretation of whole-rock geochemical data in igneous geochemistry: introducing Geochemical Data Toolkit (GCDkit). Journal of Petrology 47(6): 1255-1259. doi: 10.1093/petrology/egl013.

For the full notice, see `THIRD_PARTY_NOTICES.md`.
