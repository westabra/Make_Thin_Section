# Make_Thin_Section

Make_Thin_Section is a local Shiny application for maintaining a structured database of petrographic thin-section descriptions.

The application provides a practical interface for building and using a petrographic-description database: field metadata, microscopic rock description, mineral composition, image captions, lithological interpretation, QAPF plotting, Word report generation, and project-level GIS export are handled from one Shiny panel.

Author: Sergei Abramov  
Contact: westabra@gmail.com  
Current public app file: `app_Make_Thin_Section_version_61_improve_setting.R`

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
- Expands common mineral abbreviations from `minerals.csv`.
- Uses `biblio_for_mineral_descriptions.csv` as a reference library for mineral morphology and genesis text suggestions.
- Stores, orders, captions, compresses, and reuses sample images.
- Exports petrographic descriptions to DOCX using `petro_template.docx`.
- Provides a QAPF diagram tool for manual modal values.
- Links samples to lithological / mapping-unit classifications from `lookup_rocktype.csv`.
- Displays project samples on a Leaflet map.
- Exports project point layers as KML, Shapefile ZIP, or GeoPackage.
- Exports clean static project maps as PNG or PDF.
- Includes a separate Image Fragmentator tool for petrographic image segmentation experiments.

## Intended Users

This app is intended for geologists, petrographers, students, and researchers who need a local, reproducible way to describe thin sections and keep sample descriptions, images, and report exports together.

It is not an automatic mineral-identification system. Mineral names, textures, interpretations, and lithological assignments remain expert-controlled user inputs.

## Main Workflow

1. Start the app.
2. Select or create a project in the left panel.
3. Enter the sample ID and basic field metadata.
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

The left panel is the central sample form. The tabs on the right provide supporting work areas: mineral table, database loading tools, QAPF, reference lookup, map/GIS export, settings, and image management.

## Repository Files

Keep these files in the same folder unless you also update paths in the R code:

- `app_Make_Thin_Section_version_61_improve_setting.R` - main public Shiny application.
- `run_demo.R` - demo launcher. This is the file most new users should run first.
- `install_main_dependencies.R` - installs missing packages required by the main application and its export tools.
- `install_fragmentator_dependencies.R` - installs missing packages required by Image Fragmentator, including `EBImage`.
- `create_demo_sqlite.R` - developer / maintenance script for rebuilding the demo database and demo images.
- `Make_Thin_Section_demo.sqlite` - small two-project demonstration database stored next to the app.
- `images/` - image library folder used by the demo database and by local SQLite databases stored in this app folder.
- `image_fragmentator_V6.R` - standalone image-segmentation helper app.
- `petro_template.docx` - Word template used for DOCX export.
- `minerals.csv` - mineral names and abbreviation mapping.
- `biblio_for_mineral_descriptions.csv` - reference phrases for mineral descriptions.
- `grainsizes.csv` - grain-size vocabulary.
- `textures.csv` - texture vocabulary and interpretation support.
- `lookup_rocktype.csv` - lithology / mapping-unit classifier source.
- `rock_name_list.csv` - optional supporting rock-name vocabulary; version 61 does not load it automatically.
- `White et. al. Abbreviations for names of rock-forming minerals.csv` - optional supporting abbreviation reference; version 61 does not load it automatically.

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
sf, leaflet, DBI, RSQLite
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
shiny::runApp("app_Make_Thin_Section_version_61_improve_setting.R")
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
shiny::runApp("app_Make_Thin_Section_version_61_improve_setting.R")
```

## Data Storage

By default, the application stores data in:

```text
C:/GCDkit.data/petro_db/petro_v3.sqlite
```

Images and exports are stored beside that database:

```text
C:/GCDkit.data/petro_db/images/
C:/GCDkit.data/petro_db/exports/
```

On first use, the app creates the SQLite database schema and the required folders if they do not already exist.

To use another database location without editing the app code, set the `PETRO_DB_PATH` environment variable before launching:

```r
Sys.setenv(PETRO_DB_PATH = "C:/path/to/your/petro_demo.sqlite")
shiny::runApp("app_Make_Thin_Section_version_61_improve_setting.R")
```

If `PETRO_DB_PATH` is not set, the app uses the default path shown above.

The included `Make_Thin_Section_demo.sqlite` is an example of this approach: it is stored next to the Shiny script and can be launched through `run_demo.R`.

## Read-Only Mode

The app supports a read-only LAN mode through the environment variable `SHINY_READ_ONLY`.

```r
Sys.setenv(SHINY_READ_ONLY = "TRUE")
shiny::runApp("app_Make_Thin_Section_version_61_improve_setting.R")
```

In read-only mode, the app opens the existing SQLite database without allowing write operations such as saving samples, creating projects, deleting records, or importing classifier rows.

## Main Interface Areas

### Sample Metadata

The left panel contains the primary sample record: sample ID, field rock name, field notes, coordinates, microscope rock name, grain size, primary texture, project, author, and lithological interpretation.

### Rock Description

This tab contains the mineral-composition editor, the microscopic description field, and the selected image/caption preview used for report export.

### Database Tools

This tab is used to create projects, load saved samples, delete samples or projects, optimize the stored image library, and repair image paths if files have moved.

### QAPF

This tab plots manual Q, A, P, and F values on a Streckeisen-style QAPF diagram. It supports plutonic and volcanic diagram modes and can export the diagram as an image.

Some helper functions used in the QAPF tab were adapted from GCDkit. Please cite Janousek, Farrow and Erban (2006) when this part of the application is used in published work.

### Mineral Reference

This tab searches the mineral-description reference library and inserts selected morphology/genesis text into the mineral table editor.

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

For public release, consider adding:

- a license file;
- a citation file (`CITATION.cff`);
- example screenshots;
- screenshots based on the included demo database;
- a short example DOCX export;
- a short example GIS export;
- a launcher script for non-demo local work.

## Current Limitations

- The default database path is configured in the R script, but it can be overridden with `PETRO_DB_PATH`.
- The first screen is powerful but dense; new users should follow the workflow section above.
- Some features assume Windows-style paths and local desktop use.
- Advanced basemap export depends on web rendering, installed screenshot tools, and internet availability for map tiles.
- The app is not a validated analytical instrument; petrographic interpretation remains the responsibility of the user.

## Suggested Citation

Abramov, S. (2025-2026). Make_Thin_Section: a Shiny application for petrographic thin-section description, image management, DOCX reporting, and project GIS export.

## Acknowledgements And External Code

The QAPF tab uses some helper functions adapted from GCDkit:

Janousek, V.; Farrow, C. M.; Erban, V. (2006). Interpretation of whole-rock geochemical data in igneous geochemistry: introducing Geochemical Data Toolkit (GCDkit). Journal of Petrology 47(6): 1255-1259. doi: 10.1093/petrology/egl013.
