# Make Thin Section v70 Lite

This release contains the complete Make Thin Section application, reference tables,
Word template and educational demo. R packages, Image Fragmentator, Python and model
weights are not included. RStudio is optional.

## First start on Windows

1. Install **64-bit R 4.5 or later** from https://cran.r-project.org/bin/windows/base/.
   R 4.5.3 was used for release checks. Extract the ZIP to a writable folder.
2. Double-click **1_Check_and_install_dependencies.cmd**. Internet access is needed
   to download missing R packages and their required dependencies from CRAN.
   Leave the console open until it reports SUCCESS. Existing loadable packages are reused.
3. Double-click **3_Start_Demo.cmd**. A browser opens the educational demo.
4. To use your database, edit **MTS_DATABASE** in **config.R**, then run
   **2_Start_Make_Thin_Section.cmd**. Until changed, this also opens the demo.
   Keep the database's associated images folder beside the database.

Packages are installed in a user-owned MakeThinSection library outside the release
folder, separated by R major/minor version. Setup prints the exact location. You can
override it with MTS_R_LIBRARY. The ZIP stays small, but downloaded dependencies still
occupy disk space. Do not run the installer as administrator.

The installer uses CRAN's available Windows binaries; it does not freeze all transitive
dependencies. R_REQUIREMENTS.csv records the tested direct-package versions and licenses.
Installed versions are reported in logs/installed_R_packages.csv. Check the demo if versions differ.
If setup fails, review the console and logs/dependency-install.log, fix the connection or
R installation, and rerun setup. It does not reinstall packages that already load.

## Launch modes

| File | Purpose |
|---|---|
| 2_Start_Make_Thin_Section.cmd | Local editing, http://127.0.0.1:3081 |
| 3_Start_Demo.cmd | Educational demo, same local port |
| 4_Start_LAN_readonly.cmd | Read-only viewing, port 3080; no automatic browser |
| run_local_edit.R | Same editing mode when sourced in R/RStudio |
| run_LAN_readonly.R | Same viewing mode when sourced in R/RStudio |

Each CMD launcher starts Rscript directly. Keep its console open; Ctrl+C stops the app.
For LAN viewing set MTS_LAN_HOST in config.R to your server's actual LAN IP, then open
http://YOUR_SERVER_IP:3080 from the other computer. The release defaults to localhost.
Readonly mode disables SQLite editing and autosave/recovery. It is not an authentication system.
For trusted LAN use, allow the required connection in the server's firewall if necessary.
MTS_PORT overrides the selected mode's port. MTS_BROWSER=FALSE suppresses automatic browsing.

If R cannot be found, create **R_PATH.txt** with one line containing the full path to
Rscript.exe, without quotation marks, for example C:/Program Files/R/R-4.5.3/bin/Rscript.exe.

## Optional Image Fragmentator

Make Thin Section works without it. To connect an existing **7.7.0 Portable** installation,
open Image container (sorted), click **Configure Fragmentator**, enter its root folder and
save. The path is stored in fragmentator_path.txt and survives a restart. Install its own
dependencies using its own setup instructions; the Make Thin Section installer does not do so.
MTS_FRAGMENTATOR_ROOT can override the saved path. No Fragmentator release URL is assumed.
Launch and stop it using the adjacent buttons. Export results before stopping it or closing
Make Thin Section. On a remote LAN client, run Fragmentator on that client separately.

## Updating from an existing V70

Extract this release into a new folder. Copy your database setting into config.R; keep the
actual working database and images at their existing location. Do not replace a working
database with the educational demo. R packages already available to this R installation
are reused; private libraries belonging to an older portable bundle are not automatically copied.
Database-specific recovery drafts remain available when the database path stays the same.

See README.md for the full user guide and RELEASE_NOTES.md for changes.

---

# Make_Thin_Section

Make_Thin_Section is a local Shiny application for maintaining a structured database of petrographic thin-section descriptions.

The application provides a practical interface for building and using a petrographic-description database: field metadata, microscopic rock description, mineral composition, image captions, lithological interpretation, QAPF plotting, Word report generation, and project-level GIS export are handled from one Shiny panel.

Author: Sergei Abramov  
Contact: westabra@gmail.com  
Current app file: `app_Make_Thin_Section_v70.R` (v70 Lite, updated 2026-10-13)  
Version 62 DOI: [10.13140/RG.2.2.18311.97448](https://doi.org/10.13140/RG.2.2.18311.97448)

## Version 66: Work recovery and autosave

Start the updated version with `source("run_local_edit.R")`. Keep the v70 app, launcher, README.md and support files in the same application folder. Earlier launchers still start their earlier app versions. For LAN viewing use `run_LAN_readonly.R`; its host address and port must match the serving computer.

### Open Work recovery

The light-green **Work recovery (Click to open)** control is below **New Sample**, **Reset Form** and **Clone previous sample** in the left panel. Click it to expand or collapse the connection status, browser text-backup status, local draft status, **Save draft now** and **Restore draft**. It starts collapsed. Automatic saving continues while it is collapsed. Available drafts no longer trigger a persistent notification; open **Restore draft** when you need to inspect them.

### Restore unfinished work

1. After an interruption, restart the app in local edit mode and open the browser page. Use the same database path to find its local drafts; browser text backups also require the same browser profile and origin (address and port).
2. Expand **Work recovery** and click **Restore draft**. The app attempts to preserve the current form as a draft before showing available recovery copies.
3. Choose by date/time, project and sample ID. Local drafts include mineral/texture tables, images and captions, plus pending editor text. Entries marked **browser text only** do not include tables or images; restoring these clears unrelated tables and images from the current form.
4. Click **Restore selected draft**. Restoration replaces the current form, without writing to SQLite. Review all restored fields, tables and images, and add any pending mineral/texture editor entries to their tables as appropriate.
5. Use **Save sample to SQLite** to save the reviewed description to the database.

**Save draft now** makes a local recovery copy even when automatic local saving is off. Drafts are recovery aids: the most recent changes may be missing after a crash or interrupted connection. A saved draft is not a saved SQLite sample.

### Autosave and recovery settings

At the bottom of **Settings**, click **Autosave and recovery...**. Its separate window controls local autosave, browser text backup, saving intervals, retained revisions per sample/session, retention and cleanup, and the storage-size warning. It also displays the local draft folder and storage usage.

Click **Save autosave settings** to apply and persist changes. **Close** discards unsaved setting changes when the window is next opened. Saved preferences apply to this session; reload other open sessions to use them. Defaults enable local autosave and browser backup, save after a 1.5-second pause and check every 10 seconds, and retain three revisions per sample/session. Age-based deletion and automatic cleanup are off by default.

Drafts and preferences are stored separately from SQLite, in a database-specific folder under `%LOCALAPPDATA%/MakeThinSection/drafts` on Windows, or under `MTS_DRAFT_DIR` when configured. Keep the database path unchanged to reuse the same recovery folder.

Before manual cleanup, inspect the preview and confirm only if those old drafts are no longer needed. Cleanup can permanently remove descriptions never saved to SQLite. Recent image copies, active sessions and unreadable draft revisions are protected. The size warning alone does not delete files; automatic cleanup requires its own setting. **Back to autosave settings** returns from the cleanup preview.

### LAN read-only mode

With `SHINY_READ_ONLY=TRUE` (the LAN read-only launcher), **Work recovery** and **Autosave and recovery...** are hidden and unavailable. The server does not register recovery handlers, read local drafts/preferences, save browser or local drafts, or run draft cleanup. Recovery settings and drafts therefore cannot be viewed or edited from that session. Existing drafts remain available for a later local edit session.

**About Program** in Settings identifies V70 Lite. Recovery behavior introduced in v66 is retained.

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
- Can launch an optional external Image Fragmentator 7.7.0 Portable installation; it is not included in Lite.

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

## Installation and data storage

Follow START_HERE.md for installation, launch modes and optional external Fragmentator configuration. The demo is educational; point config.R to your own existing SQLite database for production work. Keep its images folder alongside it and back up both database and images.

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
`app_Make_Thin_Section_v70.R`. The external
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

- `MTS_DATABASE` in `config.R`;
- local controlled-vocabulary CSV files;
- the DOCX template;
- map export settings;
- package installation for `sf`, `magick` and `webshot2` (Fragmentator dependencies are separate).

Possible additions for future releases include a citation file
(`CITATION.cff`), example screenshots, example DOCX and GIS exports, and a
additional export examples.

## Current Limitations

- The default database path is configured in config.R, but it can be overridden with `PETRO_DB_PATH`.
- The first screen is powerful but dense; new users should follow the workflow section above.
- Some features assume Windows-style paths and local desktop use.
- Advanced basemap export depends on web rendering, installed screenshot tools, and internet availability for map tiles.
- The app is not a validated analytical instrument; petrographic interpretation remains the responsibility of the user.

## Suggested Citation

Abramov, S. (2026). *Make_Thin_Section v62: a Shiny application for petrographic thin-section description, image management, DOCX reporting, and project GIS export*. ResearchGate. [https://doi.org/10.13140/RG.2.2.18311.97448](https://doi.org/10.13140/RG.2.2.18311.97448)

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

## Rock-description search and additional modal diagrams

### Database tools: Search rock descriptions

On **Database tools**, project creation/deletion and sample loading/deletion appear first. Open the green **Search rock descriptions (Click to open)** panel below these controls to use search; it starts collapsed.

Choose **One project**, **Selected projects**, or **Entire database**. For a selected-project search, choose one or more project names. Use **Refresh project list** after adding projects elsewhere.

Enter a word or phrase in any of these fields:

- **4) Rock name (microscope identification)** searches the saved microscope rock name.
- **2a) Field Rock Name (field identification)** searches the saved field rock name.
- **Suggested lithological unit** searches the name of the linked lithological unit.
- **Mineral composition** searches mineral names, volume text, morphology and genesis across all mineral rows for each sample.

Matching is case-insensitive, including Unicode text. Each box is a literal substring: a phrase is searched as entered, and characters such as `%`, `_`, `*` and quotes are not wildcards. Empty fields are ignored. **Match all (AND)** requires every filled field to match; **Match any (OR)** requires at least one. Click **Search** after changing filters. Searches use saved database records, not unsaved form edits or recovery drafts.

Results contain one row per project/sample pair. Select a row to preview its descriptions. **Open selected sample** asks before replacing the current form. **Export results (CSV)** exports the complete result of the last search, including descriptions, regardless of the visible table page. Text that could be interpreted as a spreadsheet formula is prefixed with an apostrophe in the export.

Searching, previewing and downloading results are available in LAN read-only mode. Search connections never write to SQLite and do not modify its schema.

### Rock classification

The **Rock classification** tab adds five manual, single-sample ternary tools alongside the existing QAPF tab:

1. Amphibolite modal field: Remaining constituents / Plagioclase / Amphibole.
2. Olivine / Orthopyroxene / Clinopyroxene.
3. Olivine / Pyroxene / Hornblende.
4. Plagioclase / Pyroxene / Olivine.
5. Plagioclase / Pyroxene / Hornblende.

Choose a diagram, enter non-negative modal values and an optional sample label, then click **Plot sample**. The three components are normalized to 100%; a zero total is rejected. For amphibolite, include every remaining mineral in **Remaining constituents**. The axes for the igneous diagrams represent only the named components. Pyroxene means orthopyroxene plus clinopyroxene.

The display identifies the diagram field. On a shared boundary, adjacent fields are listed. The amphibolite result describes modal-field membership; it does not determine metamorphic texture or amphibole identity. For the igneous diagrams, first establish whether the whole rock belongs to the relevant mafic or ultramafic classification. Plagioclase-pyroxene diagrams alone do not distinguish gabbro, gabbronorite and norite, or gabbro from diorite.

Use **Download PNG** or **Download PDF** to save the plotted result. **Add diagram to Image container** adds an image and caption to the current editable sample so it can be included in a Word report. In LAN read-only mode, plotting and downloading remain available; adding to the image container is blocked. These tools do not automatically replace the rock name or write to SQLite. Downloads reflect the last **Plot sample** action.

The diagrams follow the numerical geometry of BGS Figures 13 and 16. In the olivine diagrams, the 5% minor-component boundaries continue straight across Ol=40%. The basal end triangles use 90% of the dominant basal component and reach Ol=10%; the central olivine-free band ends at Ol=5%. Only the split between the two mixed pyroxene/hornblende fields uses Px=Hbl. The Plagioclase-Pyroxene-Hornblende base has four fields: plagioclase-bearing pyroxenite, hornblende pyroxenite, pyroxene hornblendite, and hornblendite. Dashed external guides label the broad BGS groups and the leuco-, meso-, and mela- bands (Pl=90-65%, 65-35%, and 35-10%). These group guides do not replace the solid field boundaries. The amphibolite modal region uses amphibole >=30%, plagioclase >=5%, and their combined abundance >=75%.

References: [BGS Rock Classification Scheme, Volume 1, Figs. 13 and 16](https://nora.nerc.ac.uk/id/eprint/3223/1/RR99006.pdf); [SCMR IGC2004 report](https://www2.bgs.ac.uk/scmr/docs/IGC2004.pdf); [IODP Expedition 399 methods](https://publications.iodp.org/proceedings/399/EXP_REPT/CHAPTERS/399_102.PDF). The four igneous diagrams were rechecked against the supplied BGS original images on 2026-10-12. Field numbers are local legend references, not official BGS identifiers.
