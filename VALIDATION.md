# V70 Lite validation

## V70.1 path settings update — 2026-10-15

- Checked plain Windows paths, quoted Copy as path values, UNC paths and Unicode strings.
- Verified database/environment/demo precedence and settings persistence.
- Validated an existing database read-only; its SHA-256 remained unchanged. Missing and invalid databases were rejected, with no new database created.
- Verified a quoted Windows path to the installed Image Fragmentator.
- Started real Rscript/Shiny sessions for local editing, LAN readonly and the standalone settings page, using a separate copy of the demo. HTTP checks passed; the database settings button is absent in readonly mode.
- Verified ZIP file hashes. The update archive excludes databases, images, config.R and saved paths.
- Native Windows file/folder selection is implemented with utils::choose.files and utils::choose.dir. Interactive selection clicks were not automated in this check.

## Original V70 checks

Validated on Windows with 64-bit R 4.5.3 on 2026-10-13.

- Installed CRAN binary dependencies in an initially empty, isolated R library.
- Increased download timeout to 600 seconds after large GIS package downloads exceeded the default 60 seconds.
- Reran setup successfully; available packages were reused.
- Passed 33 integration checks: syntax, demo database/image integrity, description search and CSV export, all five diagram PNG/PDF exports, full app startup in edit and readonly modes, collapsed search UI, readonly image-container protection, QAPF, Word/flextable export, XLSX, Leaflet HTML, sf coordinate transformation and image conversion.
- No production database was used or modified.
- External Fragmentator absence/configuration was checked. A full external segmentation session was not rerun for this release.
- Browser screenshot/basemap export depends on an installed Chrome/Chromium and network access; that external service was not validated here.

R_REQUIREMENTS.csv records the direct-package versions used for release checks. The installer downloads currently available CRAN binaries; this is not a fully locked, offline environment.
