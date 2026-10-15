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
4. Run **5_Configure_paths.cmd**, choose your database, and click **Check and save database**. Then run
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

## Choosing paths without R syntax (V70.1)

Run **5_Configure_paths.cmd** after installing dependencies. A local settings page opens in your browser. Use **Choose database...** to select an existing SQLite file and **Choose folder...** to select your Image Fragmentator installation. The Windows selection dialog opens on the computer running the program. Alternatively, paste an ordinary Windows path, for example `C:\GeoData\My rocks.sqlite` or `C:\GeoApps\Image_Fragmentator_7.7.0_Lite`. Quotes added by Windows **Copy as path** are accepted. Do not add R assignments or double the backslashes.

Click the corresponding **Check and save** button. Database validation opens the existing SQLite file read-only and checks for the samples table; it does not create or edit the database. Fragmentator validation checks its required files. It does not install dependencies or run segmentation.

You can also use **Database tools > Choose database...** and **Configure Fragmentator > Choose folder...** inside local editing mode. A saved database choice takes effect after restarting Make Thin Section; existing sessions stay connected to their current database. Path settings cannot be changed through LAN read-only mode or a remote browser session.

Paths are saved as plain text in **database_path.txt** and **fragmentator_path.txt** beside the launch files. They are private computer settings, not part of the public release. Keep your database's images folder beside the database. The demo launcher always uses the educational demo. Advanced overrides remain supported: **PETRO_DB_PATH > database_path.txt > MTS_DATABASE in config.R**, and **MTS_FRAGMENTATOR_ROOT > fragmentator_path.txt**. Editing config.R manually still requires valid R syntax; ordinary setup no longer requires editing it.

For an existing V70 Lite installation, close the program and extract the **V70.1 path-settings update ZIP** directly into its folder, allowing the listed program files to be replaced. The update contains no database, images, config.R or saved paths, so your LAN address and existing configuration remain in place. A restart is required.

## Optional Image Fragmentator

Make Thin Section works without it. To connect an existing **7.7.0 Portable** installation,
open Image container (sorted), click **Configure Fragmentator**, choose its root folder or paste a Windows path and
save. The path is stored in fragmentator_path.txt and survives a restart. Install its own
dependencies using its own setup instructions; the Make Thin Section installer does not do so.
MTS_FRAGMENTATOR_ROOT can override the saved path. No Fragmentator release URL is assumed.
Launch and stop it using the adjacent buttons. Export results before stopping it or closing
Make Thin Section. On a remote LAN client, run Fragmentator on that client separately.

## Updating from an existing V70

Extract this release into a new folder. Run 5_Configure_paths.cmd to select your existing database; keep the
actual working database and images at their existing location. Do not replace a working
database with the educational demo. R packages already available to this R installation
are reused; private libraries belonging to an older portable bundle are not automatically copied.
Database-specific recovery drafts remain available when the database path stays the same.

See README.md for the full user guide and RELEASE_NOTES.md for changes.
