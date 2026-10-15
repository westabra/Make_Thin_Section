# Make Thin Section V70.1 Lite — easier connection settings

- Select the SQLite database and Image Fragmentator folder using Windows dialogs or paste ordinary Windows paths, including quotes from Copy as path.
- New 5_Configure_paths.cmd opens a local setup page before the main application starts.
- Database tools includes Choose database; Configure Fragmentator includes Choose folder.
- Saved database choices apply after restart. Existing sessions remain on their current database.
- Invalid or missing databases are rejected without creating a new file. Database validation is read-only.
- LAN read-only sessions cannot change paths. Existing environment overrides and config.R fallback continue to work.
- Updated English instructions and About Program version.

For a new installation, use the complete V70.1 Lite ZIP. For an existing V70 Lite folder, close the application and extract the path-settings update ZIP into it. The update excludes databases, images, config.R and private path settings.

## Previous V70 release

# Make Thin Section V70 Lite

Lightweight Windows release of Make Thin Section. R packages and Image Fragmentator are installed separately.

- Search saved descriptions by microscope rock name, field rock name, lithological unit and mineral composition, across one or multiple projects or the entire database.
- Five additional rock classification diagrams; corrected BGS field boundaries, group annotations and internal field numbers.
- Database tools keeps project/sample controls at the top and search in a collapsed green panel.
- Work recovery and autosave retained; unavailable in LAN readonly mode.
- Separate edit and readonly launchers run R directly without RStudio.
- First-run R dependency installer reuses available packages and downloads missing dependencies.
- Optional external Image Fragmentator 7.7.0 Portable path can be configured in the interface.

See START_HERE.md before first launch. Download the Lite ZIP attached to this release. The educational demo is not a replacement for an existing working database.
