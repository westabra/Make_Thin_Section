Make_Thin_Section

The main project documentation is now in README.md.

Start there for:
- project overview;
- installation requirements;
- launch instructions;
- demo database launch instructions;
- main workflow;
- SQLite storage notes;
- DOCX, image, QAPF, and GIS export notes;
- publication notes for GitHub or ResearchGate.

Main app file:
app_Make_Thin_Section_version_62_mineral_reference_web_search.R

Before the first launch, install the main application dependencies:
source("install_main_dependencies.R")

To use Image Fragmentator, install its additional dependencies:
source("install_fragmentator_dependencies.R")

Demo launcher:
run_demo.R

For normal demo use, run only:
source("run_demo.R")

The included demo projects are:
- Isle_of_Skye_demo
- Oldoinyo_Lengai_demo

Source attribution is documented in README.md.

Developer / maintenance script:
create_demo_sqlite.R

Run create_demo_sqlite.R only if the demo database must be rebuilt.
