# ghqc (development version)

# ghqc 0.0.0.9004

## New features

-   Changes the available commits comparison in `ghqc_update_app()` from:

    1.  Initial QC commit and most recent QC issue update comment commit
    2.  Previous QC issue update comment commit and most recent QC issue update comment commit

    to:

    1.  Initial QC commit and most recent commit
    2.  Selectable "Reference" and "Comparator" commits (where Comparator is newer/more recent chronologically)

-   Adds a "preview" button for each selected QC file to allow users to preview the contents of the file in `ghqc_create_app()`.


-   Converts previous file tree from `shinyWidgets::treeInput()` to `jsTreeR::treeNavigatorServer()`/`jsTreeR::treeNavigatorUI()`.

    -   Loads only files that are from the opened directories rather than recursively getting the entire directory.

    -   Uses undetermined state on top level directories to prevent deselection unless all children are deselected.

    -   Filters out all binary files and returns a modalDialog that prevents further indexing into a directory if the directory only contains binary files and shows a list of the files. See `exclude_patterns()` and `list.files_and_dirs()` for full accounting of items that are excluded from the file tree.

-   generate_qc_report() can take a vector milestones as its input, as well as an optional just_tables flag that will only output the tables in the report.


## Minor improvements and bug fixes

-   Adds additional status check to prevent issue creation in `ghqc_create_app()` if there is already an existing issue name of the selected file in the same milestone name.

-   Changes the checklist info button in `ghqc_create_app()` from a question mark symbol to text ("checklist info") to better show what it is for.

-   Adds "No Assignee" to dropdown selection for the individual file selection assignee and now defaults to it rather than first available assignee in `ghqc_create_app()`.

-   Moves all modalDialog (pop-ups) buttons to the top right for ease of closing without scrolling.

-   author in metadata is now the git user who published the most recent version of the script

-   file hashes for reference and comparator added to comment metadata

-   removes empty milestones in `get_open_milestone_objects()` and `get_open_milestone_object_names()`

-   `check_if_updates_since_init()` function

-   generate_qc_report() errors if any inputted milestones don't exist or are empty

-   in issue body metadata and report: author is the most recent modifier of a script on github and collaborators are other editors of the script (only appears if there are any collaborators)

-   in issue body metadata, file history url is now listed

-   in generate_qc_report(), Issue section renamed to QC data - file name removed (because that's the section name, and is thus redundant), milestone description is listed if it exists. 

# ghqc 0.0.0.9003

## Minor improvements and bug fixes

-   Adds sorting by open/closed items to `ghqc_update_server()`/`ghqc_update_app()` for milestone specific issues.

-   Adds logging messages and timers to app initialization items and logging messages to gh api interactions.

-   Closes assignee dropdown box after selection in `ghqc_create_server()`/`ghqc_create_app()`.

-   fixed bug in milestone.R function milestone_exists

-   added checklists with subheaders to drop down in `ghqc_create_app()`

-   added link to github milestone in `ghqc_create_app()` success pop-up

-   added link to github issue in `ghqc_update_app()` success pop-up

-   improved summary table formatting in `generate_qc_report()`

-   fixed file difference bug in `ghqc_update_app()`

# ghqc 0.0.0.9002

## Minor improvements and bug fixes

-   Fixes `pmx_list()` so that it works on older versions of R.
