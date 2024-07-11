# ghqc (development version)

# ghqc 0.0.0.9004

## Minor improvements and bug fixes

-   Adds additional status check to prevent issue creation in `ghqc_create_app()` if there is already an existing issue name of the selected file in the same milestone name.

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
