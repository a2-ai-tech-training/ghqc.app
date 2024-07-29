#' @import shiny
#' @importFrom jsTreeR renderJstree jstree jstreeOutput
#' @importFrom pkglite ext_binary
#' @importFrom fs dir_ls dir_exists is_file is_dir
NULL

# repurposed some functions and js from https://github.com/stla/jsTreeR
# changes: allows more filtering of lists, excluded dir selections of only excluded
# files give modals w/ excluded file list, renames rootFolder to basename,
# redid id/naming so ns can be passed in

exclude_patterns <- function(){
  # excludes binaries as won't be qc items
  exclude_pattern <- paste0("\\.(", paste(ext_binary(flat = TRUE), collapse = "|"), ")$", collapse = "")

  # removes renv folder and specifically
  # makes sure to only scope exactly for "renv/" only so renv2/ 2renv/ renv.R gets picked up
  exclude_pattern <- c(exclude_pattern, "\\brenv\\b")
  exclude_pattern <- paste(exclude_pattern, collapse = "|")
  return(exclude_pattern)
}

list.files_and_dirs <- function(path, pattern, all.files){
  # changed so pattern is only filtered out after retrieving all non filtered out values
  lfs <- fs::dir_ls(path = path, all = all.files, regexp = NULL, recurse = F, ignore.case = TRUE, invert = TRUE)
  included_files <- lfs[!grepl(pattern, lfs)]

  non_empty_dirs <- sapply(included_files, function(x) {
    if (fs::dir_exists(x)) {
      length(fs::dir_ls(x)) > 0
    } else {
      TRUE
    }
  })

  # remove dirs w/o ANY files as otherwise will be unclickable dir
  if (any(!non_empty_dirs)) {
    included_files <- included_files[non_empty_dirs]
  }

  # if included_files returns an empty list because all files were filtered out, dir_ls is rerun
  # w/ recurse to expose those files to show user as to why dir is not able to be indexed into
  # didn't reuse lfs because wanted only files rather than both files and dirs + recurse
  if (length(included_files) == 0) {
    list_all <- fs::dir_ls(path = path, all = TRUE, regexp = NULL, recurse = T, ignore.case = TRUE, type = "file")
    return(list(files = list_all, empty = TRUE))
  }

  files <- sort(included_files[fs::is_file(included_files)])
  dirs <- sort(included_files[fs::is_dir(included_files)])

  files_and_dirs <- c(dirs, files)
  return(list(files = files_and_dirs, empty = FALSE))
}


treeNavigatorUI <- function(id, width = "100%", height = "auto"){
  tree <- jstreeOutput(outputId = id, width = width, height = height)
  tagList(tree,
          tags$link(rel = "stylesheet", type = "text/css", href = "ghqc/css/tree.css"),
          tags$script(type = "module", src = "ghqc/js/tree.js"))
}

treeNavigatorServer <- function(
    id, rootFolder, search = TRUE, wholerow = FALSE, contextMenu = FALSE,
    theme = "proton", pattern = NULL, all.files = FALSE, ...
){
  theme <- match.arg(theme, c("default", "proton"))
  moduleServer(id, function(input, output, session){

    output[["treeNavigator"]] <- renderJstree({
      req(...)

      suppressMessages(jstree(
        nodes = list(
          list(
            text = basename(rootFolder),
            type = "folder",
            children = FALSE,
            li_attr = list(
              class = "jstree-x"
            )
          )
        ),
        types = list(
          folder = list(
            icon = "fa fa-folder"
          ),
          file = list(
            icon = "far fa-file"
          )
        ),
        checkCallback = TRUE,
        theme = theme,
        checkboxes = TRUE,
        search = search,
        wholerow = wholerow,
        contextMenu = contextMenu,
        selectLeavesOnly = TRUE
      ))
    })

    # changed text of rootFolder to give back basename so need to
    # reconstruct original/full pathing of files to allow js to incrementally load in files
    dirname <- dirname(rootFolder)

    # example: given input "testTree/inst/www", full_path will be "/path/to/proj/testTree/inst/www"
    observeEvent(input[["path_from_js"]], {
      input <- input[["path_from_js"]]

      # null is sent back to reset the input if user wants to reselect unviable dirs
      if(is.null(input)){
        return()
      }
      full_path <- fs::path(dirname, input)

      lf <- list.files_and_dirs(full_path, pattern = pattern, all.files = all.files)

      # if no viable children found, send msg to revert state and open modal
      # otherwise tree state will have miscalculated state and think node exists when it does not
      if (lf$empty) {
        message_content <- generate_binary_file_message(lf$files)
        session$sendCustomMessage("noChildrenFound", lf$empty)
        return(showModal(modalDialog(
          title = tags$div(modalButton("Dismiss"), style = "text-align: right;"),
          footer = NULL,
          easyClose = TRUE,
          HTML(message_content)
          )
          )
        )
      }

      fi <- file.info(lf$files, extra_cols = FALSE)
      x <- list(
        "elem"   = as.list(basename(lf$files)),
        "folder" = as.list(fi[["isdir"]])
      )

      session$sendCustomMessage("getChildren", x)
    })

    # example: given input "testTree/inst/www", Paths is "inst/www"
    Paths <- reactiveVal()
    observeEvent(input[["treeNavigator_selected_paths"]], {
      selected <- input[["treeNavigator_selected_paths"]]
      adjusted_paths <- sapply(selected, function(item){
        fs::path_rel(item[["path"]], start = basename(rootFolder))
      })
      Paths(adjusted_paths)
    })

    Paths
  })
}
