#' @import shiny
#' @importFrom jsTreeR renderJstree jstree jstreeOutput
#' @importFrom pkglite ext_binary
#' @importFrom fs dir_ls dir_exists is_file is_dir
NULL

# repurposed some functions and js from https://github.com/stla/jsTreeR
# changes: allows more filtering of lists, prevents binary file selections, renames rootFolder to basename

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

  lfs <- fs::dir_ls(path = path, all = all.files, regexp = pattern, recurse = FALSE, ignore.case = TRUE, invert = TRUE)

  # if lfs returns an empty list because all files were filtered out, dir_ls is rerun
  # to expose those files to show user as to why dir is not able to be indexed into
  # can't recursively look into a dir at top level because that will end up running
  # the whole proj dir, which can cause significant slow down if large amt of files
  # TODO: rewrite so it gives back all dir_ls initially and the grepl afterwards
  # lfs <- lfs[!grepl(exclude_patterns(), lfs)]


  if (length(lfs) == 0) {
    list_all <- fs::dir_ls(path = path, all = TRUE, regexp = FALSE, recurse = FALSE, ignore.case = TRUE, invert = TRUE)
    return(list(files = list_all, empty = TRUE))
  }

  # non_empty_dirs <- sapply(lfs, function(x) {
  #   if (fs::dir_exists(x)) {
  #     length(fs::dir_ls(x)) > 0
  #   } else {
  #     TRUE
  #   }
  # })
  #
  # # remove dirs w/o files as otherwise will be unclickable dir
  # lfs <- lfs[non_empty_dirs]

  files <- sort(lfs[fs::is_file(lfs)])
  dirs <- sort(lfs[fs::is_dir(lfs)])

  files_and_dirs <- c(dirs, files)
  return(list(files = files_and_dirs, empty = FALSE))
}


treeNavigatorUI <- function(id, width = "100%", height = "auto"){
  if(grepl("-", id)){
    stop("The `id` must not contain a minus sign.")
  }
  outputId <- NS(id, "treeNavigator___")
  tree <- jstreeOutput(outputId, width = width, height = height)
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

    output[["treeNavigator___"]] <- renderJstree({
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
    observeEvent(input[["treeNavigator____selected_paths"]], {
      selected <- input[["treeNavigator____selected_paths"]]
      adjusted_paths <- sapply(selected, function(item){
        fs::path_rel(item[["path"]], start = basename(rootFolder))
      })
      Paths(adjusted_paths)
    })

    Paths
  })
}
