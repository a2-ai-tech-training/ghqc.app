#' @import shiny
#' @importFrom jsTreeR renderJstree jstree jstreeOutput
#' @importFrom pkglite ext_binary
NULL

# repurposed some functions from https://github.com/stla/jsTreeR

exclude_patterns <- function(){
  # excludes binaries as won't be qc items
  exclude_pattern <- paste0("\\.(", paste(ext_binary(flat = TRUE), collapse = "|"), ")$", collapse = "")
  # makes sure to only scope exactly for "renv/" only so renv2/ 2renv/ renv.R gets picked up
  exclude_pattern <- c(exclude_pattern, "\\brenv\\b")
  exclude_pattern <- paste(exclude_pattern, collapse = "|")
  return(exclude_pattern)
}

list.files_and_dirs <- function(path, pattern, all.files){
  lfs <- dir_ls(path = path, all = all.files, regexp = pattern, recurse = F, ignore.case = TRUE, invert = TRUE)

  # returns a null if lfs is empty so observeEvent can send message to revert state
  if (length(lfs) == 0){
    return(NULL)
  }

  non_empty_dirs <- sapply(lfs, function(x) {
    if (fs::dir_exists(x)) {
      length(fs::dir_ls(x)) > 0
    } else {
      TRUE
    }
  })

  # remove dirs w/o files as otherwise will be unclickable dir
  lfs <- lfs[non_empty_dirs]

  files <- sort(lfs[fs::is_file(lfs)])
  dirs <- sort(lfs[fs::is_dir(lfs)])

  files_and_dirs <- c(dirs, files)
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
      jstree(
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
      )
    })

    # changed text of rootFolder to give back basename so need to
    # reconstruct original/full pathing of files to allow js to incrementally load in files
    dirname <- dirname(rootFolder)

    # example: given input "testTree/inst/www", full_path will be "/path/to/proj/testTree/inst/www"
    observeEvent(input[["path_from_js"]], {
      input <- input[["path_from_js"]]
      full_path <- fs::path(dirname, input)

      lf <- list.files_and_dirs(full_path, pattern = pattern, all.files = all.files)

      # if no viable children found, send msg to revert state and pop modal
      if (is.null(lf)){
        session$sendCustomMessage("noChildrenFound", lf)
        return(showModal(modalDialog("stop. no selectable files")))
      }

      fi <- file.info(lf, extra_cols = FALSE)
      x <- list(
        "elem"   = as.list(basename(lf)),
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
