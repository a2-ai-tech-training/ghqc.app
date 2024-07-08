run_app_in_background <- function(app_name, pkg_path = "~/ghqc") {
  script <- tempfile("background", tmpdir = getwd(), fileext = ".R")

  writeLines(glue::glue(".libPaths(\"~/ghqc-rpkgs/\")
                        devtools::load_all(\"{pkg_path}\")
                        library(ghqc)
                        {app_name}")
             , script)

  port <- httpuv::randomPort()
  Sys.setenv("GHQC_SHINY_PORT" = port)
  job_id <- rstudioapi::jobRunScript(script)
  Sys.sleep(5)
  withr::defer(fs::file_delete(script))
  rstudioapi::viewer(sprintf("https://127.0.0.1:%s", port))
}
