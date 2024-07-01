run_app_in_background <- function() {
  port <- httpuv::randomPort()
  Sys.setenv("GHQC_SHINY_PORT" = port)
  job_id <- rstudioapi::jobRunScript(temp_file)
  Sys.sleep(5)
  rstudioapi::viewer(sprintf("https://127.0.0.1:%s", port))
}
