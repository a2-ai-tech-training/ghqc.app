run_app_in_background <- function(script) {
  port <- httpuv::randomPort()
  Sys.setenv("GHQC_SHINY_PORT" = port)
  job_id <- rstudioapi::jobRunScript(script)
  Sys.sleep(5)
  rstudioapi::viewer(sprintf("https://127.0.0.1:%s", port))
}
