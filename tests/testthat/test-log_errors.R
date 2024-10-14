test_that("A descriptive error shall occur if a user is not in an Rproj", {
  temp_dir <- tempdir()
  withr::with_dir(temp_dir, {
    logs <- capture.output({
      shiny::testServer(ghqc_assign_server, {
        #Sys.sleep(5)
        #res <- reactive_val()
       # stopApp()
      })
    })
  })
  browser()
  logs_collapsed <- glue::glue_collapse(logs, sep = "\n")
  expect_true(stringr::str_detect(
    logs_collapsed,
    "[ERROR] There was no Rproj file found within the directory"
    )
  )



})
