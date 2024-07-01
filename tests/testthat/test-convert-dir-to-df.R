test_that("convert_dir_to_df handles directories and files", {
  df <- suppressMessages(convert_dir_to_df(dir_path = ".")) # used current wd against finding the default root dir as that needs an .Rproj file

  expected <- data.frame(
    level1 = c("📁 app","📄 test-convert-dir-to-df.R"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(expected)) {
    expect_true(nrow(subset(df, level1 == expected$level1[i])) > 0)
  }
})
