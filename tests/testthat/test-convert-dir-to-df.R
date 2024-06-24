test_that("convert_dir_to_df handles directories and files", {
  df <- convert_dir_to_df(dir_path = find_root_directory())

  expected <- data.frame(
    level1 = c("📁 app","📄 test-convert-dir-to-df.R"),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(expected)) {
    expect_true(nrow(subset(df, level1 == expected$level1[i])) > 0)
  }
})
