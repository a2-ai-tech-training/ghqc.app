create_test_dir_structure <- function(base_path) {
  dir.create(file.path(base_path, "dir1"))
  dir.create(file.path(base_path, "dir2"))
  file.create(file.path(base_path, "file1.R"))
  file.create(file.path(base_path, "file2.mod"))
  file.create(file.path(base_path, "file3.txt"))
  file.create(file.path(base_path, "dir1", "file4.cpp"))
  file.create(file.path(base_path, "dir1", "file5.txt"))
  file.create(file.path(base_path, "dir2", "file6.R"))
}

test_that("get_files_tree_list handles empty directory", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  expect_equal(get_files_tree_list(temp_dir), list())
})

test_that("get_files_tree_list handles directory with files and subdirectories", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  create_test_dir_structure(temp_dir)

  result <- get_files_tree_list(temp_dir)

  expect_true(is.list(result))
  expect_named(result, c("dir1", "dir2", "file1.R", "file2.mod", "file3.txt"))
  expect_true(is.list(result$dir1))
  expect_true(is.list(result$dir2))
  expect_equal(attr(result$file1.R, "sttype"), "file")
  expect_equal(attr(result$file1.R, "sticon"), "fa fa-file")
  expect_equal(attr(result$file2.mod, "sttype"), "file")
  expect_equal(attr(result$file2.mod, "sticon"), "fa fa-file")
  expect_equal(attr(result$dir1, "sttype"), "folder")
  expect_equal(attr(result$dir1, "sticon"), "fa fa-folder")
  expect_equal(attr(result$dir2, "sttype"), "folder")
  expect_equal(attr(result$dir2, "sticon"), "fa fa-folder")
})

# test_that("get_files_tree_list excludes non-matching file extensions", {
#   temp_dir <- tempfile()
#   dir.create(temp_dir)
#   on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
#   create_test_dir_structure(temp_dir)
#
#   result <- get_files_tree_list(temp_dir)
#
#   expect_false("file3.txt" %in% names(result))
#   expect_false("file5.txt" %in% names(result$dir1))
# })

test_that("get_files_tree_list includes matching file extensions in subdirectories", {
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))
  create_test_dir_structure(temp_dir)

  result <- get_files_tree_list(temp_dir)

  expect_true("file4.cpp" %in% names(result$dir1))
  expect_true("file6.R" %in% names(result$dir2))
  expect_equal(attr(result$dir1$file4.cpp, "sttype"), "file")
  expect_equal(attr(result$dir1$file4.cpp, "sticon"), "fa fa-file")
  expect_equal(attr(result$dir2$file6.R, "sttype"), "file")
  expect_equal(attr(result$dir2$file6.R, "sticon"), "fa fa-file")
})
