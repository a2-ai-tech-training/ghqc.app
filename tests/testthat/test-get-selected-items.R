create_test_tree <- function() {
  list(
    folder1 = structure(list(
      file1 = structure("", stselected = TRUE, id = 1),
      file2 = structure("", stselected = FALSE, id = 2),
      subfolder1 = structure(list(
        file3 = structure("", stselected = TRUE, id = 3),
        file4 = structure("", stselected = FALSE, id = 4)
      ), stselected = TRUE)
    ), stselected = TRUE),
    folder2 = structure(list(
      file5 = structure("", stselected = TRUE, id = 5)
    ), stselected = FALSE),
    file6 = structure("", stselected = TRUE, id = 6)
  )
}


test_that("get_selected_items handles empty tree", {
  tree <- list()
  result <- get_selected_items(tree)
  expect_equal(result, list())
})


# test_that("get_selected_items handles selected items", {
#   tree <- create_test_tree()
#
#   result <- get_selected_items(tree)
#
#   expected_result <- list(
#     folder1 = list(
#       file1 = 1,
#       subfolder1 = list(
#         file3 = 3
#       )
#     ),
#     folder2 = list(
#       file5 = 5
#     ),
#     file6 = 6
#   )
#
#   expect_equal(result, expected_result)
# })

test_that("get_selected_otems handles selected items", {
  tree <- create_test_tree()

  result <- get_selected_items(tree)

  expected_result <- list(
    folder1 = list(
      "folder1/file1" = 1,
      subfolder1 = list(
        "folder1/subfolder1/file3" = 3
      )
    ),
    folder2 = list(
      "folder2/file5" = 5
    ),
    "file6" = 6
  )

  expect_equal(result, expected_result)
})


test_that("get_selected_items excludes unselected items", {
  tree <- list(
    folder1 = structure(list(
      file1 = structure("", stselected = FALSE, id = 1),
      file2 = structure("", stselected = FALSE, id = 2)
    ), stselected = FALSE)
  )

  result <- get_selected_items(tree)

  expect_equal(result, list())
})
