devtools::load_all()
library(shinytest2)
library(ghqc)
#
# test_that("{shinytest2} recording: convert_dir_to_df renders from treeInput", {
#   app <- AppDriver$new(variant = platform_variant(), name = "empty_tree", height = 728, width = 609)
#   app$set_inputs(`ghqc_create_app-assignees` = character(0), wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone_description` = "", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone` = "", wait_ = FALSE)
#
#   app$wait_for_idle()
#
#   vals <- app$get_values()
#   expect_identical(vals$output$`ghqc_create_app-main_panel`$message, "No files selected")
#
#   app$expect_screenshot()
# })
#
# test_that("{shinytest2} recording: render_selected_list renders from treeInput and tree_paths.js", {
#   app <- AppDriver$new(
#     variant = platform_variant(), name = "file_selected", height = 727,
#     width = 609
#   )
#   app$set_inputs(`ghqc_create_app-assignees` = character(0), wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone_description` = "", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone` = "", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-tree_list` = character(0), wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone` = "qc id", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-tree_list` = c("📄 testthat.R"))
#   app$wait_for_idle()
#
#   vals <- app$get_values()
#   expect_true(grepl("testthat.R", vals$output$`ghqc_create_app-main_panel`$html))
#   app$expect_screenshot()
# })
#
#
# test_that("{shinytest2} recording: file_info button works", {
#   app <- AppDriver$new(
#     variant = platform_variant(), name = "file_info", height = 727,
#     width = 609
#   )
#   app$set_inputs(`ghqc_create_app-assignees` = character(0), wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone_description` = "", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-milestone` = "", wait_ = FALSE)
#   app$set_inputs(`ghqc_create_app-tree_list` = character(0), wait_ = FALSE)
#   app$click("ghqc_create_app-file_info")
#   app$set_inputs(`ghqc_create_app-checklist_info` = names(get_checklists()[1]), wait_ = FALSE)
#
#   app$wait_for_idle()
#
#   vals <- app$get_values()
#   dynamic_id <- names(vals$output)[grepl("^ghqc_create_app-out", names(vals$output))]
#   expect_true(length(dynamic_id) == 1)
# })
