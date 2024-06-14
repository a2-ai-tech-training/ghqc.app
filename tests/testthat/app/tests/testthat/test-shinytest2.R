devtools::load_all()
library(shinytest2)
library(ghqc)
# expectations have to be specified as expect_values() will have additional input when trees are opened and test files have been generated

test_that("{shinytest2} recording: get_files_tree_list renderTree renders", {
  app <- AppDriver$new(variant = platform_variant(), name = "get_files_tree_list renderTree renders", height = 728, width = 609)
  app$set_inputs(`ghqc_app-assignees` = character(0), wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone_description` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "", wait_ = FALSE)
  app$wait_for_idle()

  vals <- app$get_values()
  expect_identical(vals$output$`ghqc_app-main_panel`$message, "No files selected")

  app$expect_screenshot()
})

test_that("{shinytest2} recording: get_files_tree_list renderTree nesting renders", {
  app <- AppDriver$new(variant = platform_variant(),name = "get_files_tree_list renderTree nesting renders", height = 728, width = 609)
  app$set_inputs(`ghqc_app-assignees` = character(0), wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone_description` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "qc id", wait_ = FALSE)
  app$get_js("var tree = $('#ghqc_app-tree_list').jstree(true);
             tree.open_all();
                 $('#ghqc_app-tree_list').find('li').each(function() {
      var node = $('#ghqc_app-tree_list').jstree(true).get_node(this.id);
      if (node.text === 'testthat') {
        $('#ghqc_app-tree_list').jstree('close_node', this.id);
      }
    });
  ")
  app$wait_for_idle()

  vals <- app$get_values()
  expect_identical(vals$output$`ghqc_app-main_panel`$message, "No files selected")

  app$expect_screenshot()
})


test_that("{shinytest2} recording: render_selected_list renders from get_selected_items", {
  app <- AppDriver$new(variant = platform_variant(), name = "render_selected_list renders from get_selected_items", height = 728, width = 609)
  app$set_inputs(`ghqc_app-assignees` = character(0), wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone_description` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "qc id", wait_ = FALSE)
  app$get_js("var tree = $('#ghqc_app-tree_list').jstree(true);
    $('#ghqc_app-tree_list').find('li').each(function() {
      var node = $('#ghqc_app-tree_list').jstree(true).get_node(this.id);
      if (node.text === 'app.R') {
        $('#ghqc_app-tree_list').jstree('select_node', this.id);
      }
    });
  ")
  app$wait_for_idle()

  vals <- app$get_values()
  expect_true(grepl("app.R", vals$output$`ghqc_app-main_panel`$html))
  expect_identical(attr(vals$input$`ghqc_app-tree_list`$app.R, "stselected"), TRUE)

  app$expect_screenshot()
})



test_that("{shinytest2} recording: file info button renders get_checklists items", {
  app <- AppDriver$new(variant = platform_variant(), name = "file info button renders get_checklists items", height = 969, width = 609)
  app$set_inputs(`ghqc_app-assignees` = character(0), wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone_description` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "", wait_ = FALSE)
  app$set_inputs(`ghqc_app-milestone` = "qc id", wait_ = FALSE)
  app$click("ghqc_app-file_info")
  app$set_inputs(`ghqc_app-checklist_info` = names(get_checklists()[1]), wait_ = FALSE)
  app$wait_for_idle()

  vals <- app$get_values()
  dynamic_id <- names(vals$output)[grepl("^ghqc_app-out", names(vals$output))]
  expect_true(length(dynamic_id) == 1)

  app$expect_screenshot()
})
