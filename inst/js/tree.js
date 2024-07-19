$(document).ready(function () {
  var Trees = {};
  var Ids = {};
  var Children = null;
  var lastState = {}; // To store the last state before getChildren

  Shiny.addCustomMessageHandler("getChildren", function (x) {
    Children = x;
  });

  // need to revert to previous state because tree state thinks unselectable node is selected/open
  Shiny.addCustomMessageHandler("noChildrenFound", function (x) {
    console.log("No selectable files found. Reverting changes.");
    // Revert to the last stored state
    for (var id in lastState) {
      if (lastState.hasOwnProperty(id)) {
        Trees[id].settings.core.data = lastState[id];
        Trees[id].refresh();
      }
    }
    lastState = {}; // Clear the last state
  });

  var $navigator = $("div[id$='treeNavigator___']");

  console.time("Initial tree rendering time");

  $navigator.on("ready.jstree", function (e, data) {
    console.timeEnd("Initial tree rendering time");
    var id = e.target.id;
    Ids[id] = id.split("-")[0];
    Trees[id] = data.instance;
    var tree = Trees[id];
    var li_id = $("#" + id + ">ul>li").attr("id");
    tree.disable_checkbox(li_id);
    tree.disable_node(li_id);
  });

  $navigator.on("after_open.jstree", function (e, data) {
    var tree = Trees[e.target.id];
    tree.enable_checkbox(data.node);
    tree.enable_node(data.node);
  });

  $navigator.on("after_close.jstree", function (e, data) {
    var tree = Trees[e.target.id];
    tree.disable_checkbox(data.node);
    tree.disable_node(data.node);
  });

  $navigator.on("click", "li.jstree-x > i", function (e) {
    var $li = $(this).parent();
    if (!$li.hasClass("jstree-x")) {
      alert("That should not happen...");
      return;
    }
    var div_id = $li.closest("div").attr("id");
    var tree = Trees[div_id];
    var ns = Ids[div_id];
    var id = $li.attr("id");
    var node = tree.get_node(id);
    if (tree.is_leaf(node) && node.original.type === "folder") {
      var path = tree.get_path(node, "/");

      // Store the current state before getChildren
      lastState[div_id] = tree.settings.core.data;

      Shiny.setInputValue(ns + "-path_from_js", path);
      var interval = setInterval(function () {
        if (Children !== null) {
          clearInterval(interval);
          console.time("Node rendering time");
          console.log("Starting node rendering for:", Children.elem.length, "elements");
          for (var i = 0; i < Children.elem.length; i++) {
            var isdir = Children.folder[i];
            var newnode = tree.create_node(id, {
              text: Children.elem[i],
              type: isdir ? "folder" : "file",
              children: false,
              li_attr: isdir ? { class: "jstree-x" } : null
            });
            if (isdir) {
              tree.disable_checkbox(newnode);
              tree.disable_node(newnode);
            }
          }
          console.timeEnd("Node rendering time");
          Children = null;
          setTimeout(function () {
            tree.open_node(id);
          }, 10);
        }
      }, 100);
    }
  });
});
