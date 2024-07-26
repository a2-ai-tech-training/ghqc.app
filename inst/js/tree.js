$(document).ready(function () {
  var Trees = {};
  var Ids = {};
  var Children = null;
  var childrenFlag = true;

  Shiny.addCustomMessageHandler("getChildren", function (x) {
    console.log("Received getChildren message:", x);
    Children = x;
    childrenFlag = true;
  });

  Shiny.addCustomMessageHandler("noChildrenFound", function (x) {
    console.log("childrenFlag set to false.");
    childrenFlag = false;
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
    console.log("Leaf node clicked with id:", id, "Node:", node);


    if (tree.is_leaf(node) && node.original.type === "folder") {
      var path = tree.get_path(node, "/");
      console.log("Fetching children for path:", path);

      Shiny.setInputValue(ns + "-path_from_js", path);

      // Introduce a delay to check if the flag changes
      var checkInterval = setInterval(function () {
        if (Children !== null || !childrenFlag) {
          clearInterval(checkInterval);
          if (!childrenFlag) {
            console.log("Children flag was set to false, ignoring action");
            return;
          }

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
      }, 100); // Check every 100 milliseconds
    }
  });
});
