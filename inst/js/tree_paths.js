$(document).ready(function() {
  console.log("Document is ready. Waiting for tree_list input.");

  function findPaths(ns_prefix) {
    var paths = [];
    console.log("Finding paths...");

    function traverse(node, currentPath) {
      var $node = $(node);
      var label = $node.children(".treejs-label").text();

      if ($node.hasClass("treejs-placeholder treejs-node__checked")) {
        var fullPath = currentPath.concat(label).join("/");
        paths.push(fullPath);
      } else {
        currentPath.push(label);
        $node.children("ul").children("li").each(function() {
          traverse(this, currentPath.slice());
        });
      }
    }

    $("div[id$='-tree_list'] li.treejs-placeholder.treejs-node__checked").each(function() {
      var currentPath = [];
      var $node = $(this);
      $node.parents("li").each(function() {
        var label = $(this).children(".treejs-label").text();
        currentPath.unshift(label);
      });
      traverse(this, currentPath);
    });

    console.log("Paths found: ", paths);
    if (typeof Shiny !== 'undefined') {
      Shiny.setInputValue(ns_prefix + "-paths", paths);
    }
  }

  Shiny.addCustomMessageHandler("process_tree_list", function(message) {
    console.log("Namespace from server:", message.ns);
    console.log("Received tree_list input from Shiny. Processing...");
    findPaths(message.ns);
  })
});
