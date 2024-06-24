import { adjust_grid } from './adjust_grid.js';

$(document).ready(function() {
  $('button[id$="-toggle_sidebar"]').on('click', function() {
    var sidebar = $('div[id$="-sidebar"]');
    sidebar.toggleClass('collapsed');
    // Change the icon based on the sidebar state
    var icon = $(this).find('i'); // find icon
    if (sidebar.hasClass('collapsed')) {
      icon.removeClass('fa-angle-double-left').addClass('fa-angle-double-right');
    } else {
      icon.removeClass('fa-angle-double-right').addClass('fa-angle-double-left');
    }
    // call adjust_grid after toggle
    setTimeout(adjust_grid, 100)
  });
});
