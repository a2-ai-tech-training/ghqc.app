export function adjust_grid() {
  // Function to get the maximum width of the first column across all grid-items
  function getMaxWidth() {
    const rows = document.querySelectorAll('.grid-items');
    let maxWidth = 0;

    console.log('Adjusting grid layout'); // Log to check if the function is executed

    rows.forEach((row, index) => {
      const firstColumn = row.querySelector('.item-a');
      if (firstColumn) {
        const width = firstColumn.scrollWidth;
        if (width > maxWidth) {
          maxWidth = width;
        }
      } else {
        console.warn(`First column not found in row ${index}`);
      }
    });

    console.log(`Max width: ${maxWidth}px`); // Log the maximum width found
    return maxWidth;
  }

  // Function to apply the maximum width to all grid-items with a constraint of 2fr
  function applyMaxWidth(maxWidth) {
    if (maxWidth === 0) {
      console.warn("Max width is 0px, no adjustments made.");
      return;
    }

    const container = document.querySelector('.grid-container-depth-0');
    const containerWidth = container ? container.clientWidth : 0;

    console.log(`Container width: ${containerWidth}px`);

    const sidebarCollapsed = document.querySelector("div[id$='-sidebar']").classList.contains('collapsed');
    const maxAllowableWidth = sidebarCollapsed ? containerWidth * 2 / 5 : containerWidth * 2 / 5;


    if (maxWidth > maxAllowableWidth) {
      maxWidth = Math.max(maxWidth, 100)
    } else {
      maxWidth = maxAllowableWidth;

    }

    console.log(`Adjusted max width (constrained to '2fr'}): ${maxWidth}px`);

    const rows = document.querySelectorAll('.grid-items');
    rows.forEach((row, index) => {
      if (row) {
        row.style.transition = 'all 0.5s ease';
        row.style.gridTemplateColumns = `minmax(100px, ${maxWidth}px) minmax(100px, 1fr) minmax(125px, 1fr)`;
      } else {
        console.warn(`Row ${index} is undefined`);
      }
    });
  }

  // Get the maximum width and apply it
  const maxWidth = getMaxWidth();
  applyMaxWidth(maxWidth);
}

// Debounce function to limit the rate at which adjust_grid is called
function debounce(func, wait) {
  let timeout;
  return function() {
    const context = this, args = arguments;
    clearTimeout(timeout);
    timeout = setTimeout(() => func.apply(context, args), wait);
  };
}

const debouncedAdjustGrid = debounce(adjust_grid, 200);

// Call debouncedAdjustGrid on window resize
window.addEventListener('resize', debouncedAdjustGrid);

// Expose the function to be called from Shiny
if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('adjust_grid', function(message) {
    $(document).on('shiny:idle', function() {
      setTimeout(adjust_grid, 100);
    });
  });
}
