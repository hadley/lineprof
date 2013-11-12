// view-source:http://mleibman.github.io/SlickGrid/examples/example2-formatters.html

html_formatter = function(row, cell, value, columnDef, dataContext) {
  return value;
};

escape_html = function(text) {
  return (text + "").replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");
}

escape_string = function(text) {
  return (text + "").replace(/"/g,'\\"');
}
var cols = [
  {id: "lineno", name: "#", field: "lineno", width: 20, cssClass: "lineno"},
  {id: "src", name: "Source code", field: "src", width: 420, cssClass: "code"},
  {id: "time", name: "t", field: "time", width: 60, toolTip: "Time (s)"},
  {
    id: "release", name: "r", field: "release", width: 30, cssClass: "negative",
    toolTip: "Memory released (MB)"
  },
  {
    id: "alloc", name: "a", field: "alloc", width: 30, 
    toolTip: "Memory allocated (MB)"
  },
  {id: "dups", name: "d", field: "dups", width: 30, toolTip: "Duplications"}
];

var slickGridOpts = {
  editable: false,
  enableAddRow: false,
  enableColumnReorder: false,
  enableCellNavigation: true,
  defaultFormatter: html_formatter,
  syncColumnCellResize: true,
  fullWidthRows: true,
};

Shiny.addCustomMessageHandler("formatTable",
  function(data) {

    // Quickly hack around rjson bug
    if (typeof data.alloc == "number") {
      data.alloc = [data.alloc]
      data.time = [data.time]
      data.release = [data.release]
      data.dups = [data.dups]
      data.src = [data.src]
      data.ref = [data.ref]
    }

    max_time = Math.max.apply(null, data.time);
    max_alloc = Math.max.apply(null, data.alloc);
    max_release = Math.max.apply(null, data.release);
    max_dups = Math.max.apply(null, data.dups);
    
    var rows = [];
    for (var i = 0; i < data.alloc.length; i++) {
      var d = (rows[i] = {});
  
      if (data.ref[i]) {
        var link = "<a href = '#' " + 
          "onClick = 'return navigate(\"" + escape_string(data.ref[i]) + "\")'>" +
          escape_html(data.src[i]) + "</a>";
        d["src"] = link;
      } else {
        d["src"] = escape_html(data.src[i]);
      }
      
      d["lineno"] = i + 1;
      d["time"] = percent_bar(data.time[i], max_time, " s");
      d["release"] = percent_bar(data.release[i], max_release, " MB");
      d["alloc"] = percent_bar(data.alloc[i], max_alloc, " MB");
      d["dups"] = percent_bar(data.dups[i], max_dups, "");
    }
    
    grid = new Slick.Grid("#profile", rows, cols, slickGridOpts);
  }
);

var percent_bar = function(val, max, suffix) {
  if (val == 0) return "";

  return "<span class='bar' " + 
    "style = 'width:" + (val / max) * 100 + "%' " + 
    "title = '" + val + suffix + "'></span>";
};

// https://github.com/jcheng5/leaflet-shiny/blob/master/inst/www/binding.js#L34
var navigate = function(ref) {
  Shiny.onInputChange("navigate", ref);
  return false;
};
