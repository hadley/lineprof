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
  {id: "src", name: "Source code", field: "src", width: 450, cssClass: "code"},
  {id: "time", name: "t", field: "time", width: 60},
  {id: "release", name: "r", field: "release", width: 30, cssClass: "negative"},
  {id: "alloc", name: "a", field: "alloc", width: 30},
  {id: "dups", name: "d", field: "dups", width: 30}
];

var slickGridOpts = {
  editable: false,
  enableAddRow: false,
  enableColumnReorder: false,
  enableCellNavigation: true,
  defaultFormatter: html_formatter,
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

    max_alloc = Math.max.apply(null, data.alloc);
    max_time = Math.max.apply(null, data.time);
    max_release = Math.max.apply(null, data.release);
    max_dups = Math.max.apply(null, data.dups);
    
    var rows = [];
    for (var i = 0; i < data.alloc.length; i++) {
      var d = (rows[i] = {});
  
      if (data.ref[i]) {
        var link = "<a href = '#' " + 
          "onClick = 'navigate(\"" + escape_string(data.ref[i]) + "\")'>" +
          escape_html(data.src[i]) + "</a>";
        d["src"] = link;
      } else {
        d["src"] = escape_html(data.src[i]);
      }
      
      d["time"] = percent_bar(data.time[i], max_time);
      d["release"] = percent_bar(data.release[i], max_release);
      d["alloc"] = percent_bar(data.alloc[i], max_alloc);
      d["dups"] = percent_bar(data.dups[i], max_dups);
    }
    
    grid = new Slick.Grid("#profile", rows, cols, slickGridOpts);
  }
);

var percent_bar = function(val, max, spanClass) {
  return "<span class='bar' " + 
    "style = 'width:" + (val / max) * 100 + "%' " + 
    "title = '" + val + "'></span>";
};

// https://github.com/jcheng5/leaflet-shiny/blob/master/inst/www/binding.js#L34
var navigate = function(ref) {
  Shiny.onInputChange("navigate", ref);  
  return false;
};