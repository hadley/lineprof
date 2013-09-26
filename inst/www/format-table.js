Shiny.addCustomMessageHandler("formatTable",
  function(message) {
    alert(JSON.stringify(message));
  }
);