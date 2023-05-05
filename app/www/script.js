// This recieves messages of type "testmessage" from the server.
// See http://shiny.rstudio.com/gallery/server-to-client-custom-messages.html
// for details
Shiny.addCustomMessageHandler("userMessage",
  function(message) {
    alert(message);
  }
);

Shiny.addCustomMessageHandler("jsCode",
  function(message) {
    console.log(message)
    eval(message.code);
  }
);
