"use strict";

(function(){
  var appendReload = function(element){
    var reload = document.createElement("script");
    reload.src = "/reload/reload.js";
    element.appendChild(reload);
  };

  appendReload(document.body);
})();
