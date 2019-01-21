"use strict";

(function(config){
  var current_path = config.path;

  var redirectToNextVersion = function(version){
    GettoDetect({
      version_to_path: function(version){
        return "/" + version + "/" + current_path;
      }
    }).from_current_version(version,function(path) {
      location.href = path + location.search;
    });
  };

  var version = document.getElementById("version")
    .innerText
    .replace("version : ","");

  redirectToNextVersion(version);
})(config);
