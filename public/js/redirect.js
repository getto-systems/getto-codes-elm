GettoDetect().from_current_version(
  "0.0.0",
  function(path) {
    location.href = path;
  },
  function() {
    location.href = "/dist/index.html";
  }
);
