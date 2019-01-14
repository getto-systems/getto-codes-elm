GettoDetect().from_current_version(
  "0.2.3",
  function(path) {
    location.href = path;
  },
  function() {
    location.href = "/dist/index.html";
  }
);
