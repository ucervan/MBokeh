(function() {
var fn = function() {
Bokeh.safely(function() {
  (function(root) {
    function embed_document(root) {
      var docs_json = `docs_json`;
      var render_items = [`render_items`];
  
      root.Bokeh.embed.embed_items(docs_json, render_items);
    }
  
    if (root.Bokeh !== undefined) {
      embed_document(root);
    } else {
      var attempts = 0;
      var timer = setInterval(function(root) {
        if (root.Bokeh !== undefined) {
          embed_document(root);
          clearInterval(timer);
        }
        attempts++;
        if (attempts > 100) {
          console.log("Bokeh: ERROR: Unable to embed document because BokehJS library is missing")
          clearInterval(timer);
        }
      }, 10, root)
    }
  })(window);
});
};
if (document.readyState != "loading") fn();
else document.addEventListener("DOMContentLoaded", fn);
})();

