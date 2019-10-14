(function() {
  var i, text, code, codes = document.getElementsByTagName('code');
  for (i = 0; i < codes.length;) {
    code = codes[i];
    if (code.parentNode.tagName !== 'PRE' && code.childElementCount === 0) {
      text = code.textContent;
      if (/^\\\((.|\s)+\\\)$/.test(text) || /^\\\[(.|\s)+\\\]$/.test(text) ||
          /^\$\$(.|\s)+\$\$$/.test(text) ||
          /^\\begin\{([^}]+)\}(.|\s)+\\end\{[^}]+\}$/.test(text)) {
        code.outerHTML = code.innerHTML;  // Is this loading? remove <code></code>
        continue;
      }
    }
    i++;
  }
})();

(function () {
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src  = 'libs/MathJax/MathJax.js?config=TeX-AMS_HTML';
  script.async = true;
  //if (location.protocol !== 'file:' && /^https?:/.test(script.src))
    //script.src  = script.src.replace(/^https?:/, '');
  document.getElementsByTagName('head')[0].appendChild(script);
})();

