(function() {
  var initClipboard, newCopyButton, wrapInDiv;

  initClipboard = function() {
    var preElements = document.getElementsByTagName('pre');
    for (var i = 0; i < preElements.length; i++) {
      preElements[i].appendChild(newCopyButton());
      // need to wrap the element inside a div so we can add some styling
      wrapInDiv(preElements[i]);
    }

    var clipboard = new Clipboard('.copy-button', {target: function(trigger) {return trigger.previousElementSibling;}});
    clipboard.on('success', function(event) {
      event.clearSelection();
      event.trigger.textContent = 'Copied';
      window.setTimeout(function() {
        event.trigger.textContent = 'Copy';
      }, 1000);
    });
  }

  newCopyButton = function() {
    var button = document.createElement('button');
    button.className = 'copy-button';
    button.textContent = 'Copy';
    return button;
  }

  wrapInDiv = function(el) {
    var wrapper = document.createElement('div');
    wrapper.className = "codeBlock";

    if (el.nextSibling) {
      el.parentNode.insertBefore(wrapper, el.nextSibling);
    } else {
      el.parentNode.appendChild(wrapper);
    }
    return wrapper.appendChild(el);
  };

  document.onreadystatechange = function() {
    if (document.readyState === 'complete') {
      initClipboard();
    }
  };
})();
