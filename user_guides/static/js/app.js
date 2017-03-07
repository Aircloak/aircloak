function initClipboard() {
  var preElements = document.getElementsByTagName('pre');
  for (var i = 0; i < preElements.length; i++) {
    preElements[i].appendChild(newCopyButton());
  }

  new Clipboard('.copy-button', {
    target: function(trigger) {
      return trigger.previousElementSibling;
    }
  }).
    on('success', function(event) {
      event.clearSelection();
      event.trigger.textContent = 'Copied';
      window.setTimeout(function() {
        event.trigger.textContent = 'Copy';
      }, 1000);
    });
}

function newCopyButton() {
  var button = document.createElement('button');
  button.className = 'copy-button';
  button.textContent = 'Copy';
  return button;
}

document.onreadystatechange = function() {
  if (document.readyState === 'complete') {
    initClipboard()
  }
};
