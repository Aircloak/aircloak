# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

$ ->
  # adds a message to the event console element
  log = (message) ->
    time = (new Date()).toLocaleTimeString()
    text = document.createTextNode(time + ":> " + message)
    event_console = document.getElementById('event_console')
    event_console.appendChild(text)
    br = document.createElement("br")
    event_console.appendChild(br)

  # callback for processing listen events
  airpub_callback = (object) ->
    if (object.type == "event")
      log "New event: " + object.event_type
    else if (object.type == "article")
      published_at = new Date(parseInt(object.published_at))
      log "Info: path=" + object.path + " content_type=" + object.content_type +
          " content_encoding=" + object.content_encoding + " published_at=" + published_at.toLocaleString()
      log "Content: '" + object.content + "'"

  window.ws = airpub_listen $('.params').data('server-url'), $('.params').data('request'), airpub_callback
