window.airpub_listen = (server, request, callback) ->
  unless "WebSocket" of window
    alert "This browser does not support WebSockets"
    return null

  console.log "Connecting to " + server
  ws = new WebSocket(server)

  ws.onopen = () ->
    console.log 'Connected. Sending request: ' + request
    callback
      type : "event"
      event_type : "connected"
    ws.binaryType = "arraybuffer"
    ws.send request

  ws.onmessage = (event) ->
    if typeof event.data is "string"
      # parse object header
      @object = {}
      tokens = event.data.split(" ")
      @object.type = tokens[0]
      for token in tokens
        attribute = token.split('=')
        @object[attribute[0]] = attribute[1]
    else
      # add content and invoke callback
      object = @object
      delete @object
      object.content = String.fromCharCode.apply(null, new Uint8Array(event.data)) # convert to string
      callback object

  ws.onclose = (event) ->
    console.log 'Connection closed!'
    callback
      type : "event"
      event_type : "closed"

  ws.onerror = (event) ->
    console.log "Connection error: " + event.reason
    callback
      type : "event"
      event_type : "error"

  return ws
