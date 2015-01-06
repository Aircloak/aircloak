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

      # On some browsers (specifically on the mac) String.fromCharCode
      # only accepts a limited number of arguments. For longer payloads
      # the conversions fails and the result is never delivered to the
      # callback, unless we manually break the payload up into parts
      # not exceeding the allowed length.
      # See https://bugs.webkit.org/show_bug.cgi?id=80797
      maxSize = 65537
      rawData = new Uint8Array(event.data)
      accumulatedContent = ""
      startSubArray = 0
      while startSubArray < rawData.length
        endSubArray = Math.min(startSubArray + maxSize, rawData.length)
        subArray = rawData.subarray startSubArray, endSubArray # the range is [a,b)
        startSubArray = endSubArray
        accumulatedContent += String.fromCharCode.apply(null, subArray) # convert to string
      object.content = accumulatedContent
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
