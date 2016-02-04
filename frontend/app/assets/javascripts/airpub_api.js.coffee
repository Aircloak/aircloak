//= require pako_inflate.min

window.airpub_listen = (requestPath, callback) ->
  unless "WebSocket" of window
    alert "This browser does not support WebSockets"
    return null

  ws = null
  request = null
  connect = null

  # Code from: http://snipplr.com/view/31206/
  readUTF8String = (bytes) ->
    ix = 0
    ix = 3 if bytes.subarray(0,3) == "\xEF\xBB\xBF"
    string = ""
    while ix < bytes.length
      byte1 = bytes[ix]
      if byte1 < 0x80
        string += String.fromCharCode byte1
      else if byte1 >= 0xC2 and byte1 < 0xE0
        byte2 = bytes[++ix]
        string += String.fromCharCode(((byte1&0x1F)<<6) + (byte2&0x3F))
      else if  byte1 >= 0xE0 and byte1 < 0xF0
        byte2 = bytes[++ix]
        byte3 = bytes[++ix]
        string += String.fromCharCode(((byte1&0xFF)<<12) + ((byte2&0x3F)<<6) + (byte3&0x3F))
      else if  byte1 >= 0xF0 && byte1 < 0xF5
        byte2 = bytes[++ix]
        byte3 = bytes[++ix]
        byte4 = bytes[++ix]
        codepoint = ((byte1&0x07)<<18) + ((byte2&0x3F)<<12) + ((byte3&0x3F)<<6) + (byte4&0x3F)
        codepoint -= 0x10000;
        string += String.fromCharCode(
          (codepoint>>10) + 0xD800,
          (codepoint&0x3FF) + 0xDC00
        )
      else
        throw "Invalid UTF8 character in received data at byte #{ix}"
      ix++
    string


  onSocketOpen = () ->
    console.log 'Connected. Sending request: ' + request
    callback
      type : "event"
      event_type : "connected"
    ws.binaryType = "arraybuffer"
    ws.send request

  onSocketMessage = (event) ->
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
      content = new Uint8Array(event.data)
      if object.content_encoding == "gzip"
        # we have a compressed object, decompress before invoking callback
        content = pako.ungzip(content)
        object.content_encoding = "identity"
      object.content = readUTF8String(content)
      callback object

  onSocketClose = (event) ->
    console.log 'Connection closed!'
    callback
      type : "event"
      event_type : "closed"
    # Reconnect on close
    setTimeout(connect, 1000)

  onSocketError = (event) ->
    console.log "Connection error: " + event.reason
    callback
      type : "event"
      event_type : "error"

  connect = () ->
    response = $.ajax(
      type: "POST",
      url: "/airpub/request_parameters",
      data: {path: requestPath},
      error: () -> setTimeout(connect, 1000)
      success: (connectData) ->
        console.log "Connecting to " + connectData.server
        request = connectData.request

        ws = new WebSocket(connectData.server)
        ws.onopen = onSocketOpen
        ws.onclose = onSocketClose
        ws.onmessage = onSocketMessage
        ws.onerror = onSocketError
    )

  connect()
