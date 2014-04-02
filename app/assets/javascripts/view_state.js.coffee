class ViewState
  constructor: (@controlIds) ->
    @initControls()
    @recalcParams()

  initControls: () ->
    urlParams = @urlParams()
    _.each(
          @controlIds,
          (paramName) =>
            $('#' + paramName).val(urlParams[paramName]) if urlParams[paramName]
        )

  urlParams: () ->
    return {} if !window.location.search
    _.reduce(
          window.location.search.replace(/^\?/, "").split("&"),
          ((memo, param) ->
            [name, value] = param.split("=")
            memo[name] = decodeURIComponent(value.replace(/\+/g, ' '))
            memo),
          {}
        )

  controlParams: () ->
    _.reduce(
          @controlIds,
          (memo, controlId) ->
            control = $("#" + controlId)
            memo[controlId] = control.val() if (control[0])
            memo
          {}
        )

  allParams: () ->
    _.extend(@urlParams(), @controlParams())

  recalcParams: () -> @params = @allParams()

  store: () ->
    @recalcParams()
    history.replaceState(null, "", "?" + $.param(@params))

  param: (name) -> @params[name]

window.ViewState = ViewState