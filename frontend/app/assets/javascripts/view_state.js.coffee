# This class can be used to manipulate view state from some controller. The state
# here represents a key-value mapping with keys and values being only strings.
# The reference point of the state are URL query params. However, for
# performance reasons, the class keeps a JSON hash internally.
# Furthermore, some basic services for moving params to/from controls are
# provided.
#
# See metrics controller for example usage
window.ViewState = (controlIds) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  params = {}

  initControls = ->
    currentUrlParams = urlParams()
    _.each(
          controlIds,
          (paramName) =>
            setControlValue($('#' + paramName), currentUrlParams[paramName]) if currentUrlParams[paramName]
        )

  urlParams = ->
    return {} if !window.location.search
    _.reduce(
          window.location.search.replace(/^\?/, "").split("&"),
          ((memo, param) ->
            [name, value] = param.split("=")
            memo[name] = decodeURIComponent(value.replace(/\+/g, ' '))
            memo),
          {}
        )

  controlParams = ->
    _.reduce(
          controlIds,
          (memo, controlId) ->
            control = $("#" + controlId)
            memo[controlId] = controlValue(control) if (control[0])
            memo
          {}
        )

  recalcParams = -> params = allParams()

  allParams = () ->
    _.extend(urlParams(), controlParams())

  store = -> history.replaceState(null, "", "?" + $.param(params))

  setControlValue = (control, value) ->
    if control.prop('type') == 'checkbox'
      control.prop('checked', value == "true")
    else
      control.val(value)

  controlValue = (control) ->
    if control.prop('type') == 'checkbox'
      control.prop('checked')
    else
      control.val()


  # ------------------------------------
  # Constructor
  # ------------------------------------

  initControls()
  recalcParams()

  _.extend(self, {
    # Combines url and control params. If some param is defined in both, the
    # control one takes precedence.
    allParams: allParams

    # Returns the value for some param
    param: (name) -> params[name]

    # Returns the value for some url param
    urlParam: (name) -> urlParams()[name]

    # Recalculates params and stores them.
    recalcAndStore: () ->
      recalcParams()
      store()

    # Merges new params
    merge: (newParams) ->
      recalcParams()
      for key, value of newParams
        params[key] = value
      store()
  })