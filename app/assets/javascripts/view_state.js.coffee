# This class can be used to manipulate view state from some controller. The state
# here represents a key-value mapping with keys and values being only strings.
# The reference point of the state are URL query params. However, for
# performance reasons, the class keeps a JSON hash internally.
# Furthermore, some basic services for moving params to/from controls are
# provided.
#
# See metrics controller for example usage
class ViewState
  ## -------------------------------------------------------------------
  ## Public instance methods
  ## -------------------------------------------------------------------

  # Constructor receives a list of control ids (without #). These controls
  # are then synced with state params.
  constructor: (@controlIds) ->
    @initControls()
    @recalcParams()

  # Combines url and control params. If some param is defined in both, the
  # control one takes precedence.
  allParams: () ->
    _.extend(@urlParams(), @controlParams())

   # Returns the value for some param
  param: (name) -> @params[name]

  # Recalculates params and stores them.
  recalcAndStore: () ->
    @recalcParams()
    @store()

  # Merges new params
  merge: (newParams) ->
    @recalcParams()
    for key, value of newParams
      @params[key] = value
    @store()


  ## -------------------------------------------------------------------
  ## Internal public instance methods
  ## -------------------------------------------------------------------

  initControls: () ->
    urlParams = @urlParams()
    _.each(
          @controlIds,
          (paramName) =>
            setControlValue($('#' + paramName), urlParams[paramName]) if urlParams[paramName]
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
            memo[controlId] = controlValue(control) if (control[0])
            memo
          {}
        )

  recalcParams: () -> @params = @allParams()

  store: () -> history.replaceState(null, "", "?" + $.param(@params))

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

# Makes class globally accessible
window.ViewState = ViewState