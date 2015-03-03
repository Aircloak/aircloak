# Implements a period picker control.
#
# To use the control, just add the data-period-picker attribute to the text input
# element:
#
#   <input data-period-picker="true" ...>
#
# When the page is loaded, PeriodPicker will find all such elements, and place
# it's UI in their place. The original input control still exists on the page,
# (but it is hidden), and can be accessed as usual (e.g. by id).
# PeriodPicker ensures that each change in its UI controls is propagated as JSON
# to the original input element. Thus, you can simply take the value of the
# original input element and do something with it.
window.PeriodPicker = (inputControl) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  inputControl = $(inputControl)
  picker = $(HandlebarsTemplates["controls/period_picker"]())

  uiElement = (name) -> picker.find("[name=#{name}]")

  periodContainers = ->
    every: "interval_container"
    daily: "hour"
    weekly: "wday"
    monthly: "mday"

  periodControls = ->
    every: "interval"
    daily: "hour"
    weekly: "wday"
    monthly: "mday"

  selectedPeriod = ->
    uiElement("period").val()

  selectedPeriodContainer = ->
    uiElement(periodContainers()[selectedPeriod()])

  selectedPeriodControl = ->
    uiElement(periodControls()[selectedPeriod()])

  showHideControls = ->
    _.each(_.values(periodContainers()), (name) -> uiElement(name).hide())
    selectedPeriodContainer().show()

  deserialize = ->
    uiElement("period").val("")
    return if inputControl.val() == ""
    [[period, info]] = _.pairs(JSON.parse(inputControl.val()))
    uiElement("period").val(period)
    selectedPeriodControl().val(info)

  serialize = ->
    value =
      if selectedPeriod() == ""
        ""
      else
        json = {}
        json[selectedPeriod()] = selectedPeriodControl().val()
        JSON.stringify(json)
    inputControl.val(value)


  # ------------------------------------
  # Constructor
  # ------------------------------------

  picker.
    insertAfter(inputControl).
    append(inputControl)

  inputControl.
    hide().
    data("periodPickerController", self)

  deserialize()
  showHideControls()

  uiElement("period").change(showHideControls)

  _.each(
        _.values(periodControls()).concat("period"),
        (controlName) -> uiElement(controlName).change(serialize)
      )


  # ------------------------------------
  # API
  # ------------------------------------

  _.extend(self, {})

window.PeriodPicker.decorate = (inputControl) ->
  return if ($(inputControl).data("periodPickerController"))
  new PeriodPicker(inputControl)

window.PeriodPicker.decorateAll = ->
  _.each($("[data-period-picker]"), window.PeriodPicker.decorate)

$(document).ready ->
  PeriodPicker.decorateAll()