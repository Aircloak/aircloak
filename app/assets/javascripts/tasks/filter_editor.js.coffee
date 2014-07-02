# Setup the global namespace
window.Tasks or= {}

Handlebars.registerPartial("tasks/filter", HandlebarsTemplates["tasks/filter"]);

Tasks.FilterEditor = (inOptions) ->
  self = {}

  # ------------------------------------
  # Private members
  # ------------------------------------

  options = inOptions
  filter = options.tableFilter.filter().clone()
  view = null
  dirty = false

  render = ->
    Popup.show(HandlebarsTemplates["tasks/edit_filter"](
          table: options.tableFilter.table(),
          filter: filter,
          operators: _.keys(Tasks.Operators)
          newTemplate: {}
        ))
    view.setElement("#editFilter")
    filterToControls()
    $("#newFilter [data-field=column]").focus()
    self

  save = ->
    return unless dataValid()
    controlsToFilter()
    options.tableFilter.setFilter(filter)
    dirty = false
    closeWindow()
    options.onSaved()

  dataValid = ->
    errorElement = _.find($("[data-filter-input]"), (el) -> $(el).val() == "")
    if !errorElement
      true
    else
      errorElement = $(errorElement)
      errorElement.focus()
      $("#filterError").html("Please enter #{errorElement.data("field")}")
      false

  closeWindow = ->
    if !dirty || confirm("Abandon unsaved changes?")
      view.remove()
      Popup.close()

  filterToControls = ->
    return if filter.groups().length == 0
    _.each(filter.group(0).filters(), (filter, index) ->
          _.each(_.pairs(filter), ([key, value]) ->
                $("#subFilter#{index} [data-field=#{key}]").val(value)
              )
        )

  controlsToFilter = ->
    filter.clear()
    _.each(
          $("[data-filter-controls]"),
          (el) ->
            filter.addFilterToLastGroup(subFilterValues(el))
        )

  subFilterValues = (parent) ->
    parent = $(parent)
    _.reduce(["column", "operator", "value"],
          (memo, param) ->
            memo[param] = parent.find("[data-field=#{param}]").val()
            memo
          {}
        )

  addFilter = (e) ->
    handleEventAndCancel(e, ->
          setDirty()
          controlsToFilter()
          filter.addFilterToLastGroup({})
          render()
        )

  removeFilter = (e) ->
    handleEventAndCancel(e, ->
          setDirty()
          controlsToFilter()
          filter.removeFilterFromLastGroup(parseInt($(e.target).data("index")))
          render()
        )

  setDirty = ->
    dirty = true

  handleEventAndCancel = (e, fun) ->
    e.stopPropagation()
    e.preventDefault()
    fun()
    false


  # ------------------------------------
  # Constructor
  # ------------------------------------

  view = new Backbone.View(
    render: render

    events: ->
      "click #saveTaskFilter": save
      "click #closeTaskFilter": closeWindow
      "click #addFilter": addFilter
      "click [data-remove-filter]": removeFilter
      "change #filterControls [data-filter-input]": setDirty
  )

  _.extend(self, {
    render: render
  })