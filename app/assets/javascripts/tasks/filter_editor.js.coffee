# Setup the global namespace
window.Tasks or= {}

Handlebars.registerPartial("tasks/filter", HandlebarsTemplates["tasks/filter"]);

Tasks.FilterEditor = (inOptions) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  options = inOptions
  minLimit = options.tableFilter.minLimit()
  minLimit = Math.round(minLimit / 60.0) if minLimit
  userRows = options.tableFilter.userRows()
  filter = options.tableFilter.filter().clone()
  view = null
  dirty = false

  render = ->
    Popup.show(HandlebarsTemplates["tasks/edit_filter"](
          table: options.tableFilter.table()
          minLimit: minLimit
          userRows: userRows
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
    options.tableFilter.minLimit(minLimit)
    options.tableFilter.userRows(userRows)
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
    # Ensure that values are either positive, or null
    userRows = Math.max(parseInt($("#userRows").val()), 0) || null
    minLimit = Math.max(parseInt($("#minLimit").val()), 0) || null

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
      "change #userRows": setDirty
      "change #minLimit": setDirty
  )

  _.extend(self, {
    render: render
  })