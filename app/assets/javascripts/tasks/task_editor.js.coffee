# Setup the global namespace
window.Tasks or= {}

Tasks.Editor = (tables, operators) ->
  self = {}

  # ------------------------------------
  # Private members
  # ------------------------------------

  data = null
  view = null

  render = ->
    $("#prefetchTables").html(HandlebarsTemplates["tasks/prefetch_tables"](data))
    showHideAddTable()
    self

  initCodeEditor = ->
    CodeMirror.fromTextArea(document.getElementById("task_code"), {
      lineNumbers: true, mode: "lua", vimMode: true, matchBrackets: true, showCursorWhenSelecting: true
    })

  showHideAddTable = ->
    if (parseInt($("#newTableName").val()) > 0)
      $("#addTable").show()
    else
      $("#addTable").hide()

  handleEventAndCancel = (e, fun) ->
    e.stopPropagation()
    e.preventDefault()
    fun()
    false

  addTable = (e) ->
    handleEventAndCancel(e, ->
          data.newTableFilter(parseInt($("#newTableName").val()))
          render()
        )

  removeTable = (e) ->
    handleEventAndCancel(e, ->
          data.removeTableFilter(parseInt($(e.target).data("table-index")))
          render()
        )

  editFilter = (e) ->
    handleEventAndCancel(e, ->
          openFilterEditor(data.tableFilter(parseInt($(e.target).data("table-index"))))
          render()
        )

  openFilterEditor = (tableFilter) ->
    editor = new Tasks.FilterEditor
      tableFilter: tableFilter
      onSaved: render
    editor.render()

  submit = (e) ->
    $("#task_data").val(JSON.stringify(data))

  decodePrefetch = (tables) ->
    data = new Tasks.Data(tables)
    json = $("#task_data").val()
    return if json == ""
    data.fromJson(json)


  # ------------------------------------
  # Constructor
  # ------------------------------------

  view = new Backbone.View({
    el: "#taskEditor",
    render: render,
    events:
      "change #newTableName": showHideAddTable
      "click #addTable": addTable
      "click [data-remove-table]": removeTable
      "click [data-edit-filter]": editFilter
      "submit": submit
  })

  Tasks.Operators = operators
  decodePrefetch(tables)
  initCodeEditor()
  render()

  _.extend(self, {})