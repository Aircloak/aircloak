# Setup the global namespace
window.Tasks or= {}

Tasks.Editor = (tables, operators) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  data = null
  view = null
  codeEditor = null

  render = ->
    data.selectClusterId(selectedClusterId())
    $("#prefetchTables").html(HandlebarsTemplates["tasks/prefetch_tables"](data))
    renderSandboxEditor()
    showHideAddTable()
    self

  selectedClusterId = ->
    parseInt($('#task_cluster_id').val())

  initCodeEditor = ->
    codeEditor = CodeMirror.fromTextArea(document.getElementById("task_code"), {
      lineNumbers: true, mode: "lua", vimMode: false, matchBrackets: true, showCursorWhenSelecting: true
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
    data.selectClusterId(selectedClusterId())
    json = $("#task_data").val()
    return if json == ""
    data.fromJson(json)

  renderSandboxEditor = () ->
    $("#sandboxRunner").html(HandlebarsTemplates["tasks/sandbox_runner"](data))

  renderUserControls = () ->
    table = data.table($("#sandboxUserTable").val())
    if table
      $("#userEntryControls").html(HandlebarsTemplates["tasks/sandbox_user"](table))
      sampleData = data.sampleTestUser(table)
      _.each($("[data-sandbox-field]"),
            (el) ->
              el = $(el)
              el.val(sampleData[el.data("sandbox-field")] || "")
          )
    else
      $("#userEntryControls").html("")

  addSandboxUser = (e) ->
    handleEventAndCancel(e, ->
      table = data.table($("#sandboxUserTable").val())
      testUser = _.reduce($("[data-sandbox-field]"),
            (memo, control) ->
              control = $(control)

              columnName = control.data("sandbox-field")
              columnType =
                if columnName == "user_id"
                  "string"
                else
                  _.find(table.columns, (c) -> c.name == columnName).type

              memo[columnName] =
                if columnType == "integer" || columnType == "bigint"
                  parseInt(control.val())
                else if columnType == "float" || columnType == "double"
                  parseFloat(control.val())
                else if columnType == "boolean"
                  control.val().toLowerCase() == "true"
                else
                  control.val()

              memo
            {table: table.name}
          )
      data.addTestUser(testUser)
      renderSandboxEditor()
    )

  runInSandbox = (e) ->
    handleEventAndCancel(e, ->
          codeEditor.save()
          response = $.ajax(
            type: "POST",
            url: "/sandbox/run",
            async: false,
            processData: false,
            dataType: "json",
            contentType: "application/json; charset=utf-8",
            data: JSON.stringify({
                  payload: JSON.stringify({
                        users_data: data.testJson(),
                        code: $("#task_code").val()
                      })
                })
          )
          $("#sandboxResult").html("HTTP #{response.status}\n#{response.responseText}")
        )


  # ------------------------------------
  # Constructor
  # ------------------------------------

  view = new Backbone.View({
    el: "#taskEditor",
    render: render,
    events:
      "change #task_cluster_id": render
      "change #newTableName": showHideAddTable
      "change #sandboxUserTable": renderUserControls
      "click #addTable": addTable
      "click #addSandboxUser": addSandboxUser
      "click #runInSandbox": runInSandbox
      "click [data-remove-table]": removeTable
      "click [data-edit-filter]": editFilter
      "submit": submit
  })

  Tasks.Operators = operators
  decodePrefetch(tables)
  initCodeEditor()
  render()

  _.extend(self, {})