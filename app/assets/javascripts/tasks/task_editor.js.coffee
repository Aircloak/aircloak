# Setup the global namespace
window.Tasks or= {}

Tasks.Editor = (taskExceptions, completions, tables, operators) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  data = null
  view = null
  codeEditor = null
  changed = false

  render = ->
    data.selectClusterId(selectedClusterId())
    $("#prefetchTables").html(HandlebarsTemplates["tasks/prefetch_tables"](data))
    renderSandboxEditor()
    renderExceptions()
    renderSaveInfo()
    showHideAddTable()
    self

  renderExceptions = ->
    html =
      if taskExceptions.length == 0
        ""
      else
        HandlebarsTemplates["tasks/task_exceptions"](count: taskExceptions.length, exceptions: taskExceptions)
    $("#taskExceptions").html(html)

  saveInfo = ->
    if changed
      class: "alert-info",
      message: "You didn't test the task since last changes were done. The task may not work properly."
    else if taskExceptions.length > 0
      class: "alert-error",
      message: "Last run produced some errors. The task can be saved, but it will not work properly."
    else
      class: "hidden",
      message: ""

  renderSaveInfo = ->
    si = saveInfo()
    $("#taskStatus").
          removeClass("hidden alert-error alert-info").
          addClass(si.class).
          html(si.message)

  selectedClusterId = ->
    parseInt($('#task_cluster_id').val())

  initCodeEditor = ->
    CodeMirror.commands.autocomplete = (cm) ->
      # Initializes auto-completion
      cm.showHint(
            hint:
              (editor, options) ->
                # Custom hint function. It determines the word at the cursor, and
                # then returns all choices from the selection provided during by
                # the rails controller.

                regex = /(\w|\.)/
                cur = editor.getCursor()
                curLine = editor.getLine(cur.line)
                start = cur.ch
                end = start

                # find start and end of the word
                while (end < curLine.length && regex.test(curLine.charAt(end)))
                  end++
                while (start > 0 && regex.test(curLine.charAt(start - 1)))
                  start--

                curWord = curLine.slice(start, end)
                list = _.filter(completions, (candidate) -> candidate.text.indexOf(curWord) == 0)

                return {
                  list: _.sortBy(list, "text"),
                  from: CodeMirror.Pos(cur.line, start),
                  to: CodeMirror.Pos(cur.line, end)
                }
          )

    CodeMirror.commands.runInSandbox = doRunInSandbox

    CodeMirror.commands.save = (cm) ->
      unless saveInfo().message.length == 0 || window.confirm("#{saveInfo().message}\n\nSave anyway?")
        return
      $("#task_code").closest('form').submit()

    codeEditor = CodeMirror.fromTextArea(document.getElementById("task_code"), {
      lineNumbers: true, mode: "lua", vimMode: false, matchBrackets: true, showCursorWhenSelecting: true,
      extraKeys: {
        "Ctrl-Space": "autocomplete",
        "Ctrl-R": "runInSandbox"
        "Ctrl-S": "save"
      }
    })

    codeEditor.on("change", () ->
          if !changed
            changed = true
            renderSaveInfo()
        )

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
    handleEventAndCancel(e, doRunInSandbox)

  doRunInSandbox = () ->
    codeEditor.save()
    response = $.ajax(
      type: "POST",
      url: "/sandbox/run",
      async: false,
      processData: false,
      dataType: "json",
      contentType: "application/json; charset=utf-8",
      data: JSON.stringify({
            task_spec:
              users_data: data.testJson(),
              code: $("#task_code").val()
          })
    )
    changed = false
    reportSandboxErrors(response)
    renderSaveInfo()
    renderResults(response)

  reportSandboxErrors = (response) ->
    taskExceptions =
      if (response.status != 200)
        [{message: "HTTP error #{response.status}", count: 1}]
      else
        (JSON.parse(response.responseText).errors || [])
    renderExceptions()

  renderResults = (response) ->
    results = []
    if (response.status == 200)
      _.each(
            _.pairs(JSON.parse(response.responseText).reported_properties),
            ([user, properties]) ->
              _.each(
                    _.pairs(properties),
                    ([label, value]) ->
                      results.push(user: user, bucket: "#{label}: #{value}")
                  )
          )
    results = _.sortBy(results, (result) -> "#{result.user}_#{result.bucket}")
    $("#sandboxResult").html(HandlebarsTemplates["tasks/sandbox_results"](results))

  removeTestUser = (e) ->
    handleEventAndCancel(e, ->
          data.removeTestUser($(e.target).data("user-id"))
          renderSandboxEditor()
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
      "click [data-remove-test-user]": removeTestUser
      "submit": submit
  })

  Tasks.Operators = operators
  decodePrefetch(tables)
  initCodeEditor()
  render()

  _.extend(self, {})