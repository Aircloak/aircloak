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

  # Custom hint function. It determines the word at the cursor, and
  # returns the completion list.
  completionList = (cm, editor, options) ->
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

    list =
      _.chain([]).
        union(
              completions,
              _.map(data.selectedTables(), (table) -> text: "tables.#{table.name}"),
              _.map(CodeMirror.hint.anyword(cm, word: /[a-zA-Z_](\w)*/).list, (word) -> text: word)
            ).
        filter((candidate) -> candidate.text.indexOf(curWord) == 0).
        uniq((el) -> el.text).
        sortBy((el) -> el.text.toUpperCase()).
        value()

    return {
      list: list,
      from: CodeMirror.Pos(cur.line, start),
      to: CodeMirror.Pos(cur.line, end)
    }

  initCodeEditor = ->
    CodeMirror.commands.autocomplete = (cm) ->
      cm.showHint(hint: _.bind(completionList, null, cm))

    CodeMirror.commands.runInSandbox = doRunInSandbox

    CodeMirror.commands.save = (cm) ->
      unless saveInfo().message.length == 0 || window.confirm("#{saveInfo().message}\n\nSave anyway?")
        return
      $("#task_code").closest('form').submit()

    codeEditor = CodeMirror.fromTextArea(document.getElementById("task_code"), {
      lineNumbers: true, mode: "lua", vimMode: false, matchBrackets: true, showCursorWhenSelecting: true, viewportMargin: Infinity
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

  addSandboxUser = (e) ->
    handleEventAndCancel(e,
          ->
            userId = data.newTestUserId()
            _.each(
                  data.selectedTables(),
                  (table) ->
                    testUser = data.sampleTestUser(table, userId)
                    data.addTestUser(testUser)
                )
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
    renderSandboxResults(response)

  reportSandboxErrors = (response) ->
    taskExceptions =
      if (response.status != 200)
        [{message: "HTTP error #{response.status}", count: 1}]
      else
        (JSON.parse(response.responseText).errors || [])
    renderExceptions()

  renderSandboxResults = (response) ->
    if (response.status == 200)
      results =
        _.chain(JSON.parse(response.responseText).reported_properties).
          pairs().
          map(([user, properties]) -> _.map(properties, (property) -> user: user, property: property)).
          flatten(true).
          sortBy((result) -> "#{result.user}_#{result.property.label}_#{result.property.value}").
          value()
    $("#sandboxResult").html(HandlebarsTemplates["tasks/sandbox_results"](results || []))

  anotherUserEntry = (e) ->
    handleEventAndCancel(e,
          ->
            userId = $(e.target).data("user-id")
            testUser = data.sampleTestUser(data.selectedTableForName($(e.target).data("table")), userId)
            data.addTestUser(testUser)
            renderSandboxEditor()
        )

  removeTestUser = (e) ->
    handleEventAndCancel(e, ->
          data.removeTestUser($(e.target).data("userRowId"))
          renderSandboxEditor()
        )

  editSandboxUser = (e) ->
    handleEventAndCancel(e, ->
          target = $(e.target)
          fields =
            _.chain(data.findTestUser(target.data("user-row-id"))).
              pairs().
              reduce(
                    (memo, [field, value]) -> memo.push(name: field, value: value.toString()); memo
                    []
                  )
              .value()
          showSandboxUserEditor(target.data("table"), target.data("user-row-id"), fields)
        )

  showSandboxUserEditor = (tableName, userRowId, fields) ->
    Popup.show(HandlebarsTemplates["tasks/sandbox_user"](fields: fields))
    $("[data-sandbox-field]").focus()
    $('#updateSandboxUser').on(
          'click',
          (e) -> handleEventAndCancel(e, -> updateSandboxUser(tableName, userRowId))
        )

  updateSandboxUser = (tableName, userRowId) ->
    userData =
      _.reduce(
            $("[data-sandbox-field]"),
            (memo, control) ->
              control = $(control)
              columnName = control.data("sandbox-field")
              table = data.selectedTableForName(tableName)
              columnType = _.find(table.columns, (c) -> c.name == columnName).type

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
            {}
          )
    $('#updateSandboxUser').off('click')
    Popup.close()
    data.updateTestUser(tableName, userRowId, userData)
    renderSandboxEditor()


  # ------------------------------------
  # Constructor
  # ------------------------------------

  view = new Backbone.View({
    el: "#taskEditor",
    render: render,
    events:
      "change #task_cluster_id": render
      "change #newTableName": showHideAddTable
      "click #addTable": addTable
      "click #addSandboxUser": addSandboxUser
      "click #runInSandbox": runInSandbox
      "click [data-remove-table]": removeTable
      "click [data-edit-filter]": editFilter
      "click [data-remove-test-user]": removeTestUser
      "click [data-another-user-entry]": anotherUserEntry
      "click [data-edit-sandbox-user]": editSandboxUser
      "submit": submit
  })

  Tasks.Operators = operators
  decodePrefetch(tables)
  initCodeEditor()
  render()

  _.extend(self, {})
