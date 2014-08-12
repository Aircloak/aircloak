//= require backbone


## ------------------------------------------------------------------
## Models
## ------------------------------------------------------------------

Column = Backbone.Model.extend
  initialize: ->
    @_isDeleted = false
    @_wasJustCreated = false

  # We cannot use the built in `isNew/0` function as it
  # is conditional on if the object has a server provided
  # id or not, which never applies here
  wasJustCreated: (created) ->
    if arguments.length == 1
      @_wasJustCreated = created
      @trigger "change"
    @_wasJustCreated

  isDeleted: -> @_isDeleted

  toggleDeleted: ->
    @_isDeleted = not @_isDeleted
    @trigger "change"

  sync: -> false


ColumnList = Backbone.Collection.extend
  model: Column

  setup: (params) ->
    @whetherCreation = params.is_creation
    @reset params.raw_data unless params.raw_data.length == 0
    @setPreviousMigration params.raw_previous_migration

  isCreation: -> @whetherCreation

  # Returns a list of columns that have been added
  # since the beginning of this session.
  # If we are creating a new table, then all new columns are returned.
  # If we are altering an existing table, columns that have been
  # added in this session are returned.
  addedColumns: ->
    _.filter @models, (column) -> column.wasJustCreated()

  # Returns a list of column names that have
  # been removed which should be dropped in
  # the migration that is being made.
  # If we are creating a new table, then dropped columns names
  # will always be an empty list as none of the columns
  # have yet been created in the cloak.
  droppedColumnNames: (columnsNow) ->
    diff = _.filter @models, (column) -> column.isDeleted()
    _.map(diff, (c) -> c.get 'name')

  # Returns all columns in the table, excluding those that have
  # been marked for removal. If the table is saved at this point,
  # then the columns returned by this method are the ones that
  # exist in the table.
  allLiveColumnsAsJSON: ->
    liveColumns = _.filter @models, (column) -> not column.isDeleted()
    _.map liveColumns, (column) -> column.toJSON()

  hasColumnWithName: (name) ->
    _.find @models, (column) -> column.get('name') == name

  # When a creation or migration fails,
  # we are redisplaying the form with all the columns
  # the user created intact.
  # At this point the columns have lost the information
  # about if the user intended to remove or keep them.
  # This we can reconstruct from the previous migration,
  # making columns previously marked for removal, removed,
  # and new columns rendered as new columns
  setPreviousMigration: (migration) ->
    @markColumnsAsNew(migration.columns) if migration.columns
    @markColumnsAsNew(migration.add_columns) if migration.add_columns
    @markColumnsAsDeleted(migration.drop_columns) if migration.drop_columns

  markColumnsAsNew: (newColumns) ->
    _.each @models, (column) ->
      unless _.findWhere(newColumns, name: column.get "name") == undefined
        column.wasJustCreated(true)

  markColumnsAsDeleted: (droppedColumnNames) ->
    _.each @models, (column) ->
      if _.contains(droppedColumnNames, column.get "name")
        column.trigger "destroy"


## ------------------------------------------------------------------
## Views
## ------------------------------------------------------------------

ColumnView = Backbone.View.extend
  tagName: "tr"
  template: HandlebarsTemplates['migrations/column']

  events: "click .button-remove": "remove"

  remove: ->
    if Columns.isCreation() || @model.wasJustCreated()
      Columns.remove @model
      @el.remove()
    else
      @$el.toggleClass 'error' # Warn user about pending column drop
      @model.toggleDeleted()
      @render()
      @trigger 'pendingDelete'

  initialize: ->
    @listenTo @model, 'change', @render
    @listenTo @model, 'destroy', @remove

  render: ->
    columnData = @model.toJSON()
    columnData.isDeleted = @model.isDeleted()
    @$el.html (@template columnData)
    @$el.addClass 'success' if @model.wasJustCreated()
    @


MigrationView = Backbone.View.extend
  el: 'div'

  initialize: ->
    @inputTableName = $ "input#table_name"
    @inputColumnName = $ "input#column_name"
    @selectType = $ "select#type"
    @inputSize = $ "input#size"
    @inputSpan = $ "span#size-span"
    @submitButton = $ ".submit-btn"
    @columnsView = $ "tbody#columns"

    @listenTo Columns, 'add', @addOne
    @listenTo Columns, 'reset', @addAll
    @listenTo Columns, 'all', @render
    @listenTo Columns, "add remove reset", @modelChanged

  events: ->
    "keypress #newColumns": "createOnReturn"
    "click .addRow": "createNewColumn"
    "change #type": "typeSelected"
    "submit #dummy-form": "dummyFormSubmit"
    "submit #table-form": "mainFormSubmit"

    # We need the table name to be reflected in the
    # generated migrations, and therefore need to listen
    # to all sort of possible events on the input
    "onchange input#table_name": "modelChanged"
    "change input#table_name": "modelChanged"
    "keyup input#table_name": "modelChanged"
    "keypress input#table_name": "modelChanged"
    "paste input#table_name": "modelChanged"
    "blur input#table_name": "modelChanged"

  typeSelected: ->
    val = @selectType.val()
    if val == "varchar"
      @inputSpan.removeClass "hidden"
      @inputSize.focus()
    else
      @inputSpan.addClass "hidden"
      @inputSize.val ""

  createOnReturn: (e) ->
    return unless e.keyCode == 13
    @createNewColumn()

  createNewColumn: ->
    name = @inputColumnName.val()
    return if name == ""
    checkboxNotNull = $("#not_null")[0]

    if @model.hasColumnWithName name
      alert "A column name '#{name}' already exists"
    else
      constraints = []
      constraints.push "NOT NULL" if checkboxNotNull.checked
      type = @selectType.val()
      type = "#{type}(#{@inputSize.val()})" if type == "varchar"

      column = new Column
        name: name
        constraints: constraints
        type: type
      column.wasJustCreated(true)
      Columns.add column

    # Reset input fields so the next column can be added
    @inputColumnName.val ""
    @inputSize.val ""
    checkboxNotNull.checked = false
    @inputColumnName.focus()

  addOne: (column) ->
    view = new ColumnView model: column
    @listenTo view, 'pendingDelete', @modelChanged
    @columnsView.append view.render().el

  addAll: ->
    Columns.each(this.addOne, this);

  render: ->
    @

  # Whenever the value of the table changes,
  # we create a valid migration for the pending changes
  # that can later be submitted with the form.
  # Likewise we also generate a representation of
  # all the columns present in the table.
  # This representation can later be used elsewhere
  # in the application, for example in the data
  # query selection interface.
  modelChanged: ->
    @updateSubmitButton()
    return unless @formHasValidChanges()
    $("#table_json").val(JSON.stringify Columns.allLiveColumnsAsJSON())
    migration =
      table_name: @inputTableName.val()
    if @model.isCreation()
      migration.action = "create"
      migration.columns = @model.addedColumns()
    else
      migration.action = "alter"
      addedColumns = @model.addedColumns()
      migration.add_columns = addedColumns if addedColumns.length > 0
      droppedColumnNames = @model.droppedColumnNames()
      migration.drop_columns = droppedColumnNames if droppedColumnNames.length > 0
    $("#migration").val(JSON.stringify migration)

  formHasValidChanges: ->
    @inputTableName.val() != "" and
        (@model.addedColumns().length != 0 or @model.droppedColumnNames().length != 0)

  updateSubmitButton: ->
    if @formHasValidChanges()
      @submitButton.removeClass 'disabled'
    else
      @submitButton.addClass 'disabled'

  # We want to prevent the main form from submitting unless there are
  # changes to submit.
  mainFormSubmit: ->
    @formHasValidChanges()

  # In order to get proper styling for the form elements
  # used to add columns, we have them placed inside a form element.
  # This form should never be submitted, so we always cancel submissions.
  dummyFormSubmit: -> false


## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

Columns = new ColumnList
window.columns = Columns

new MigrationView
  model: Columns
