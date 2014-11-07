# Setup the global namespace
window.Tasks or= {}

# Represents the data, as seen by UI and users. Data consists of list of tables
# each table having an optional filter.
Tasks.Data = (tables) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  tableFilters = []
  clusterId = null
  testUsers = []

  selectedTablesMap = ->
    _.reduce(
          tableFilters,
          (memo, filter) ->
            memo[filter.table().id] = true
            memo
          {}
        )

  selectedTables = ->
    result = _.filter(
          tables,
          (table) -> selectedTablesMap()[table.id] && table.cluster_id == clusterId
        )
    _.sortBy(result, "name")

  availableTables = ->
    result = _.filter(
          tables,
          (table) -> !selectedTablesMap()[table.id] && table.cluster_id == clusterId
        )
    _.sortBy(result, "name")


  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    clear: -> tableFilters = []

    availableTables: -> availableTables()
    selectedTables: -> selectedTables()
    tableFilter: (id) -> tableFilters[id]
    tableFilters: -> tableFilters

    newTableFilter: (tableId, tableFilterDescriptor, noThrow) ->
      selectedTable = _.find(tables, (table) -> table.id == tableId)
      if !selectedTable || selectedTable.cluster_id != clusterId
        throw(new Error("invalid table")) unless noThrow
        return
      tableFilter = new TableFilter(selectedTable, tableFilterDescriptor)
      tableFilters.push(tableFilter)
      tableFilter

    toJSON: -> tableFilters

    fromJson: (json) ->
      self.clear()
      _.each(JSON.parse(json), (tableFilterDescriptor) ->
            self.newTableFilter(tableFilterDescriptor.tableId, tableFilterDescriptor, true)
          )

    removeTableFilter: (index) ->
      tableFilters.splice(index, 1)

    selectClusterId: (newClusterId) ->
      clusterId = newClusterId
      tableFilters = _.filter(tableFilters, (filter) -> (filter.table().cluster_id == clusterId))

    table: (id) ->
      _.find(tables, (table) -> table.id == parseInt(id))

    addTestUser: (testUser) -> testUsers.push(testUser)
    removeTestUser: (userId) ->
      testUsers = _.filter(testUsers, (testUser) -> testUser.user_id != userId)

    sampleTestUser: (table) ->
      nextId = 1 + _.reduce(
            testUsers,
            (memo, testUser) ->
              currentSuffix = parseInt(testUser.user_id.replace("user_", ""))
              Math.max(memo, currentSuffix)
            0
          )

      _.reduce(
            table.columns,
            (memo, column) ->
              val =
                if column.type == "integer" || column.type == "bigint"
                  Math.floor(Math.random() * 100)
                else if column.type == "float" || column.type == "double"
                  Math.floor(Math.random() * 100) + 0.01 * Math.floor(Math.random() * 100)
                else if column.type == "boolean"
                  (Math.floor(Math.random() * 2) == 1).toString()
                else
                  "foobar"
              memo[column.name] = val
              memo
            {user_id: "user_#{nextId}"}
          )

    testUsers: ->
      _.map(testUsers, (testUser) -> {data: testUser, text: JSON.stringify(testUser)})

    testJson: ->
      usersData = {}
      _.each(testUsers, (testUser) ->
            usersData[testUser.table] ||= {columns: [], data: {}}
            targetTable = usersData[testUser.table]
            userRow = []

            iterator = ([field, value]) ->
              return if field == "table" || field == "user_id"
              columnIndex = targetTable.columns.indexOf(field)
              if columnIndex == -1
                targetTable.columns.push(field)
                columnIndex = targetTable.columns.indexOf(field)
              userRow[columnIndex] = value

            _.each(_.pairs(testUser), iterator)

            targetTable.data[testUser.user_id] ||= []
            targetTable.data[testUser.user_id].push(userRow)
          )
      usersData
  })


# Represents a single selected table, and associated filter.
TableFilter = (inTable, tableFilterDescriptor) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  table = inTable
  filter = new Filter()
  userRows = null
  timeLimit = null

  # ------------------------------------
  # Constructor
  # ------------------------------------

  if tableFilterDescriptor
    filter = Filter.fromRawGroups(tableFilterDescriptor.filter.groups)
    userRows = tableFilterDescriptor.user_rows
    timeLimit = tableFilterDescriptor.time_limit

  _.extend(self, {
    toJSON: ->
      filter.compact()
      {tableId: table.id, user_rows: userRows, time_limit: timeLimit, filter: filter}

    filterString: ->
      res = []
      res.push(table.name)
      res.push("last #{self.minLimit()} min") if self.minLimit()
      res.push("max #{userRows}") if userRows
      res.push("(#{filter.string()})") unless filter.empty()
      res.join(", ")

    table: -> table

    column: (name) ->
      _.find(self.table().columns, (column) -> column.name == name)

    userRows: ->
      userRows = arguments[0] if arguments.length == 1
      userRows

    minLimit: ->
      if arguments.length == 1
        timeLimit = arguments[0]
        timeLimit *= 60 if timeLimit
      timeLimit && Math.round(timeLimit / 60)

    filter: -> filter
    setFilter: (newFilter) -> filter = newFilter
  })


# Represents a filter for the table. A filter can have multiple groups, each
# group representing a set of conditions (see below). When the final query is
# built, filters represented by each group will be OR-joined.
Filter = (inGroups) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------
  groups = inGroups || []


  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    clear: ->
      groups = []

    newGroup: ->
      newGroup = new Group
      groups.push(newGroup)
      newGroup

    addFilterToLastGroup: (filter) ->
      (_.last(groups) || self.newGroup()).addFilter(filter)

    removeFilterFromLastGroup: (index) ->
      return if groups.length == 0
      _.last(groups).removeFilter(index)

    string: ->
      maybeJoin(_.map(groups, (group) -> group.string()), "or", true)

    empty: ->
      self.compact()
      groups.length == 0

    clone: ->
      new Filter(_.map(groups, (group) -> group.clone()))

    compact: ->
      groups = _.filter(groups, (group) -> group.filters().length > 0)

    groups: -> groups

    group: (index) -> groups[index]

    toJSON: () ->
      groups: groups
  })

Filter.fromRawGroups = (groups) ->
  filter = new Filter
  _.each(groups, (group) ->
        filter.newGroup()
        _.each(group.filters, filter.addFilterToLastGroup)
      )
  filter


# Represents a group inside a filter. A group has multiple conditions,
# with condition consisting of a column name, operator, and value.
# When target query is generated, all conditions will be AND-joined.
Group = (inFilters) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  filters = inFilters || []


  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    filters: -> filters

    addFilter: (newFilter) ->
      filters.push(newFilter)

    removeFilter: (index) ->
      filters.splice(index, 1)

    string: ->
      maybeJoin(_.map(filters, (filter) ->
            "#{filter.column}#{filter.operator}#{filter.value}"
          ), "and")

    clone: ->
      new Group(filters.map((filter) -> _.clone(filter)))

    toJSON: () ->
      filters: filters
  })


# ------------------------------------
# Internal helpers
# ------------------------------------

maybeJoin = (list, separator, parens) ->
  result = list.join(" #{separator} ")
  result = "(#{result})" if parens && list.length > 1
  result