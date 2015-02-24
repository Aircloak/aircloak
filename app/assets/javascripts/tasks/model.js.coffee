# Setup the global namespace
window.Tasks or= {}

# Represents the data, as seen by UI and users. Data consists of list of tables
# each table having an optional filter.
Tasks.Data = (tables, initialClusterId, filterData, testData) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  tableFilters = []
  clusterId = null
  nextTestRunId = 1
  testRuns = []

  newTestRun = ->
    new TestRun(nextTestRunId++)

  findTestRun = (runId) ->
    _.find(testRuns, (testRun) -> testRun.id == runId)

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

  testRuns = [newTestRun()]

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

      _.each(testRuns, (testRun) -> testRun.addTableForTestUsers(selectedTable))

      tableFilter = new TableFilter(self, selectedTable, tableFilterDescriptor)
      tableFilters.push(tableFilter)
      tableFilter

    toJSON: -> tableFilters

    removeTableFilter: (index) ->
      removed = tableFilters.splice(index, 1)
      if removed[0]
        _.each(testRuns, (testRun) -> testRun.removeTestUsersForTable(removed[0].table().name))

    selectClusterId: (newClusterId) ->
      clusterId = newClusterId
      tableFilters = _.filter(tableFilters, (filter) -> (filter.table().cluster_id == clusterId))

    table: (id) ->
      _.find(tables, (table) -> table.id == parseInt(id))

    selectedTableForName: (name) ->
      _.find(selectedTables(), (table) -> table.name == name)

    simplifyPrefetch: ->
      _.each(tableFilters, (tableFilter) -> tableFilter.simplify())

    simplifyRuns: ->
      if testRuns.length == 0
        self.addTestRun()
      else
        testRuns = [testRuns[0]]

    testRuns: -> testRuns,

    addTestUser: (runId) ->
      findTestRun(runId).addTestUser(selectedTables())

    anotherUserEntry: (runId, userId, tableName) ->
      findTestRun(runId).addTestUser(self.selectedTableForName(tableName), userId)

    removeTestUser: (runId, userRowId) ->
      findTestRun(runId).removeTestUser(userRowId)

    findTestUser: (runId, userRowId) ->
      findTestRun(runId).findTestUser(userRowId)

    updateTestUser: (runId, tableName, userRowId, userData) ->
      findTestRun(runId).updateTestUser(tableName, userRowId, userData)

    addTestRun: ->
      lastRun = _.last(testRuns)
      newRun = newTestRun()
      testRuns.push(newRun)
      if (!lastRun)
        _.each(selectedTables(), (selectedTable) -> newRun.addTableForTestUsers(selectedTable))
      else
        newRun.import(lastRun.export())

    removeTestRun: (runId) ->
      testRuns = _.filter(testRuns, (testRun) -> testRun.id != runId)

    testJson: ->
      _.map(testRuns, (testRun) -> testRun.testJson())

    exportTestData: ->
      _.map(testRuns, (testRun) -> testRun.export())
  })

  self.selectClusterId(initialClusterId)

  if filterData && filterData != ""
    self.clear()
    _.each(JSON.parse(filterData), (tableFilterDescriptor) ->
          self.newTableFilter(tableFilterDescriptor.tableId, tableFilterDescriptor, true)
        )

  if testData && testData != ""
    testRuns = []
    _.each(JSON.parse(testData), (testRun) -> testRuns.push(newTestRun().import(testRun)))

  self


# Represents the data for a single test run
TestRun = (id) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  testUsers = []
  userRowId = 1

  newUserRowId = -> userRowId++

  newTestUserId = ->
      nextId = 1 + _.reduce(
            testUsers,
            (memo, testUser) ->
              currentSuffix = parseInt(testUser.user_id.replace("user_", ""))
              Math.max(memo, currentSuffix)
            0
          )
      "user_#{nextId}"

  addTestUser = (tables, userId) ->
    userId ||= newTestUserId()
    _.each(_.flatten([tables]),
          (table) ->
            testUser = sampleTestUser(table, userId)
            testUsers.push(_.extend({userRowId: newUserRowId()}, testUser))
        )

  sampleTestUser = (table, userId) ->
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
            {table: table.name, user_id: userId}
          )


  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    id: id

    export: ->
      testUsers: testUsers
      userRowId: userRowId

    import: (from) ->
      testUsers = _.map(from.testUsers, (testUser) -> _.clone(testUser))
      userRowId = from.userRowId
      self

    addTestUser: addTestUser

    addTableForTestUsers: (table) ->
      if testUsers.length > 0
        uniqueUsers = {}
        _.each(testUsers, (testUser) -> uniqueUsers[testUser.user_id] = true)
        _.each(
              _.keys(uniqueUsers),
              (userId) -> addTestUser(table, userId)
            )
      else
        addTestUser(table)

    removeTestUser: (userRowId) ->
      testUsers = _.filter(testUsers, (testUser) -> testUser.userRowId != userRowId)

    removeTestUsersForTable: (tableName) ->
      testUsers = _.filter(testUsers, (testUser) -> testUser.table != tableName)

    updateTestUser: (tableName, userRowId, userData) ->
      testUsers = _.map(
            testUsers,
            (testUser) ->
              if testUser.table == tableName && testUser.userRowId == userRowId
                _.extend(testUser, userData)
              else
                testUser
          )

    testUsers: ->
      _.map(
            _.sortBy(testUsers, (user) -> "#{user.user_id}_#{user.table}"),
            (user) ->
              _.extend({fields: JSON.stringify(_.omit(user, "table", "user_id", "userRowId"))}, user)
          )

    findTestUser: (userRowId) ->
      _.chain(testUsers).
        find((testUser) -> testUser.userRowId == userRowId).
        omit("table", "user_id", "userRowId").
        value()

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
TableFilter = (task, inTable, tableFilterDescriptor) ->
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

    simplify: ->
      filter = new Filter()
      userRows = null
      timeLimit = null

    supportExtendedFilters: ->
      task.supportExtendedFilters
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