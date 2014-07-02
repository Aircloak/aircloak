# Setup the global namespace
window.Tasks or= {}

# Represents the data, as seen by UI and users. Data consists of list of tables
# each table having an optional filter.
Tasks.Data = (tables) ->
  self = {}

  # ------------------------------------
  # Private members
  # ------------------------------------

  allTables = tables
  availableTables = null
  tableFilters = null

  clear = ->
    availableTables = allTables
    sortAvailableTables()
    tableFilters = []

  sortAvailableTables = ->
    availableTables = _.sortBy(availableTables, "name")


  # ------------------------------------
  # Constructor
  # ------------------------------------

  clear()

  _.extend(self, {
    clear: clear,

    availableTables: -> availableTables
    tableFilter: (id) -> tableFilters[id]
    tableFilters: -> tableFilters

    newTableFilter: (tableId, filter) ->
      [[selectedTable], availableTables] = _.partition(
            availableTables,
            (table) -> table.id == tableId
          )
      throw(new Error("invalid table")) if !selectedTable
      tableFilter = new TableFilter(selectedTable, filter)
      tableFilters.push(tableFilter)
      tableFilter

    toJSON: -> tableFilters

    fromJson: (json) ->
      clear()
      _.each(JSON.parse(json), (rawFilter) ->
            self.newTableFilter(rawFilter.tableId, Filter.fromRawGroups(rawFilter.filter.groups))
          )

    removeTableFilter: (index) ->
      oldTable = tableFilters.splice(index, 1)
      availableTables.push(oldTable[0].table())
      sortAvailableTables()
  })


# Represents a single selected table, and associated filter.
TableFilter = (inTable, inFilter) ->
  self = {}

  # ------------------------------------
  # Private members
  # ------------------------------------

  table = inTable
  filter = inFilter || new Filter

  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    toJSON: ->
      filter.compact()
      {tableId: table.id, filter: filter}

    filterString: ->
      res = table.name
      res += " (#{filter.string()})" unless filter.empty()
      res

    table: -> table
    filter: -> filter
    setFilter: (newFilter) -> filter = newFilter
  })


# Represents a filter for the table. A filter can have multiple groups, each
# group representing a set of conditions (see below). When the final query is
# built, filters represented by each group will be OR-joined.
Filter = (inGroups) ->
  self = {}

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
  self = {}

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