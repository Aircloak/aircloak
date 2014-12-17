# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

# create namespace for results-related shared variables
window.Results = {}
# the time when last article was published
Results.last_article_update = 0
# holds existing results table columns and maps them to indices
Results.columns = {}
Results.columns.count = 1

# creates a column if doesn't exists already
create_column = (name) ->
  if not Results.columns[name]?
    Results.columns[name] = Results.columns.count
    Results.columns.count++
    column = $('<th/>').html name
    $('#results_table thead tr:first').append column


# makes sure all columns needed for showing the result are created
create_columns = (result) ->
  for bucket in result.buckets
    create_column bucket.name


format_date = (timestamp) ->
  # this will be shown in local time as "YYYY-MM-DD hh:mm:ss"
  date = new Date(timestamp).toISOString()
  date.substring(0, date.length - 5).replace('T', ' ')


# adds a row to the results table representing the specified result
Results.display = (result) ->
  table = document.getElementById 'results_table'

  if Results.resultsTableLimit?
    if Results.resultsTableLimit < table.rows.length
      table.deleteRow table.rows.length - 1

  row = table.insertRow 1
  date = row.insertCell 0

  # remember when the last article was published
  Results.last_article_update = result.published_at

  timestamp = parseInt result.published_at
  date.innerHTML = format_date timestamp
  if timestamp < Results.task_last_update
    date.innerHTML += "*"

  errors = row.insertCell 1
  if result.exceptions.length > 0
    errors.innerHTML = "present"
    errors.style.color = "red"
  else
    errors.innerHTML = "none"

  create_columns result
  cells = _.map [1..Results.columns.count], (index) -> row.insertCell index + 1

  for bucket in result.buckets
    index = Results.columns[bucket.name]
    cells[index-1].innerHTML = bucket.value


$ ->
  Results.task_last_update = $('.render_params').data('task-last-update')
  oldResults = $('.render_params').data('results')
  for result in oldResults
    Results.display result
