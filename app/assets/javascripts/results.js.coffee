# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

# holds existing results table columns and maps them to indices
window.columns = {}
columns.count = 1


# creates a column if doesn't exists already
create_column = (name) ->
  if not columns[name]?
    columns[name] = columns.count
    columns.count++
    header = document.getElementById('results_table').tHead
    column = document.createElement('th')
    column.innerHTML = name
    header.rows[0].appendChild(column)


# makes sure all columns needed for showing the result are created
create_columns = (result) ->
  for bucket in result.buckets
    create_column bucket.name


format_date = (timestamp) ->
  # this will be shown in local time as "YYYY-MM-DD hh:mm:ss"
  date = new Date(timestamp).toISOString()
  date.substring(0, date.length - 5).replace('T', ' ')


# adds a row to the results table representing the specified result
display_result = (result) ->
  table = document.getElementById('results_table')
  row = table.insertRow()

  date = row.insertCell(0)
  timestamp = parseInt(result.published_at)
  date.innerHTML = format_date(timestamp)
  if timestamp < task_last_update
    date.innerHTML += "*"

  errors = row.insertCell(1)
  if result.exceptions.length > 0
    window.e = errors
    errors.innerHTML = "present"
    errors.style.color = "red"
  else
    errors.innerHTML = "none"

  create_columns(result)

  cells = []
  for index in [1..columns.count]
    cells.push row.insertCell(index+1)

  for bucket in result.buckets
    index = columns[bucket.name]
    cells[index-1].innerHTML = bucket.value


$ ->
  window.task_last_update = $('.render_params').data('task-last-update')
  results = $('.render_params').data('results')
  for result in results
    display_result result
