# include result exception handling functionality
//= require tasks/result_exception

# create namespace for results-related shared variables
window.Results or= {}

# the time when last article was published
Results.last_article_update = 0
# holds existing results table columns and maps them to indices
Results.columns = {}
Results.columns.count = 0
Results.columns.start = 0


# creates a column if doesn't exists already
create_column = (name) ->
  if not Results.columns[name]?
    Results.columns[name] = Results.columns.count
    Results.columns.count++
    cell = $('<th/>').html name
    cb = () ->
        $(@).append cell
        cell = $('<td/>')
    $('#results_table tr').each cb


name_from_bucket = (bucket) ->
  _.compact([bucket.label, bucket.value]).join(": ")


# makes sure all columns needed for showing the result are created
create_columns = (result) ->
  for bucket in result.buckets
    create_column (name_from_bucket bucket)


format_date = (timestamp) ->
  # this will be shown in local time as "YYYY-MM-DD hh:mm:ss"
  date = new Date(timestamp).toISOString()
  date.substring(0, date.length - 5).replace('T', ' ')


# adds a row to the results table representing the specified result
Results.display = (result) ->
  # call page new result callback if any registered
  Results.new_result_callback(result) if Results.new_result_callback

  if result.buckets.length > 25
    notice = "buckets count (#{result.buckets.length}) exceeds column limit (25), use the " +
        if result.buckets.length <= 100
          "single result view for details"
        else
          "REST API or CSV export for details"
    result.buckets = [{label: "notice", value: "too many buckets", count: notice}]

  table = document.getElementById 'results_table'

  if Results.resultsTableLimit?
    if Results.resultsTableLimit < table.rows.length
      table.deleteRow table.rows.length - 1

  create_columns result

  # remember when the last article was published
  Results.last_article_update = result.published_at

  row = table.insertRow 1

  time = row.insertCell 0
  timestamp = parseInt result.published_at
  time.innerHTML = format_date timestamp
  if timestamp < Results.task_last_update
    time.innerHTML += "*"

  errors = row.insertCell 1
  if result.exceptions.length > 0
    errors.innerHTML = "present"
    errors.classList.add "error-text"
    error_texts = _.map result.exceptions, (exception) ->
        ResultException.format_error(ResultException.parse_to_error exception)
    errors.title = error_texts.join("\n")
  else
    errors.innerHTML = "none"

  if Results.columns.start >= 2
    details = row.insertCell 2
    details.innerHTML = "<a href='#{result.details_url}'>view</a>"

  cells = _.map [0...Results.columns.count], (index) -> row.insertCell index + Results.columns.start + 1

  for bucket in result.buckets
    index = Results.columns[name_from_bucket bucket]
    cells[index].innerHTML = bucket.count


$ ->
  Results.task_last_update = $('.render_params').data('task-last-update')
  Results.columns.start = $('#results_table th').length - 1
  oldResults = $('.render_params').data('results')
  for result in oldResults
    Results.display result
  # table does not show properly if we don't create an initial row, which we now delete here
  $('#results_table tr:last').remove()
