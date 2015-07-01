# include buckets aggregation functionality
//= require tasks/results_aggregation

# create namespace for results-related shared variables
window.Results = window.Results or {}

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
  result.buckets = _.sortBy(result.buckets, name_from_bucket)
  result.buckets = Results.aggregate_quantized_buckets result.buckets
  if result.buckets.length > 100
    result.buckets = [{label: "notice", value: "result too big", \
          count: "buckets count limit exceeded, use REST API or CSV export to view result"}]

  table = document.getElementById 'results_table'

  if Results.resultsTableLimit?
    if Results.resultsTableLimit < table.rows.length
      table.deleteRow table.rows.length - 1

  create_columns result

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
    title = ""
    for exception in result.exceptions
     # look for sandbox error messages
      sandboxErrorMatch = exception.error.match /^{sandbox_error,"(.*)"}$/
      if sandboxErrorMatch
        error = sandboxErrorMatch[1]
        # look for task code error messages
        lineErrorMatch = error.match /^\[string \\"task_code\\"\]:(\d*)(.*)/
        if lineErrorMatch # if we have a match
          # cleanup error string
          error = "error in task code at line " + lineErrorMatch[1] + lineErrorMatch[2]
      else
        error = exception.error # we don't know the format of this type of error message
      title += "#{exception.count} occurances of \"#{error}\"\n"
    errors.title = title.trim()
  else
    errors.innerHTML = "none"

  cells = _.map [1..Results.columns.count], (index) -> row.insertCell index + 1

  for bucket in result.buckets
    index = Results.columns[name_from_bucket bucket]
    cells[index-1].innerHTML = bucket.count


$ ->
  Results.task_last_update = $('.render_params').data('task-last-update')
  oldResults = $('.render_params').data('results')
  for result in oldResults
    Results.display result
