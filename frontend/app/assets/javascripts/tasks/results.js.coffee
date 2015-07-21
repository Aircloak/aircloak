# include buckets aggregation functionality
//= require tasks/results_aggregation

# create namespace for results-related shared variables
window.Results = window.Results or {}

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
  result.buckets = _.sortBy(result.buckets, name_from_bucket)
  result.buckets = Results.aggregate_quantized_buckets result.buckets
  if result.buckets.length > 25
    result.buckets = [{label: "notice", value: "too many buckets", \
          count: "buckets count (#{result.buckets.length}) exceeds column limit (25), view single result for details"}]

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
