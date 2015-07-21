# include buckets aggregation functionality
//= require tasks/results_aggregation

# create namespace for results-related shared variables
window.Results = window.Results or {}
# the time when last article was published
Results.last_article_update = 0

name_from_bucket = (bucket) ->
  _.compact([bucket.label, bucket.value]).join(": ")


format_date = (timestamp) ->
  # this will be shown in local time as "YYYY-MM-DD hh:mm:ss"
  date = new Date(timestamp).toISOString()
  date.substring(0, date.length - 5).replace('T', ' ')


# adds a row to the results table representing the specified result
Results.display = (result) ->
  result.buckets = _.sortBy(result.buckets, name_from_bucket)
  result.buckets = Results.aggregate_quantized_buckets result.buckets
  if result.buckets.length > 100
    result.buckets = [{label: "notice", value: "too many buckets", \
          count: "buckets count (#{result.buckets.length}) exceeds row limit (100), use REST API or CSV export to view result"}]

  timestamp = parseInt result.published_at
  text = format_date timestamp
  if timestamp < Results.task_last_update
    text += " (outdated code)"
  $('#time').text text

  if result.exceptions.length > 0
    text = ""
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
      text += "#{exception.count} occurances of \"#{error}\"\n"
      $('#errors').text text.trim()
  else
    $('#errors').text "none"

  table = document.getElementById 'result_table'
  # delete all rows
  while table.rows.length > 0
    table.deleteRow 0

  for bucket in result.buckets
    row = table.insertRow -1

    property = row.insertCell 0
    property.innerHTML = name_from_bucket bucket
    property.style.textAlign = 'left'

    count = row.insertCell 1
    count.innerHTML = bucket.count


$ ->
  Results.task_last_update = $('.render_params').data('task-last-update')
  result = $('.render_params').data('result')
  Results.display result if result