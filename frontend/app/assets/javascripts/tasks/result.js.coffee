# include buckets aggregation functionality
//= require tasks/results_aggregation
# include result exception handling functionality
//= require tasks/result_exception

# create namespace for results-related shared variables
window.Results or= {}

# the time when last article was published
Results.last_article_update = 0


name_from_bucket = (bucket) ->
  _.compact([bucket.label, bucket.value]).join(": ")


format_date = (timestamp) ->
  # this will be shown in local time as "YYYY-MM-DD hh:mm:ss"
  date = new Date(timestamp).toISOString()
  date.substring(0, date.length - 5).replace('T', ' ')


plot_data_callback = (name, data, plot_step) ->
  # create chart canvas
  svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
  svg.setAttribute 'width', '840px'
  svg.setAttribute 'height', '260px'
  svg.setAttributeNS 'http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink'

  # insert chart canvas into page
  # Chrome and IE ignore SVG size, we need to create a parent container with the proper size
  div = document.createElement('div')
  div.style.width = '840px'
  div.style.height = '260px'
  div.appendChild svg
  $("#charts").append div

  # build chart
  nv.addGraph ->
    # create chart
    chart = nv.models.multiBarChart()
        .margin({left: 40})
        .showControls(false)
        .tooltips(false)
        .reduceXTicks(false)

    # configure chart
    format_value = (value) ->
      Math.round(value * 100) / 100
    chart.xAxis.tickFormat(format_value)
    chart.yAxis.tickFormat(d3.format(',.0f'))
    plot_step = format_value(plot_step)
    legend = "#{name} (bar width = #{plot_step})"
    d3.select(svg)
        .datum([{values: data, key: legend, color: '#7777ff', area: true}])
        .transition().duration(500)
        .call(chart)
    chart

# adds a row to the results table representing the specified result
Results.display = (result) ->
  # remove old charts, if any
  $("#charts").empty()

  # call page new result callback if any registered
  Results.new_result_callback(result) if Results.new_result_callback
  # aggregate and plot quantized data
  result.buckets = Results.aggregate_quantized_buckets result.buckets, plot_data_callback

  if result.buckets.length > 100
    result.buckets = [{label: "notice", value: "too many buckets", \
          count: "buckets count (#{result.buckets.length}) exceeds row limit (100), use REST API or CSV export to view result"}]

  timestamp = parseInt result.published_at
  text = format_date timestamp
  if timestamp < Results.task_last_update
    text += " (outdated code)"
  $('#time').text text

  if result.exceptions.length > 0
    error_texts = _.map result.exceptions, (exception) ->
        ResultException.format_error(ResultException.parse_to_error exception)
    $('#errors').text error_texts.join(", ")
    $('#errors').addClass 'error-text'
  else
    $('#errors').text "none"
    $('#errors').removeClass 'error-text'

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