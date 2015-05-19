renderGraphs = (buckets) ->
  findValue = (label) ->
    _.chain(buckets)
      .filter((bucket) -> bucket.label == label)
      .map((bucket) -> bucket.value)
      .value()
  return unless _.contains(findValue("ac_query"), "ecdf")
  # The graph element is hidden by default, but now
  # that we know there exists data to render one, we
  # should show the graph.
  $("#chart").removeClass("hidden")
  x_label = findValue("ac_ecdf_x_label")
  y_label = findValue("ac_ecdf_y_label")
  legend = findValue("ac_ecdf_legend")
  values = _.chain(buckets)
    .filter((bucket) -> bucket.label == "ecdf_val")
    .map((bucket) -> {x: parseInt(bucket.value), y: bucket.count})
    .sortBy((datum) -> datum.x)
    .value()
  data = [{
    values: values,
    key: legend,
    color: '#000000'
  }]
  nv.addGraph ->
    chart = nv.models.lineChart()
      .useInteractiveGuideline(true)
    chart.xAxis
      .axisLabel(x_label)
      .tickFormat(d3.format(',r'))
    chart.yAxis
      .axisLabel(y_label)
      .tickFormat(d3.format('.02f'))
    d3.select('#chart svg')
      .datum(data)
      .transition().duration(500)
      .call(chart)
    nv.utils.windowResize(chart.update);
    chart

window.refreshGraphsFromServer = (timestamp, renderCallback) ->
  attemptDataLoad(timestamp, renderCallback, 2)

attemptDataLoad = (timestamp, renderCallback, attemptsRemaining) ->
  taskToken = $(".task_params").data("task-token")
  if attemptsRemaining < 1
    location.reload()
  else
    $.getJSON("/tasks/#{taskToken}/particular_result/#{timestamp}", (data) =>
          if data.success
            result = data.results[0]
            renderCallback(result)
            renderGraphs(result.buckets)
          else
            attemptDataLoad(timestamp, renderCallback, attemptsRemaining - 1)
        )

$ ->
  existingResults = $('.render_params').data('results')
  if existingResults.length > 0
    # We render the graph from the most recent result
    buckets = existingResults[existingResults.length - 1].buckets
    renderGraphs(buckets)
