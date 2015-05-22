renderGraphs = (buckets) ->
  graphs = _.chain(buckets)
    .filter((bucket) -> bucket.label == "ac_graph")
    .map((bucket) -> JSON.parse(bucket.value))
    .value()
  return unless graphs.length == 1
  graph = graphs[0]

  # The graph element is hidden by default, but now
  # that we know there exists data to render one, we
  # should show the graph.
  $("#chart").removeClass("hidden")

  values = _.chain(graph.data)
    .sortBy((val) -> val.x)
    .value()
  data = [{
    values: values,
    key: graph["legend"],
    color: '#000000'
  }]
  nv.addGraph ->
    chart = nv.models.lineChart()
      .useInteractiveGuideline(true)
    chart.xAxis
      .axisLabel(graph["x_label"])
      .tickFormat(d3.format(',r'))
    chart.yAxis
      .axisLabel(graph["y_label"])
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
            renderGraphs(result.buckets)
            renderCallback(result)
          else
            attemptDataLoad(timestamp, renderCallback, attemptsRemaining - 1)
        )

$ ->
  existingResults = $('.render_params').data('results')
  if existingResults.length > 0
    # We render the graph from the most recent result
    buckets = existingResults[existingResults.length - 1].buckets
    renderGraphs(_.clone(buckets))
