$ ->
  _.each $("svg#integration_test"), (svg) ->
    data = $(svg).data("results")
    categories = _.chain(data)
      .map((row) -> _.keys(row.timings[0]))
      .flatten()
      .union()
      .value()
    renderable_categories = []
    for category in categories
      category_values = _.chain(data)
        .map((row) ->
          timings = row.timings[0]
          # The javascript date object expects times in ms
          [row.created_at * 1000, timings[category]])
        .filter((row) -> row != null)
        .value()
      renderable_categories.push({key: category, values: category_values})

    nv.addGraph ->
      chart = nv.models.lineWithFocusChart()
          .x((d) -> d[0])
          .y((d) -> d[1])

      chart.xAxis
          .axisLabel('Date')
          .tickFormat((d) -> d3.time.format('%b %d %H:%M')(new Date(d)))
      chart.xScale(d3.time.scale.utc())
      chart.yAxis.axisLabel('Phase duration /s')

      chart.x2Axis
          .axisLabel('Date')
          .tickFormat((d) -> d3.time.format('%b %d')(new Date(d)))
      chart.y2Axis.axisLabel('Phase duration /s')

      d3.select(svg)
        .datum(renderable_categories)
        .transition().duration(500)
        .call(chart)

      nv.utils.windowResize(chart.update)

      chart
