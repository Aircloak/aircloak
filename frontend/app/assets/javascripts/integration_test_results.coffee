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
          returnable_row =
            # The javascript date object expects times in ms
            x: row.created_at * 1000
            y: if timings[category] then timings[category] else 0
          )
        .filter((row) -> row != null)
        .value()
      renderable_categories.push({key: category, values: category_values})

    nv.addGraph ->
      chart = nv.models.multiBarChart()
          .reduceXTicks(true)
          .showControls(true)
          .stacked(true)

      chart.xAxis
          .axisLabel('Date')
          .tickFormat((d) -> d3.time.format('%b %d %H:%M')(new Date(d)))

      chart.yAxis.axisLabel('Duration /s')

      d3.select(svg)
        .datum(renderable_categories)
        .call(chart)

      nv.utils.windowResize(chart.update)

      chart
