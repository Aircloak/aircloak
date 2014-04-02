window.Metrics or= {}

histogram = (fullPath, aggregation, metricName) ->
  _.map(
        ["rate", "median", "average", "upper_75", "upper_90", "upper_99"],
        (type) ->
          Metrics.GraphiteSeries.
            fromPath(fullPath.concat([type])).
            aggregate(aggregation).
            alias([type, "(", metricName ,")"].join("")).
            toString()
      )

Metrics.Dashboards =
  "Cloak queries": (controller) ->
    histogram(
          [controller.selectedCloaks(), "cloak_core.query_coordinator"],
          controller.param("aggregation")
          "query_coordinator"
      )