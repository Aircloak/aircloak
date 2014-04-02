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
  "JVM": (controller) ->
    _.map(
          [
            "daemon_count", "fd_usage", "heap_commited", "heap_max", "heap_usage", "non_heap_usage", "uptime",
            "thread_count", "gc.PS-MarkSweep.runs", "gc.PS-MarkSweep.time", "gc.PS-Scavenge.runs",
            "gc.PS-Scavenge.time"
          ],
          (metric) ->
            Metrics.GraphiteSeries.
              fromPath([controller.selectedCloaks(), "cloak_core.jvm", metric, "value"]).
              alias(metric).
              toString()
        )