window.Metrics or= {}

globalParams = (controller) ->
  from: controller.param("from") || "-1h",
  until: controller.param("until") || "now",
  tz: "CET",
  width: "500",
  height: "300"

Metrics.Dashboards =
  "Cloak queries": (controller) ->
    _.map(
          ["rate", "median", "average", "upper_75", "upper_90", "upper_99"],
          (type) ->
            params = globalParams(controller)
            params.target =
              Metrics.GraphiteSeries.
                fromPath([controller.selectedCloaks(), "cloak_core.query_coordinator", type]).
                aggregate(controller.param("aggregation")).
                toString()
            params.title = "query_coordinator: " + type
            params.hideLegend = true
            params
        )
  "JVM": (controller) ->
    _.map(
          [
            "daemon_count", "fd_usage", "heap_commited", "heap_max", "heap_usage", "non_heap_usage", "uptime",
            "thread_count", "gc.PS-MarkSweep.runs", "gc.PS-MarkSweep.time", "gc.PS-Scavenge.runs",
            "gc.PS-Scavenge.time"
          ],
          (metric) ->
            params = globalParams(controller)
            params.target = Metrics.GraphiteSeries.
              fromPath([controller.selectedCloaks(), "cloak_core.jvm", metric, "value"]).
              aggregate(controller.param("aggregation")).
              toString()
            params.title = metric
            params.hideLegend = true
            params
        )