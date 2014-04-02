window.Metrics or= {}

Metrics.Dashboards =
  "Cloak queries": (controller) ->
    _.map(
          ["rate", "median", "average", "upper_75", "upper_90", "upper_99"],
          (type) ->
            href: () ->
              drilldown: ["cloak_core.query_coordinator", type].join(".")
            params:
              title: "query_coordinator: " + type,
              target:
                Metrics.GraphiteSeries.
                  fromPath([controller.selectedCloaks(), "cloak_core.query_coordinator", type]).
                  aggregate(controller.param("aggregation")).
                  toString()
        )
  "JVM": (controller) ->
    _.map(
          [
            "daemon_count", "fd_usage", "heap_commited", "heap_max", "heap_usage", "non_heap_usage", "uptime",
            "thread_count", "gc.PS-MarkSweep.runs", "gc.PS-MarkSweep.time", "gc.PS-Scavenge.runs",
            "gc.PS-Scavenge.time"
          ],
          (metric) ->
            href: () ->
              drilldown: ["cloak_core.jvm", metric, "value"].join(".")
            params:
              title: metric,
              target:
                Metrics.GraphiteSeries.
                  fromPath([controller.selectedCloaks(), "cloak_core.jvm", metric, "value"]).
                  aggregate(controller.param("aggregation")).
                  toString()
        )
  "Cloak drilldown": (controller) ->
    _.map(
          controller.selectedCloaks(),
          (cloak) ->
            params:
              title: [cloak, ": ", controller.param("drilldown")].join(""),
              target:
                Metrics.GraphiteSeries.
                  fromPath([cloak, controller.param("drilldown")]).
                  toString()
        )