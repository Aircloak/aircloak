# Setup the global namespace
window.Metrics or= {}

# Global definition of predefined dashboards. If you need to include your own
# dashboard, just add id in the object below, and it should automatically appear
# in the UI.
Metrics.Dashboards =
  # Dashboard for cloak queries
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

  # Dashboard for JVM metrics
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

  # Dashboard for drilldown into individual cloaks.
  # Note: this dashboard is explicitly disabled from dropdown selection in
  # Metrics.Controller. Instead, it is used in a hardcoded fashion, when
  # the user clicks on a graph.
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