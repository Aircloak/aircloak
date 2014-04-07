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
            graphTitle = title("query_coordinator: " + type, if type == "rate" then "queries/s" else "ms")
            href: () ->
              drilldown: ["cloak_core.query_coordinator", type].join(".")
              drilldownTitle: graphTitle
            params:
              _.extend(
                  title: graphTitle
                  plotAnonymized(
                        controller,
                        [controller.selectedCloaks(), "cloak_core.query_coordinator", type],
                        (expression) -> expression.aggregate(controller.param("aggregation"))
                      )
                )
        )

  # Dashboard for JVM metrics
  "JVM": (controller) ->
    units =
      daemon_count: "live daemon threads",
      fd_usage: "%",
      heap_commited: "MB", heap_max: "MB", heap_usage: "%", non_heap_usage: "%",
      uptime:"s", thread_count: "threads",
      "gc.PS-Scavenge.runs": "runs/s",
      "gc.PS-Scavenge.time": "ms/s",
      "gc.PS-MarkSweep.runs": "runs/s",
      "gc.PS-MarkSweep.time": "ms/s"
    _.map(
          [
            "thread_count", "daemon_count", "fd_usage",
            "heap_commited", "heap_max", "heap_usage", "non_heap_usage",
            "gc.PS-Scavenge.runs", "gc.PS-Scavenge.time", "gc.PS-MarkSweep.runs", "gc.PS-MarkSweep.time",
          ],
          (metric) ->
            graphTitle = title(metric, units[metric])
            transform = new Metrics.GraphiteSeries()
            transform = transform.scale(1/(1024*1024)) if units[metric] == "MB"
            transform = transform.derivative().scaleToSeconds(1) if metric.match(/^gc\./)
            href: () ->
              transform: transform.toString()
              drilldown: ["cloak_core.jvm", metric, "value"].join("."),
              drilldownTitle: graphTitle
            params:
              _.extend(
                    title: graphTitle,
                    lineMode: "staircase",
                    plotAnonymized(
                            controller,
                            [controller.selectedCloaks(), "cloak_core.jvm", metric, "value"],
                            (expression) ->
                              transform.
                                materialize(expression).
                                aggregate(controller.param("aggregation"))
                          )
                  )
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
              _.extend(
                    title: [
                        cloak, ": ", controller.param("drilldownTitle") || controller.param("drilldown")
                      ].join(""),
                    plotAnonymized(
                            controller,
                            [cloak, controller.param("drilldown")],
                            (expression) ->
                              if controller.param("transform")
                                (new Metrics.GraphiteSeries(controller.param("transform"))).
                                  materialize(expression)
                              else
                                expression
                          )
                  )
        )


## -------------------------------------------------------------------
## Private helper functions
## -------------------------------------------------------------------

# Plots anonymized data, optionally drawing error margins around
plotAnonymized = (controller, path, transformer) ->
  if (!transformer)
    transformer = (path) -> path
  if controller.param("errorMargin")
    highlightError(
          transformer,
          fromPath(path)
          fromPath(["anonymization_error"].concat(path))
        )
  else
    target: transformer(fromPath(path)).toString()

fromPath = (path) -> Metrics.GraphiteSeries.fromPath(path)

# Describes the graph containing anonymized data together with an error area surrounding it.
# The area is as large as an error (absolute), and centered around the drawn
# graph.
highlightError = (transformer, anonymized, relativeError) ->
  absoluteError = relativeError.absolute().scale(0.01).multiply(anonymized)
  colorList: "ffffff00,ff000060,blue", # transparent, semi-opaque red, black
  target:
    [
      transformer(anonymized.diff(absoluteError.scale(0.5))).stacked().toString(),
      transformer(absoluteError).stacked().toString(),
      transformer(anonymized).toString()
    ]

# Make title, with optional unit
title = (name, unit) ->
  if unit
    [name, " [", unit, "]"].join("")
  else
    name