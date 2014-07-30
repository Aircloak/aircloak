# Setup the global namespace
window.Metrics or= {}

# Global definition of predefined dashboards. If you need to include your own
# dashboard, just add id in the object below, and it should automatically appear
# in the UI.

Metrics.Dashboards =
  "Task": (controller) ->
    _.flatten(
          [
            rateSpec(controller, "task.started", "Task started"),
            rateSpec(controller, "task.successful", "Task success"),
            rateSpec(controller, "task.timeout", "Task timeout"),
            histogramSpec(controller, "task.total", "Task duration", "ms")
          ],
          true
        )

  "Job": (controller) ->
    _.flatten(
          [
            rateSpec(controller, "job.started", "Job started"),
            rateSpec(controller, "job.successful", "Job success"),
            rateSpec(controller, "job.fail", "Job failed"),
            rateSpec(controller, "job.timeout", "Job Timeout"),
            histogramSpec(controller, "job.duration", "Job duration", "ms")
          ],
          true
        )

  "Batch task phases": (controller) ->
    stackHistograms(controller, "Duration of batch task phases", "ms", [
          "task.parse_json", "task.prefetch", "task.group_users_tables", "task.jobs", "task.aggregation",
          "task.anonymization", "task.send_result"
        ])

  "Prefetch phases": (controller) ->
    stackHistograms(controller, "Duration of prefetch phases", "ms", [
          "prefetch.prepare", "prefetch.execute", "prefetch.return_result"
        ])

  "Job phases": (controller) ->
    stackHistograms(controller, "Duration of job execution phases", "ms", [
          "job.queued", "job.duration", "job.data_insertion"
        ])

  "User insertion": (controller) ->
    stackHistograms(controller, "Duration of user insertion phases", "us", [
          "insert_user.read_body", "insert_user.decode_json", "insert_user.validate_structure",
          "insert_user.parse_tables", "insert_user.validate_data", "insert_user.insert_data"
        ])

  "Database operations": (controller) ->
    result = []

    result = result.concat([
          params:
            title: title("rate", "times/sec")
            hideLegend: false
            target: "{#{controller.selectedCloaks()}}.cloak_core.db_operation.{finished,started}.rate"
        ])

    result = result.concat(stackHistograms(controller, "Duration of database operation phases", "ms", [
          "db_operation.queued", "db_operation.duration"
        ]))

    result = result.concat([
          params:
            title: title("query rates", "times/sec")
            hideLegend: false
            target: "{#{controller.selectedCloaks()}}.cloak_core.db_operation.queries.*.rate"
        ])

    result = _.reduce(
          histogramTypes,
          (memo, histogram) ->
            memo.push(
                  params:
                    title: title("query times #{histogram}", "ms")
                    hideLegend: false
                    target: "{#{controller.selectedCloaks()}}.cloak_core.db_operation.queries.*.#{histogram}"
                )
            memo
          result
        )

    result

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
                    title: "#{cloak}: #{controller.param("drilldownTitle") || controller.param("drilldown")}"
                    plotAnonymized(
                            controller,
                            "#{cloak}.#{controller.param("drilldown")}",
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

histogramTypes = ["median", "average", "upper_75", "upper_90", "upper_99"]

rateSpec = (controller, metricPath, graphTitle) ->
  metricSpec(controller, "#{metricPath}.rate", "#{graphTitle} rate", "times/sec")

histogramSpec = (controller, metricPath, graphTitle, dimension) ->
  _.map(
        histogramTypes,
        (type) ->
          metricSpec(controller, "#{metricPath}.#{type}", "#{graphTitle} #{type}", dimension)
      )

metricSpec = (controller, metricPath, graphTitle, dimension) ->
      href: () ->
        drilldown: "cloak_core.#{metricPath}"
        drilldownTitle: graphTitle
      params:
        _.extend(
            title: title(graphTitle, dimension)
            drawNullAsZero: true
            plotAnonymized(
                  controller,
                  "{#{controller.selectedCloaks()}}.cloak_core.#{metricPath}",
                  (expression) -> expression.aggregate(controller.param("aggregation"))
                )
          )

stackHistograms = (controller, graphTitle, dimension, metrics) ->
  _.map(
          histogramTypes,
          (type) ->
            params:
              title: title("#{graphTitle} #{type}", dimension)
              hideLegend: false
              yMin: 0
              target:
                  _.map(
                        metrics,
                        (phase) ->
                          Metrics.GraphiteSeries.fromPath([
                              controller.selectedCloaks(), "cloak_core.#{phase}.#{type}"
                            ]).
                          aggregate(controller.param("aggregation")).
                          stacked().
                          alias(phase).
                          toString()
                      )
        )

# Plots anonymized data, optionally drawing error margins around
plotAnonymized = (controller, path, transformer) ->
  if (!transformer)
    transformer = (path) -> path
  if controller.param("errorMargin")
    highlightError(
          transformer,
          Metrics.GraphiteSeries.fromPath(path)
          Metrics.GraphiteSeries.fromPath(["anonymization_error"].concat(path))
        )
  else
    target: transformer(Metrics.GraphiteSeries.fromPath(path)).toString()

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
    "#{name} [#{unit}]"
  else
    name