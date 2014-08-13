# Setup the global namespace
window.Metrics or= {}

# Global definition of predefined dashboards. If you need to include your own
# dashboard, just add id in the object below, and it should automatically appear
# in the UI.

Metrics.Dashboards =
  "Task": (controller) ->
    _.flatten(
          [
            rateSpec(controller, "task.{started,successful,timeout}", "Task rates"),
            histogramSpec(controller, "task.total", "Task duration", "ms", "taskDurationGroup"),
            stackHistograms(controller, "Duration of batch task phases", "ms", [
                  "task.parse_json", "task.prefetch", "task.group_users_tables", "task.jobs",
                  "task.aggregation", "task.anonymization", "task.send_result"
                ], "batchTaskPhasesGroup")
          ],
          true
        )

  "Job": (controller) ->
    _.flatten(
          [
            rateSpec(controller, "job.{started,successful,fail,timeout}", "Job rates"),
            histogramSpec(controller, "job.duration", "Job duration", "ms", "jobDurationGroup")
            stackHistograms(controller, "Duration of job execution phases", "ms", [
                  "job.queued", "job.duration", "job.data_insertion", "job.run_next"
                ], "jobPhasesGroup")
          ],
          true
        )

  "Prefetch phases": (controller) ->
    stackHistograms(controller, "Duration of prefetch phases", "ms", [
          "prefetch.prepare", "prefetch.execute", "prefetch.return_result"
        ], "prefetchPhasesGroup")

  "User insertion": (controller) ->
    stackHistograms(controller, "Duration of user insertion phases", "us", [
          "insert_user.read_body", "insert_user.decode_json", "insert_user.validate_structure",
          "insert_user.parse_tables", "insert_user.validate_data", "insert_user.insert_data"
        ], "userInsertionGroup")

  "Database operations": (controller) ->
    queryTimesMetrics = existingHistograms(controller, "db_operation.queries.*")
    _.flatten([
          rateSpec(controller, "db_operation.{finished,started}", "Query rates")
          rateSpec(controller, "db_operation.queries.*", "Query type rates")
          stackHistograms(controller, "Duration of database operation phases", "ms", [
                "db_operation.queued", "db_operation.duration"
              ], "databaseOperationPhases"),
          _.map(
                histogramTypes,
                (histogram) ->
                  group: {id: "queryTimes", graphId: histogram}
                  metrics: _.map(queryTimesMetrics, (metric) -> "#{metric}.#{histogram}")
                  metricsDimension: "ms"
                  params:
                    title: title("query times #{histogram}", "ms")
                    hideLegend: false
                    target: _.map(
                          queryTimesMetrics,
                          (metric) -> clusterMetric(controller, "#{metric}.#{histogram}")
                        )
              )
        ],
        true)

  # Dashboard for drilldown into individual cloaks.
  # Note: this dashboard is explicitly disabled from dropdown selection in
  # Metrics.Controller. Instead, it is used in a hardcoded fashion, when
  # the user clicks on a graph.
  "Individual metrics": (controller) ->
    _.flatten(
          _.map(
                controller.metrics(),
                (metric) ->
                  params:
                    _.extend(
                        title: title(metric, controller.param("metricsDimension"))
                        drawNullAsZero: true
                        plotAnonymized(
                              controller,
                              "{#{controller.selectedCloaks()}}.cloak_core.#{metric}",
                              (expression) -> expression.aggregate(controller.aggregation())
                            )
                      )
              )
          true
        )


## -------------------------------------------------------------------
## Private helper functions
## -------------------------------------------------------------------

existingSeries = (controller, path) ->
  params =
    from: controller.param("from") || "-1h"
    until: controller.param("until") || "now"
    tz: "CET"
    target: path
    format: "json"

  response = $.ajax(
        type: "GET",
        url: "/metrics/render_graph?#{$.param(params)}"
        async: false
      )
  try
    return [] unless response.status == 200 && response.responseText
    result = eval(response.responseText)
    return [] unless result instanceof Array
    _.uniq(_.map(
          _.filter(_.map(result, (series) -> series.target), (target) -> target)
          (key) -> key.replace(/^.+\.cloak_core\./,"")
        ))
  catch
    []

existingHistograms = (controller, path) ->
  _.uniq(_.map(
        existingSeries(
              controller,
              "{#{controller.selectedCloaks()}}.cloak_core.#{path}.{#{histogramTypes}}"
            ),
        (target) ->
          parts = target.split(".")
          parts.splice(-1, 1)
          parts.join(".")
      ))

rateSpec = (controller, metricPath, graphTitle) ->
  existingPaths = existingSeries(controller, "{#{controller.selectedCloaks()}}.cloak_core.#{metricPath}.rate")

  metrics: existingPaths
  metricsDimension: "times/sec"
  params:
    title: title(graphTitle, "times/sec")
    hideLegend: false
    target: _.map(existingPaths, _.bind(clusterMetric, null, controller))

clusterMetric = (controller, path) ->
  Metrics.GraphiteSeries.
    fromPath(
          "{#{controller.selectedCloaks()}}.cloak_core.#{path}"
        ).
    aggregate(controller.aggregation()).
    alias(path).
    toString()

histogramTypes = ["median", "average", "upper_75", "upper_90", "upper_99"]
histogramSpec = (controller, metricPath, graphTitle, dimension, groupId) ->
  _.map(
        histogramTypes,
        (type) ->
          group = null
          if groupId
            group = {id: groupId, graphId: type}
          group: group
          metrics: ["#{metricPath}.#{type}"]
          metricsDimension: dimension
          params:
            title: title(graphTitle || metricPath, dimension)
            target: clusterMetric(controller, "#{metricPath}.#{type}")
      )

stackHistograms = (controller, graphTitle, dimension, metrics, groupId) ->
  fullTargets = _.map(metrics, (metric) -> "{#{controller.selectedCloaks()}}.cloak_core.#{metric}.average")
  existingMetrics = _.map(
        existingSeries(controller, fullTargets),
        (target) ->
          res = target.split(".")
          res.splice(-1, 1)
          res.join(".")
      )

  _.map(
          histogramTypes,
          (type) ->
            if groupId
              group = {id: groupId, graphId: type}
            group: group
            metrics: _.map(existingMetrics, (metric) -> "#{metric}.#{type}")
            metricsDimension: dimension
            params:
              title: title("#{graphTitle} #{type}", dimension)
              hideLegend: false
              yMin: 0
              target:
                  _.map(
                        existingMetrics,
                        (phase) ->
                          Metrics.GraphiteSeries.fromPath([
                              controller.selectedCloaks(), "cloak_core.#{phase}.#{type}"
                            ]).
                          aggregate(controller.aggregation()).
                          stacked().
                          alias(phase).
                          toString()
                      )
        )

# Plots anonymized data, optionally drawing error margins around
plotAnonymized = (controller, path, transformer) ->
  if (!transformer)
    transformer = (path) -> path
  highlightError(
        transformer,
        Metrics.GraphiteSeries.fromPath(path)
        Metrics.GraphiteSeries.fromPath(["anonymization_error"].concat(path))
      )

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