# Setup the global namespace
window.Metrics or= {}

# This class can be used as a builder for creating graphite series selector.
# In basic usage, we create an instance of the class from some given path selector
# and then apply various graphite functions on it. Finally, we can call toString
# method to get the query param that can be sent to graphite.
#
# See dashboards for usage examples.
Metrics.GraphiteSeries = (from) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  value = (from || "##path_placeholder##").toString()

  aggregators =
    median: (query) -> query.percentileOfSeries(50)
    upper75: (query) -> query.percentileOfSeries(75)
    upper90: (query) -> query.percentileOfSeries(90)
    upper99: (query) -> query.percentileOfSeries(99)

  applyFun = (name, args...) ->
    stringArgs = _.map([self].concat(args), sanitize).join(",")
    new Metrics.GraphiteSeries("#{name}(#{stringArgs})")

  sanitize = (value) ->
    if value instanceof Metrics.GraphiteSeries
      value.toString()
    else
      JSON.stringify(value)


  # ------------------------------------
  # Constructor
  # ------------------------------------

  _.extend(self, {
    toString: -> value
    alias: (name) -> applyFun("alias", name)
    percentileOfSeries: (percentile) -> applyFun("percentileOfSeries", percentile)
    absolute: -> applyFun("absolute")
    sum: (series) -> applyFun("sumSeries", series)
    diff: (series) -> applyFun("diffSeries", series)
    multiply: (series) -> applyFun("multiplySeries", series)
    scale: (factor) -> applyFun("scale", factor)
    stacked: -> applyFun("stacked")
    derivative: -> applyFun("derivative")
    scaleToSeconds: (seconds) -> applyFun("scaleToSeconds", seconds)

    aggregate: (aggregation) ->
      aggregator = aggregators[aggregation]
      if aggregator
        aggregator(self)
      else
        applyFun(aggregation)

    materialize: (value) ->
      new Metrics.GraphiteSeries(toString().replace("##path_placeholder##", sanitize(value)))
  })


# ------------------------------------
# Public class functions
# ------------------------------------

_.extend(Metrics.GraphiteSeries, {
  fromPath: (path) ->
    fullPath = _.map(path,
          (component) ->
            if component instanceof Array
              ["{", component.join(","), "}"].join("")
            else
              component.toString()
        ).join(".")
    new Metrics.GraphiteSeries(fullPath)
})