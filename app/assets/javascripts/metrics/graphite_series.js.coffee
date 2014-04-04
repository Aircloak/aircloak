# Setup the global namespace
window.Metrics or= {}

# This class can be used as a builder for creating graphite series selector.
# In basic usage, we create an instance of the class from some given path selector
# and then apply various graphite functions on it. Finally, we can call toString
# method to get the query param that can be sent to graphite.
#
# See dashboards for usage examples.
class Metrics.GraphiteSeries
  ## -------------------------------------------------------------------
  ## Public instance methods
  ## -------------------------------------------------------------------

  constructor: (from) ->
    value = (from || "##path_placeholder##").toString()
    @toString = () -> value

  alias: (name) -> @applyFun("alias", name)
  percentileOfSeries: (percentile) -> @applyFun("percentileOfSeries", percentile)
  absolute: () -> @applyFun("absolute")
  sum: (series) -> @applyFun("sumSeries", series)
  diff: (series) -> @applyFun("diffSeries", series)
  multiply: (series) -> @applyFun("multiplySeries", series)
  scale: (factor) -> @applyFun("scale", factor)
  stacked: () -> @applyFun("stacked")
  derivative: () -> @applyFun("derivative")
  scaleToSeconds: (seconds) -> @applyFun("scaleToSeconds", seconds)
  applyFun: (args...) -> applyFun.apply(null, [args[0], this].concat(args[1..-1]))

  aggregate: (aggregation) ->
    aggregator = aggregators[aggregation]
    if aggregator
      aggregator(this)
    else
      this.applyFun(aggregation)

  materialize: (value) ->
    new Metrics.GraphiteSeries(@toString().replace("##path_placeholder##", sanitize(value)))


  ## -------------------------------------------------------------------
  ## Public class functions
  ## -------------------------------------------------------------------

  # Can be used to create initial
  @fromPath = (path) ->
    fullPath = _.map(path,
          (component) ->
            if component instanceof Array
              ["{", component.join(","), "}"].join("")
            else
              component.toString()
        ).join(".")
    new GraphiteSeries(fullPath)


  ## -------------------------------------------------------------------
  ## Private class functions
  ## -------------------------------------------------------------------

  aggregators =
    median: (query) -> query.percentileOfSeries(50)
    upper75: (query) -> query.percentileOfSeries(75)
    upper90: (query) -> query.percentileOfSeries(90)
    upper99: (query) -> query.percentileOfSeries(99)

  applyFun = (name, args...) ->
    new GraphiteSeries([name, "(", sanitizeArgs(args).join(","), ")"].join(""))

  sanitizeArgs = (args) -> _.map(args, sanitize)

  sanitize = (value) ->
    if value instanceof GraphiteSeries
      value.toString()
    else
      JSON.stringify(value)