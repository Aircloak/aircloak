window.Metrics or= {}

class Metrics.GraphiteSeries
  applyFun = (name, args...) ->
    new GraphiteSeries([name, "(", sanitizeArgs(args).join(","), ")"].join(""))

  @fromPath = (path) ->
    fullPath = _.map(path,
          (component) ->
            if component instanceof Array
              ["{", component.join(","), "}"].join("")
            else
              component.toString()
        ).join(".")
    new GraphiteSeries(fullPath)

  sanitizeArgs = (args) ->
    _.map(args,
          (arg) ->
            if arg instanceof GraphiteSeries
              arg.toString()
            else
              JSON.stringify(arg)
        )

  constructor: (from) ->
    value = (from || "").toString()
    @toString = () -> value

  alias: (name) -> applyFun("alias", this, name)
  percentileOfSeries: (percentile) -> applyFun("percentileOfSeries", this, percentile)
  applyFun: (args...) -> applyFun.apply(null, [args[0], this].concat(args[1..-1]))

  aggregators =
    median: (query) -> query.percentileOfSeries(50)
    upper75: (query) -> query.percentileOfSeries(75)
    upper90: (query) -> query.percentileOfSeries(90)
    upper99: (query) -> query.percentileOfSeries(99)

  aggregate: (aggregation) ->
    aggregator = aggregators[aggregation]
    if aggregator
      aggregator(this)
    else
      this.applyFun(aggregation)