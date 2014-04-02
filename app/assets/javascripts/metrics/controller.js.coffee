//= require ./graphite_series
//= require ./dashboards

window.Metrics or= {}

class Metrics.Controller
  clusterCloaks = window.clusterCloaks
  delete window.clusterCloaks

  constructor: () ->
    initDashboard()
    initAggregations();
    @parseParams();
    subscribeToEvents(this)

  selectedCloaks: () ->
    (clusterCloaks[@param("cluster")] || []).
      map((cloakName) -> cloakName.replace(".", "_"))

  renderGraphs: () ->
    @parseParams();
    targets = Metrics.Dashboards[@param('dashboard')](this)
    @loadGraphs(targets)

  loadGraphs: (targets) ->
    $("#graphs").html("")
    _.each(targets, (target) =>
          $("<img/>", {src: "http://localhost:10000/render?" + $.param(target)}).
            css("margin-bottom", "40px").
            appendTo($("#graphs"))
        )

  param: (value) ->
    parseParams() unless @params
    @params[value]

  parseParams: () ->
    @params = pageParams()
    _.each(
          ['cluster', 'dashboard', 'aggregation', 'from', 'until'],
          (param) => $('#' + param).val(@params[param]) if @params[param]
        )
    @params = _.extend(@params, controlsToParams())

  controlsToParams = () ->
    _.reduce(
          ['cluster', 'dashboard', 'aggregation', 'from', 'until'],
          ((memo, param) ->
            ctrlValue = $('#' + param).val()
            memo[param] = ctrlValue
            memo
          ),
          pageParams()
        )

  pageParams = () ->
    return {} if !window.location.search
    _.reduce(
          window.location.search.replace(/^\?/, "").split("&"),
          ((memo, param) ->
            [name, value] = param.split("=")
            memo[name] = decodeURIComponent(value.replace(/\+/g, ' '))
            memo),
          {}
        )

  subscribeToEvents = (controller) ->
    $('#renderGraphs').click(_.partial(onRenderGraphs, controller))
    $(window).bind('popstate', () -> controller.renderGraphs())

  onRenderGraphs = (controller) ->
    setNewState()
    controller.renderGraphs()

  setNewState = () ->
    history.replaceState(null, "", "?" + $.param(controlsToParams()))

  initDropdown = (dropdown, items) =>
    for text, value of items
      value = text if value instanceof Function
      dropdown.append($('<option/>').attr("value", value).text(text))

  initDashboard = () ->
    initDropdown($('#dashboard'), Metrics.Dashboards)

  initAggregations = () ->
    initDropdown($('#aggregation'),
          average: "averageSeries",
          max: "maxSeries",
          min: "minSeries",
          median: "median",
          upper75: "upper75",
          upper90: "upper90",
          upper99: "upper99"
        )

(new Metrics.Controller())