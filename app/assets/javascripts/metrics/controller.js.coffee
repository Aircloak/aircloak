//= require ./graphite_series
//= require ./dashboards

window.Metrics or= {}

class Metrics.Controller
  clusterCloaks = window.clusterCloaks
  delete window.clusterCloaks

  constructor: () ->
    initDashboard()
    initAggregations();
    @viewState = new ViewState(['cluster', 'dashboard', 'aggregation', 'from', 'until'])
    subscribeToEvents(this)

  selectedCloaks: () ->
    (clusterCloaks[@param("cluster")] || []).
      map((cloakName) -> cloakName.replace(".", "_"))

  renderGraphs: () ->
    @viewState.store()
    targets = Metrics.Dashboards[@param('dashboard')](this)
    @loadGraphs(targets)

  loadGraphs: (targets) ->
    $("#graphs").html("")
    _.each(targets, (target) =>
          $("<img/>", {src: "http://localhost:10000/render?" + $.param(target)}).
            css("margin-bottom", "40px").
            appendTo($("#graphs"))
        )

  param: (name) -> @viewState.param(name)

  subscribeToEvents = (controller) ->
    $('#renderGraphs').click(_.partial(onRenderGraphs, controller))
    $(window).bind('popstate', () -> controller.renderGraphs())

  onRenderGraphs = (controller) ->
    controller.renderGraphs()

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