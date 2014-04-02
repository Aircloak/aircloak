//= require ./graphite_series
//= require ./dashboards

window.Metrics or= {}

class Metrics.Controller
  clusterCloaks = window.clusterCloaks
  delete window.clusterCloaks

  constructor: () ->
    initDashboard()
    initAggregations();
    @viewState = new ViewState(["cluster", "dashboard", "aggregation", "from", "until"])
    @showHideControls()
    @subscribeToEvents()

  selectedCloaks: () ->
    (clusterCloaks[@param("cluster")] || []).
      map((cloakName) -> cloakName.replace(".", "_"))

  runDashboard: () ->
    dashboard = if @param("drilldown")
      Metrics.Dashboards["Cloak drilldown"]
    else
      Metrics.Dashboards[@param("dashboard")]
    dashboard(this)

  graphParams: (customParams) ->
    _.extend(
          {
            from: @param("from") || "-1h",
            until: @param("until") || "now",
            tz: "CET",
            width: "500",
            height: "300",
            hideLegend: true
          },
          customParams
        )

  renderGraphs: () ->
    @viewState.recalcAndStore()
    @loadGraphs(@runDashboard())
    @showHideControls()

  renderGraphsNewTab: () ->
    window.open(window.location.pathname + "?" + $.param(@viewState.allParams()), "_blank")

  makeImage: (target) ->
    $("<img/>", {src: "http://localhost:10000/render?" + $.param(@graphParams(target.params))}).
      css("margin-bottom", "40px")

  makeElement: (target) ->
    if (target.href)
      $("<a/>").
        attr("href", "?" + $.param(_.extend(_.clone(@viewState.allParams()), target.href()))).
        attr("target", "_blank").
        append(@makeImage(target))
    else
      @makeImage(target)

  loadGraphs: (targets) ->
    $("#graphs").html("")
    _.each(targets, (target) => $("#graphs").append(@makeElement(target)))

  param: (name) -> @viewState.param(name)

  subscribeToEvents: () ->
    $("#renderGraphs").click(this.renderGraphs.bind(this))
    $("#renderGraphsNewTab").click(this.renderGraphsNewTab.bind(this))
    $(window).bind("popstate", @renderGraphs.bind(this))

  showHideControls: () ->
    $("#inputControls").show()
    if @param("drilldown")
      $("#clusterParams").hide()
    else
      $("#clusterParams").show()

  initDropdown = (dropdown, items) =>
    for text, value of items
      value = text if value instanceof Function
      dropdown.append($("<option/>").attr("value", value).text(text))

  initDashboard = () ->
    initDropdown($("#dashboard"), _.omit(Metrics.Dashboards, "Cloak drilldown"))

  initAggregations = () ->
    initDropdown($("#aggregation"),
          average: "averageSeries",
          max: "maxSeries",
          min: "minSeries",
          median: "median",
          upper75: "upper75",
          upper90: "upper90",
          upper99: "upper99"
        )

(new Metrics.Controller())