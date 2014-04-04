//= require ./graphite_series
//= require ./dashboards

# Setup the global namespace
window.Metrics or= {}

# Controller and view responsible for metrics presentation
class Metrics.Controller
  # Hack used for passing global cluster -> cloaks mapping to the client side.
  # We take the hash into a local variable, and then remove it from the global
  # scope.
  clusterCloaks = window.clusterCloaks
  delete window.clusterCloaks


  ## -------------------------------------------------------------------
  ## Public instance methods
  ## -------------------------------------------------------------------

  constructor: () ->
    initDashboard()
    initAggregations();
    @viewState = new ViewState(["cluster", "dashboard", "aggregation", "from", "until", "errorMargin"])
    @showHideControls()
    @subscribeToEvents()

  selectedCloaks: () ->
    (clusterCloaks[@param("cluster")] || []).
      map((cloakName) -> cloakName.replace(/\./g, "_"))

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
    $("<img/>", {src: "/metrics/render_graph?" + $.param(@graphParams(target.params))}).
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
    $("#errorMargin").click(this.renderGraphs.bind(this))
    $(window).bind("popstate", @renderGraphs.bind(this))

  showHideControls: () ->
    $("#inputControls").show()
    if @param("drilldown")
      $("#clusterParams").hide()
    else
      $("#clusterParams").show()


  ## -------------------------------------------------------------------
  ## Private class functions
  ## -------------------------------------------------------------------

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


# Creates anonymous controller instance. Since controller binds to various
# events, its reference will be kept alive, and we don't need to store it
# anywhere in the global (window) context.
(new Metrics.Controller())