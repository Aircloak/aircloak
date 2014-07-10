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
    initAggregations()
    @viewState = new ViewState(["cluster", "dashboard", "aggregation", "from", "until", "errorMargin"])
    @showHideControls()
    @subscribeToEvents()
    @renderGraphs() if (@viewState.urlParam("cluster"))

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

  onFormSubmitted: (event) ->
    event.preventDefault()
    @renderGraphs()

  renderGraphsNewTab: () ->
    window.open(window.location.pathname + "?" + $.param(@viewState.allParams()), "_blank")

  makeElement: (target, image) ->
    element =
      if image == null
        $("<span>No data for #{target.params.title}</span>")
      else if (target.href)
        $("<a/>").
          attr("href", "?" + $.param(_.extend(_.clone(@viewState.allParams()), target.href()))).
          attr("target", "_blank").
          append(image)
      else
        image

    $("<div/>").
      append(element).
      append($("<hr/>"))

  loadGraphs: (targets) ->
    preloadImages(
          _.map(targets, (target) => "/metrics/render_graph?" + $.param(@graphParams(target.params))),
          (images) =>
            $("#graphs").html("")
            _.each(
                  _.zip(targets, images),
                  (imageData) => $("#graphs").append(@makeElement.apply(this, imageData))
                )
        )

  onAutoRefreshClicked: () ->
    window.clearInterval(@autoRefreshInterval) if @autoRefreshInterval
    if $("#autoRefresh").prop("checked")
      @autoRefreshInterval = setInterval(@renderGraphs.bind(this), 30000)
    else
      @autoRefreshInterval = null

  param: (name) -> @viewState.param(name)

  subscribeToEvents: () ->
    $("#renderGraphs").click(this.onFormSubmitted.bind(this))
    $("#renderGraphsNewTab").click(this.renderGraphsNewTab.bind(this))
    $("#errorMargin").click(this.renderGraphs.bind(this))
    $("#autoRefresh").click(this.onAutoRefreshClicked.bind(this))
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

  preloadImages = (urls, callback) ->
    count = urls.length
    images = []

    onImageLoaded = () ->
      count -= 1
      callback(images) if count == 0

    onImageError = (event) ->
      count -= 1
      index = images.indexOf(event.target)
      if (index >= 0)
        images[index] = null
      callback(images) if count == 0

    images = _.map(urls,
          (url) ->
            $("<img/>", {src: url}).
              bind("load", onImageLoaded).
              bind("error", onImageError)[0]
        )


# Creates anonymous controller instance. Since controller binds to various
# events, its reference will be kept alive, and we don't need to store it
# anywhere in the global (window) context.
(new Metrics.Controller())