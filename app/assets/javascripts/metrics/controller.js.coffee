//= require ./graphite_series
//= require ./dashboards

# Setup the global namespace
window.Metrics or= {}

Metrics.Controller = () ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  viewState = null

  runDashboard = ->
    dashboard = if param("drilldown")
      Metrics.Dashboards["Cloak drilldown"]
    else
      Metrics.Dashboards[param("dashboard")]
    dashboard(self)

  graphParams = (customParams) ->
    _.extend(defaultParams(), customParams)

  defaultParams = ->
    from: param("from") || "-1h",
    until: param("until") || "now",
    tz: "CET",
    width: "500",
    height: "300",
    hideLegend: true

  renderGraphs = ->
    viewState.recalcAndStore()
    loadGraphs(runDashboard())
    showHideControls()

  onFormSubmitted = (event) ->
    event.preventDefault()
    renderGraphs()

  renderGraphsNewTab = () ->
    window.open(window.location.pathname + "?" + $.param(viewState.allParams()), "_blank")

  makeElement = (target, image) ->
    element =
      if image == null
        $("<span>No data for #{target.params.title}</span>")
      else if (target.href)
        $("<a/>").
          attr("href", "?" + $.param(_.extend(_.clone(viewState.allParams()), target.href()))).
          attr("target", "_blank").
          append(image)
      else
        image

    $("<div/>").
      append(element).
      append($("<hr/>"))

  loadGraphs = (targets) ->
    preloadImages(
          _.map(targets, (target) => "/metrics/render_graph?" + $.param(graphParams(target.params))),
          (images) =>
            $("#graphs").html("")
            _.each(
                  _.zip(targets, images),
                  (imageData) => $("#graphs").append(makeElement.apply(this, imageData))
                )
        )

  onAutoRefreshClicked = ->
    window.clearInterval(autoRefreshInterval) if autoRefreshInterval
    if $("#autoRefresh").prop("checked")
      autoRefreshInterval = setInterval(renderGraphs, 30000)
    else
      autoRefreshInterval = null

  param = (name) -> viewState.param(name)

  subscribeToEvents = () ->
    $("#renderGraphs").click(onFormSubmitted)
    $("#renderGraphsNewTab").click(renderGraphsNewTab)
    $("#errorMargin").click(renderGraphs)
    $("#autoRefresh").click(onAutoRefreshClicked)
    $(window).bind("popstate", renderGraphs)

  showHideControls = () ->
    $("#inputControls").show()
    if param("drilldown")
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

  preloadImages = (urls, callback) ->
    count = urls.length
    images = []

    onImageLoaded = ->
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


  # ------------------------------------
  # Constructor
  # ------------------------------------

  viewState = new ViewState(["cluster", "dashboard", "aggregation", "from", "until", "errorMargin"])

  initDashboard()
  initAggregations()
  showHideControls()
  subscribeToEvents()

  _.extend(self, {
    param: param
    selectedCloaks: ->
      (clusterCloaks[param("cluster")] || []).
        map((cloakName) -> cloakName.replace(/\./g, "_"))
  })

  # We can call this only after the object has been fully constructed, with public
  # properties available.
  renderGraphs() if (viewState.urlParam("cluster"))
  self



# Creates anonymous controller instance. Since controller binds to various
# events, its reference will be kept alive, and we don't need to store it
# anywhere in the global (window) context.
(new Metrics.Controller())