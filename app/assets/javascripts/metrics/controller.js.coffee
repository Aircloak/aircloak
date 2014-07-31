//= require backbone
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
  view = null
  autoRefreshInterval = null

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

  render = ->
    viewState.recalcAndStore()
    loadGraphs(runDashboard())
    showHideControls()

  onFormSubmitted = (event) ->
    event.preventDefault()
    render()

  renderNewTab = () ->
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
    element = $(element)

    groupElement = createGroupElement(target)
    groupElement.find(".graphs").append(element.hide())
    if target.group
      groupElement.find(".selectors").
        append(
              $("<input type='radio'/>").
                attr("name", target.group.id).
                on("click", ->
                      groupElement.find(".graphs").children().hide()
                      element.show()
                    )
            ).
        append("#{target.group.graphId}&nbsp;")
        groupElement.find(".selectors input:first-child").attr("checked", "checked")

    groupElement.find(".graphs").children(":first").show()


  createGroupElement = (target) ->
    groupElement = if target.group
      if $("#graphGroup_#{target.group.id}")[0]
        $("#graphGroup_#{target.group.id}")
      else
        $("<div/>").
          attr("id", "graphGroup_#{target.group.id}").
          html(HandlebarsTemplates["metrics/group"])
    else
      $("<div><div class='graphs'/></div>")

    unless groupElement.parent()[0]
      $("#graphs").append(groupElement).append("<hr/>")

    groupElement

  loadGraphs = (targets) ->
    preloadImages(
          _.map(targets, (target) => "/metrics/render_graph?" + $.param(graphParams(target.params))),
          (images) =>
            $("#graphs").html("")
            _.each(
                  _.zip(targets, images),
                  (imageData) => makeElement.apply(null, imageData)
                )
        )

  onAutoRefreshClicked = ->
    window.clearInterval(autoRefreshInterval) if autoRefreshInterval
    if $("#autoRefresh").prop("checked")
      autoRefreshInterval = setInterval(render, 30000)
    else
      autoRefreshInterval = null

  param = (name) -> viewState.param(name)

  showHideControls = () ->
    $("#dashboardParams").show()
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

  view = new Backbone.View({
    el: "#dashboardParams",
    render: render,
    events:
      "click #renderGraphs": onFormSubmitted
      "click #renderGraphsNewTab": renderNewTab
      "click #errorMargin": render
      "click #autoRefresh": onAutoRefreshClicked
  })

  initDashboard()
  initAggregations()
  viewState = new ViewState(["cluster", "dashboard", "aggregation", "from", "until", "errorMargin"])

  showHideControls()
  $(window).bind("popstate", render)

  _.extend(self, {
    param: param
    selectedCloaks: ->
      (clusterCloaks[param("cluster")] || []).
        map((cloakName) -> cloakName.replace(/\./g, "_"))
  })

  # We can call this only after the object has been fully constructed, with public
  # properties available.
  render() if (viewState.urlParam("cluster"))
  self



# Creates anonymous controller instance. Since controller binds to various
# events, its reference will be kept alive, and we don't need to store it
# anywhere in the global (window) context.
(new Metrics.Controller())