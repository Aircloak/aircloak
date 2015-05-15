//= require backbone


## ------------------------------------------------------------------
## Models
## ------------------------------------------------------------------

Repo = Backbone.Model.extend
  initialize: ->
    console.log "Initializing repo: #{@get("repo")}"

RepoList = Backbone.Collection.extend
  model: Repo

  initialize: ->
    @loaded = false

  loadBranchInfo: ->
    unless @loaded
      console.log "Loading repos from repolist model"
      @loaded = true
      $.getJSON "/builds/branch_info", (data) =>
        @add(data)


## ------------------------------------------------------------------
## Views
## ------------------------------------------------------------------

RepoView = Backbone.View.extend
  tagName: "div"
  template: HandlebarsTemplates['builds/repo']

  render: ->
    @$el.html (@template @model.toJSON())
    @

BuildFormView = Backbone.View.extend
  el: 'form'

  initialize: ->
    # Dom elements we will need to toggle the UI as expected
    @checkboxManualBuild = $("input#manual_build")[0]
    @manualSettings = $ "div#manual_settings"
    @branchSettings = $ "div#from_branch"
    @branchSelections = $ "input#branch_selections"
    @repoViews = []

    # We default to using develop branch and a non manual build
    @branchSettings.hide()

    # Triggered when we add repo's, which in turn allows us to render them on screen
    @listenTo @model, 'add', @addOne

    $("#build_form").submit =>
      @createSubmittableSelection()

  events: ->
    "change input#from_develop": "setFromDevelop"
    "change input#from_branch": "setFromBranch"
    "change input#manual_build": "toggleManualBuild"

  addOne: (repo) ->
    $(".loading").hide()
    view = new RepoView model: repo
    @repoViews.push view
    $(".repos").append view.render().el

  setFromDevelop: ->
    @branchSettings.hide()

  setFromBranch: ->
    @branchSettings.show()
    @model.loadBranchInfo()

  toggleManualBuild: ->
    if @checkboxManualBuild.checked
      @manualSettings.hide()
    else
      @manualSettings.show()

  createSubmittableSelection: ->
    console.log "Creating JSON"
    data = []
    for repoView in @repoViews
      item =
        id: repoView.model.get "id"
        repo: repoView.model.get "repo"
        sha: repoView.$el.find(":selected").data("sha")
      data.push item
    @branchSelections.val(JSON.stringify data)

  render: ->
    @


## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

$ ->
  Repos = new RepoList
  new BuildFormView
    model: Repos
