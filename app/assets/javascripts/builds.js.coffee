//= require backbone


## ------------------------------------------------------------------
## Views
## ------------------------------------------------------------------

BuildFormView = Backbone.View.extend
  el: 'form'

  initialize: ->
    # Dom elements we will need to toggle the UI as expected
    @checkboxManualBuild = $("input#manual_build")[0]
    @manualSettings = $ "div#manual_settings"
    @branchSettings= $ "div#from_branch"

    # We default to using develop branch and a non manual build
    @branchSettings.hide()

  events: ->
    "change #from_develop": "setFromDevelop"
    "change #from_branch": "setFromBranch"
    "change #manual_build": "toggleManualBuild"

  setFromDevelop: ->
    @branchSettings.hide()

  setFromBranch: ->
    @branchSettings.show()

  toggleManualBuild: ->
    if @checkboxManualBuild.checked
      @manualSettings.hide()
    else
      @manualSettings.show()

  render: ->
    @


## ------------------------------------------------------------------
## Setup
## ------------------------------------------------------------------

$ ->
  new BuildFormView
