window.QueryFileView = ->
  alert "not set"

window.Queries =
  Models: {}
  Collections: {}
  Views: {}
  Routers: {}
  init: ->
    new Queries.Routers.QueryFiles()
    Backbone.history.start(pushState: true)

$(document).ready ->
  Queries.init()
