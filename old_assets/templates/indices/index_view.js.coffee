class Queries.Views.IndexView extends Backbone.View

  template: JST['indices/index_view']

  render: ->
    $(@el).html(@template(query_file: @model))
    this
