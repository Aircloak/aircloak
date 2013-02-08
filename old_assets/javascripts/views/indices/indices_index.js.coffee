class Queries.Views.IndicesIndex extends Backbone.View

  template: JST['indices/index']

  initialize: ->
    @collection.on 'add', @appendIndex

  render: ->
    $(@el).html @template()
    @collection.each @appendIndex
    this

  appendIndex: (index) =>
    view = new Queries.View.IndexView(model: index)
    @$('#indices').append(view.render().el)
