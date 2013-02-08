class Queries.Views.QueryFile extends Backbone.View

  template: JST['query_files/query_file']

  render: ->
    $(@el).html(@template(query_file: @model))
    console.log "In query_file render."
    console.log @model
    if @model.get('index_ops')
      indicesCollection = new Queries.Collections.Indices()
      indicesView = new Queries.Views.IndicesIndex(collection: indicesCollection)
      @$(".indices").append(indicesView.render().el)
      indicesCollection.add @model.get('indices')
    this
