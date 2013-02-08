class Queries.Views.QueryFilesIndex extends Backbone.View

  template: JST['query_files/index']

  initialize: ->
    window.QueryFileView = this
    @collection.on 'add', @appendQueryFile, this
    @collection.on 'reset', @render, this
    @.on 'new_query_file', @newQueryFile, this

  render: =>
    console.log "Called render on view query files index, collection size: #{@collection.length}"
    $(@el).html(@template())
    @collection.each(@appendQueryFile)
    this

  newQueryFile: (queryFile) =>
    @collection.add queryFile

  appendQueryFile: (queryFile) =>
    console.log "appending query file to output"
    view = new Queries.Views.QueryFile(model: queryFile)
    console.log "View as rendered with queryfile: #{view.render().el}"
    console.log view.render().el
    @$('#query_files').append(view.render().el)
