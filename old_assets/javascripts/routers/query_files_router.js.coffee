class Queries.Routers.QueryFiles extends Backbone.Router
  routes:
    'queries/new': 'new'
    'queries/:id/edit': 'edit'

  initialize: ->
    @collection = new Queries.Collections.QueryFiles()
    d = $.parseJSON($('.query_files').data('query-files'))
    dq = _.map d, (qf) -> qf.file
    @collection.reset dq

  new: ->
    view = new Queries.Views.QueryFilesIndex(collection: @collection)
    $('.query_files').html(view.render().el)
    # @collection.reset 

  edit: (id) ->
    alert "Editing id #{id}"
