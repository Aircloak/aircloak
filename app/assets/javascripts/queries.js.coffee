//= require angular
//= require jquery-fileupload/basic
//= require jquery-fileupload/vendor/tmpl

@app = angular.module("queryEditor", [])

filterOutScheduledForRemoval = (items) ->
  _.filter items, (item) -> !!!item.scheduleRemove

@app.filter "unlessScheduledForRemoval", () ->
  (items) ->
    filterOutScheduledForRemoval(items)

@app.directive "indices", -> 
  restrict: "E"
  templateUrl: "index_chooser.html"
  scope: 
    availableIndices: "=",
    queryFile: "="
  link: (scope) ->

    scope.addIndex = ->
      i = scope.selectedIndex
      return unless i
      if i.fresh
        i.count += 1
      scope.addIndexToQueryFile i
      scope.selectedIndex = undefined

    scope.removeIndex = (i) ->
      if i.fresh
        i.count -= 1
        if i.count == 0
          scope.availableIndices = _.without(scope.availableIndices, i)
      scope.queryFile.indices = _.without(scope.queryFile.indices, i)

    scope.notInIndicesForQueryFile = (i) ->
      _.filter i, (e) ->
        _.indexOf(scope.queryFile.indices, e) == -1

    scope.newIndex = (event) ->
      i = 
        name: scope.name
        human_name: scope.human_name
        system_index: scope.system_index
        fresh: true
        count: 1
      scope.addIndexToQueryFile i
      scope.name = ""
      scope.human_name = ""
      scope.system_index = false
      scope.availableIndices.push i

    scope.addIndexToQueryFile = (i) ->
      scope.queryFile.indices = [] if scope.queryFile.indices == undefined
      scope.queryFile.indices.push i


@QueryEditorCtrl = ($scope, $window) ->
  $scope.data = {}
  $scope.data.query_files = []
  $scope.temp = undefined

  # This is a little nasty...
  # We have to be able to add results 
  # from outside the scope of angular.
  # In order to do this, we have to temporarily
  # buffer the value, and then apply it
  # within the angular scope in order to get 
  # angular to pick up the change.
  $window.addQueryFile = (result) =>
    result.fresh = true
    $scope.temp = result
    $scope.$apply("appendQueryFile()")

  $scope.appendQueryFile = ->
    $scope.data.query_files.push $scope.temp
    $scope.temp = undefined

  $scope.numberOfMainClasses = ->
    f = (count, q) ->
      if q.query_interface
        count + 1
      else
        count
    _.reduce(filterOutScheduledForRemoval($scope.data.query_files), f, 0)

  $scope.tooManyMainClasses = ->
    $scope.numberOfMainClasses() > 1

  $scope.removeQueryFile = (queryFile) ->
    queryFile.scheduleRemove = true
    console.log queryFile


# Dealing with file uploads
activate_fileupload = ->
  $("#query_files_form").fileupload
    dataType: "json"
    add: (e, data) ->
      console.log "Adding file for uploading"
      types = /(\.|\/)class$/i
      file = data.files[0]
      if types.test(file.type) || types.test(file.name)
        data.context = $(tmpl("template-upload", data.files[0]).trim())
        $('.query_files_upload').append(data.context)
        data.submit()
      else
        alert("#{file.name} is not a class file")
    progress: (e, data) ->
      if data.context
        progress = parseInt(data.loaded / data.total * 100, 10)
        data.context.find('.bar').css('width', progress + "%")
    fail: (e, data) ->
      console.log "Upload failed: e: #{e}, data: #{data}"
    done: (e, data) ->
      console.log "File uploaded"
      addQueryFile data.result
      data.context.remove()

$(document).ready(activate_fileupload)
$(document).on('page:load', activate_fileupload)
