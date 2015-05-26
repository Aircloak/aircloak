# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

# pending executions handling
showPendingExecutions = (data) ->
  if data?.success is true
    progress_string = ""
    progress_string += "<li>#{progress.time}: finished ~#{progress.progress}%</li>" for progress in data.progress
    if progress_string != ""
      $('#progress_indicator').html "<h4>Pending task executions</h4><ul>#{progress_string}</ul>"
    else
      $('#progress_indicator').html ""
  else
    $('#progress_indicator').html ""

pendingCallback = =>
  jQuery.getJSON "/tasks/#{$('.task_params').data('task-token')}/pending_executions", showPendingExecutions

setInterval pendingCallback, 1000

# create namespace for task-related shared variables
window.Task = {}
Task.statusVisible = false

showTaskSuccess = ->
  return if Task.resultsHaveArrived
  $('#task_status').html "> Execution of task has been initiated."
  Task.statusVisible = true

showTaskProgress = ->
  $('#task_status').html "> Initiating execution of task."
  Task.statusVisible = true

showTaskError = ->
  $('#task_status').html "> Failed to initiate task execution."
  Task.statusVisible = true

hideTaskStatus = ->
  $('#task_status').html ">"
  Task.statusVisible = false

Task.execute = (id) ->
  # reset task status and timeout
  hideTaskStatus()
  if Task.hideTimeout?
    clearTimeout Task.hideTimeout
    delete Task.hideTimeout
  Task.resultsHaveArrived = false
  # invoke task execution
  showTaskProgress()
  $.ajax "/tasks/#{id}/execute_as_batch_task.json",
    type: 'POST'
    error: (jqXHR, textStatus, errorThrown) ->
      showTaskError()
    success: (data, textStatus, jqXHR) ->
      showTaskSuccess()

convertArticleToResult = (timestamp, article) ->
  result = {published_at: timestamp}
  article = JSON.parse article
  result.buckets = []
  result.buckets.push {label: bucket.label, value: bucket.value, count: bucket.count} for bucket in article.buckets
  result.exceptions = []
  result.exceptions.push {count: exception.count} for exception in article.exceptions
  result

requiresPostprocessing = (results) ->
  _.chain(results.buckets)
    .filter((bucket) -> bucket.label == "ac_postprocessing")
    .value()
    .length > 0

$ ->
  Results.resultsTableLimit = 10 # show maximum 10 results in the table

  # callback for processing listen events
  airpubCallback = (object) ->
    if object.type == "article" && Results.last_article_update < object.published_at
      Task.resultsHaveArrived = true
      if Task.statusVisible
        # hide status after 4 seconds from the arrival of the result
        Task.hideTimeout = setTimeout hideTaskStatus, 3000
      results = convertArticleToResult object.published_at, object.content
      if requiresPostprocessing(results)
        window.refreshGraphsFromServer(object.published_at, Results.display)
      else
        Results.display results

  Results.ws = airpub_listen $('.listen_params').data('server-url'), $('.listen_params').data('request'), airpubCallback
