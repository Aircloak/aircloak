# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

# create namespace for task-related shared variables
window.Task = {}
Task.statusVisible = false

show_task_success = ->
  return if Task.resultsHaveArrived
  $('#task_status').html "> Execution of task has been initiated."
  Task.statusVisible = true

show_task_progress = ->
  $('#task_status').html "> Initiating execution of task."
  Task.statusVisible = true

show_task_error = ->
  $('#task_status').html "> Failed to initiate task execution."
  Task.statusVisible = true

hide_task_status = ->
  $('#task_status').html ">"
  Task.statusVisible = false

Task.execute = (id) ->
  # reset task status and timeout
  hide_task_status()
  if Task.hideTimeout?
    clearTimeout Task.hideTimeout
    delete Task.hideTimeout
  # invoke task execution
  Task.resultsHaveArrived = false
  show_task_progress()
  $.ajax "/tasks/#{id}/execute_as_batch_task.json",
    type: 'POST'
    error: (jqXHR, textStatus, errorThrown) ->
      show_task_error()
    success: (data, textStatus, jqXHR) ->
      show_task_success()

convert_article_to_result = (timestamp, article) ->
  result = {published_at: timestamp}
  article = JSON.parse article
  result.buckets = []
  for bucket in article.buckets
    name = bucket.label
    name += ": #{bucket.value}" if bucket.value
    result.buckets.push {name: name, value: bucket.count}
  result.exceptions = []
  for exception in article.exceptions
    result.exceptions.push {count: exception.count}
  result

$ ->
  Results.resultsTableLimit = 10 # show maximum 10 results in the table

  # callback for processing listen events
  airpub_callback = (object) ->
    if object.type == "article" && Results.last_article_update < object.published_at
      Task.resultsHaveArrived = true
      if Task.statusVisible
        # hide status after 4 seconds from the arrival of the result
        Task.hideTimeout = setTimeout hide_task_status, 3000
      Results.display convert_article_to_result object.published_at, object.content

  Results.ws = airpub_listen $('.listen_params').data('server-url'), $('.listen_params').data('request'), airpub_callback
