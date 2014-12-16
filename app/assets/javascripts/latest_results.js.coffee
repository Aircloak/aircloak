# Place all the behaviors and hooks related to the matching controller here.
# All this logic will automatically be available in application.js.
# You can use CoffeeScript in this file: http://coffeescript.org/

show_task_success = () ->
  status = document.getElementById 'task_status'
  status.innerHTML = "> Execution of task has been initiated."
  window.taskStatusVisible = true

show_task_error = () ->
  status = document.getElementById 'task_status'
  status.innerHTML = "> Failed to initiate task execution."
  window.taskStatusVisible = true

hide_task_status = () ->
  status = document.getElementById 'task_status'
  status.innerHTML = ">"
  window.taskStatusVisible = false

window.execute_task = (id) ->
  # reset task status and timeout
  hide_task_status()
  if hideTimeout?
    clearTimeout hideTimeout
    window.hideTimeout = null
  # invoke task execution
  url = "/api/tasks/" + id + "/execute_as_batch_task"
  http = new XMLHttpRequest()
  http.open "POST", url, true
  http.onreadystatechange = () ->
    if http.readyState == http.DONE
      if http.status == 200
        show_task_success()
      else
        show_task_error()
        console.log "Execute batch task ended with status: " + http.status
  http.send()

convert_article_to_result = (timestamp, article) ->
  result = {published_at: timestamp}
  console.log "New result: " + article
  article = JSON.parse article
  result.buckets = []
  for bucket in article.buckets
    name = bucket.label
    name += ":" + bucket.value if bucket.value
    result.buckets.push {name: name, value: bucket.count}
  result.exceptions = []
  for exception in article.exceptions
    result.exception.push {count: exception.count}
  result

$ ->
  window.resultsTableLimit = 10 # show maximum 10 results in the table

  # callback for processing listen events
  airpub_callback = (object) ->
    if object.type == "article" && last_article_update < object.published_at
      if taskStatusVisible
        # hide status after 4 seconds from the arrival of the result
        window.hideTimeout = setTimeout hide_task_status, 3000
      result = convert_article_to_result object.published_at, object.content
      display_result result

  window.ws = airpub_listen $('.listen_params').data('server-url'), $('.listen_params').data('request'), airpub_callback
