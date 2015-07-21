# load the progress bar template
progressBarTemplate = HandlebarsTemplates['latest_results/progress_bar']

# pending executions handling
showPendingExecutions = (data) ->
  if data?.success is true
    if data.reports.length isnt 0
      $('#progress_indicator').html progressBarTemplate data
    else
      $('#progress_indicator').html progressBarTemplate
        reports: [
          label: "No pending execution"
          progress: 0
        ]
  else
    $('#progress_indicator').html ""
    clearInterval pendingCallbackInterval

pendingCallback = =>
  jQuery.getJSON "/tasks/#{$('.task_params').data('task-token')}/pending_executions", showPendingExecutions

pendingCallbackInterval = setInterval pendingCallback, 1000
showPendingExecutions { success: true, reports: [] } # to display pending executions directly

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
  limit = 8 * 1024 * 1024 # 8 MB size limit
  if article.length > limit
    result.buckets = [{label: "notice", value: "result too big", \
                       count: "result size (#{article.length} bytes) exceededs limit (#{limit} bytes)"}]
    result.exceptions = []
  else
    article = JSON.parse article
    result.buckets = []
    result.buckets.push {label: bucket.label, value: bucket.value, count: bucket.count} for bucket in article.buckets
    result.exceptions = article.exceptions
  result


$ ->
  Results.resultsTableLimit = 10 # show maximum 10 results in the table

  # callback for processing listen events
  airpubCallback = (object) ->
    if object.type == "article" && Results.last_article_update < object.published_at
      Task.resultsHaveArrived = true
      if Task.statusVisible
        # hide status after 4 seconds from the arrival of the result
        Task.hideTimeout = setTimeout hideTaskStatus, 3000
      result = convertArticleToResult object.published_at, object.content
      Results.display result

  Results.ws = airpub_listen $('.listen_params').data('server-url'), $('.listen_params').data('request'), airpubCallback
