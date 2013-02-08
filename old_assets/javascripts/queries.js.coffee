//= require jquery-fileupload/basic
//= require jquery-fileupload/vendor/tmpl

# Dealing with file uploads
jQuery ->
  $("#query_files_form").fileupload
    dataType: "json"
    add: (e, data) ->
      types = /(\.|\/)class$/i
      file = data.files[0]
      if types.test(file.type) || types.test(file.name)
        data.context = $(tmpl("template-upload", data.files[0]).trim())
        $('.query_files').append(data.context)
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
      window.QueryFileView.trigger 'new_query_file', data.result
      data.context.remove()

# We should only make the user add an identifier if he
# has marked that the query is an update query, as in 
# one that is run on new updates coming from the client
$(document).on 'click', '#query_update_query', (e) ->
  box = $("#query_identifier")[0]
  box.disabled = !e.target.checked
  true

# This is some ui "magic" that warsn the user if it really
# is the right choice to make the query a mutator
check_possibility_of_mutating_files = ->
  g = $("#query_type_box.control-group:first")
  imi = $(".danger-mutate-info:first")
  isi = $(".danger-mutate-shared-info:first")
  showAlert = false
  if $("#query_mutator_true:checked").size() > 0 and $("#query_update_query:checked").size() == 0
    showAlert = true
    imi.removeClass("hidden")
  else
    imi.addClass("hidden")

  if $("#query_system_query:checked").size() > 0 and $("#query_mutator_true:checked").size() > 0
    showAlert = true
    isi.removeClass("hidden")
  else
    isi.addClass("hidden")

  if showAlert
    g.addClass("alert")
  else
    g.removeClass("alert")

  true

$(document).on 'click', 'input[type=radio]', ->
  check_possibility_of_mutating_files()

$(document).on 'change', '.system-query-group', ->
  check_possibility_of_mutating_files()

$(document).on 'change', '.update-query-group', ->
  check_possibility_of_mutating_files()

$(document).on 'click', '.remove_index', (event) ->
  $(this).prev('input[type=hidden]').val('1')
  $(this).closest('fieldset').hide()
  event.preventDefault()

$(document).on 'click', '.add-existing-index', (event) ->
  event.preventDefault()
  link = $(event.target)[0]
  id = link.getAttribute('id')
  select = $(link).prev("select")[0]
  option = select.selectedOptions[0]
  index = $.parseJSON(option.getAttribute('data')).index
  $(option).remove()
  if select.options.length == 0
    $(link).addClass("disabled")
  $("#add-new-index-row").before(tmpl("template-index-#{id}", index))

$(document).on 'click', '.add-new-index', (event) ->
  event.preventDefault()
  time = new Date().getTime()
  regexp = new RegExp($(this).data('id'), 'g')
  $(this).closest('tr').before($(this).data('fields').replace(regexp, time))

$(document).on 'click', '.remove-index', (event) ->
  event.preventDefault()
  $(this).prev('input[type=hidden]').val('1')
  $(this).closest('tr').fadeOut()

$(document).on 'click', '.remove-index', (event) ->
  event.preventDefault()
  $(this).prev('input[type=hidden]').val('1')
  $(this).closest('tr').fadeOut()
