escClosePopup = (e) ->
  if (e.keyCode == 27)
    window.Popup.close()

window.Popup =
  show: (html) ->
    $("#overlay").show()
    $("#popup").html(html).show().center()
    $("body").on('keyup', escClosePopup)

  close: ->
    $("#overlay").hide()
    $("#popup").html("").hide()
    $("body").off('keyup', escClosePopup)

jQuery.fn.center = ->
  @.
    css("position","absolute").
    css("top", Math.max(0, (($(window).height() - @.outerHeight()) / 2) + $(window).scrollTop()) + "px").
    css("left", Math.max(0, (($(window).width() - @.outerWidth()) / 2) + $(window).scrollLeft()) + "px")