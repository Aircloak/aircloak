$(document).ready ->
  activeTip = undefined

  $("body .container, div#popup.popup").popover
    animation: true
    html: true
    selector: ".tip"
    trigger: "click"

  # If a click is received while there is an active popover,
  # and the click is _not_ on the active popover, _nor_ on the
  # popover link itself, then we want to remove the popover.
  #
  # We suppress click actions on popover links as they
  # have already been handled by the click handler of the
  # popover link. The reason we are still receiving the event
  # here is that we had to let it bubble up from the
  # tip click handler in order to allow the bootstrap js
  # to take over and present the popover.
  $("body").click (e) ->
    return if e.target == activeTip
    return unless $(e.target).closest(".popover-content").length == 0
    removeActiveTip(activeTip) if activeTip

  # If there already is a different popover in place, we
  # remove it first. Otherwise we ensure the activeTip is
  # set to the tip that was clicked, and let the bootstrap
  # js popover handler deal with displaying the new popover
  window.rebindAllPopuplinks = =>
    $(".tip").unbind("click").click (e) =>
      tip = e.target
      if tip == activeTip
        activeTip = undefined
      else
        removeActiveTip activeTip
        activeTip = tip

  removeActiveTip = (tip) ->
    $(tip).popover("hide").removeClass("active")
    activeTip = undefined

  window.rebindAllPopuplinks()
