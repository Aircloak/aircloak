$(document).ready ->

  previousBuild = document.getElementById("cluster_build_id").value

  window.build_change = () ->
    return unless previousBuild

    if confirm("Are you sure you want to take the cluster offline and upgrade it with a new build?")
      previousBuild = null
    else
      document.getElementById("cluster_build_id").value = previousBuild

  window.analyst_change = (checkbox) ->
    if not checkbox.checked
      if not confirm "Are you sure you want to remove this analyst from the cluster? " +
          "All the analyst's data will also be removed from the cluster. This operation can NOT be reversed."
        checkbox.checked = true

