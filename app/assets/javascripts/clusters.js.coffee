$(document).ready ->

  previousBuild = document.getElementById("cluster_build_id").value

  window.build_change = ->
    return unless previousBuild

    if confirm("Are you sure you want to take the cluster offline and upgrade it with a new build?")
      previousBuild = null
    else
      document.getElementById("cluster_build_id").value = previousBuild