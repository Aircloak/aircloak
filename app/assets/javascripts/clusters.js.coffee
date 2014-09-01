$(document).ready ->

  previousBuild = document.getElementById("cluster_build_id").value

  window.build_change = ->
    return unless previousBuild

    if confirm("Changing a cluster's build will result in that cluster being\n" +
               "taken offline and having all of it's cloaks re-imagined with\n" +
               "the new build.\nAre you sure you want to continue?")
      previousBuild = null
    else
      document.getElementById("cluster_build_id").value = previousBuild