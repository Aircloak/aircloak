# Setup the global namespace
window.TableStats = (server_url, request, table_stats) ->
  self = this

  # ------------------------------------
  # Private members
  # ------------------------------------

  refreshLink = table_stats.refresh_link

  render = () ->
    $("#stats_element").html(HandlebarsTemplates['user_tables/table_stats'](table_stats))

    if table_stats.calculating
      $("#refresh_table_stats").addClass("disabled")
      $("#refresh_table_stats").text("calculating statistics")
    else
      if table_stats.success == true
        $("#stats_message").removeClass("error-text").html("")
      else
        $("#stats_message").
          addClass("error-text").
          html("An error occurred while computing table statistics. The presented information is possibly incorrect.")

  refreshTableStats = (e) ->
    e.preventDefault()
    $.ajax(
          type: "POST",
          url: $(e.target).attr("href"),
          processData: false
        )
    false

  onStatsPublished = (object) ->
    if (object.type == "article")
      message = JSON.parse(object.content)
      switch message.type
        when "table_stats"
          table_stats = message.data
          table_stats.refresh_link = refreshLink
          table_stats.calculating = false
          table_stats.success = true
          table_stats.stats = true
          render()
        when "calculating"
          table_stats.calculating = true
          render()
        when "error"
          table_stats.calculating = false
          table_stats.success = false
          render()


  # ------------------------------------
  # Constructor
  # ------------------------------------

  render()

  view = new Backbone.View({
    el: "#stats_element",
    render: render,
    events:
      "click #refresh_table_stats": refreshTableStats
  })

  airpub_listen(server_url, request, onStatsPublished)

  _.extend(self, {})
