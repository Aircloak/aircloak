defmodule Air.LayoutView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  defp container_class(assigns) do
    if assigns[:full_width] do
      "container-fluid"
    else
      "container"
    end
  end

  defp help_links(conn) do
    case Air.Plug.HelpPages.help_pages(conn) do
      [] -> ""
      pages ->
        content_tag(:div, class: "container-fluid text-right help-links") do
          [
            content_tag(:span, "Consider reading", class: "label label-info"),
            " ",
            help_links(conn, pages)
          ]
        end
    end
  end

  defp help_links(conn, pages) do
    Stream.map(pages, fn(page) ->
      link(
        Air.HelpGuideView.page_title(page),
        to: help_guide_path(conn, :article, page),
        class: "suggested-reading"
      )
    end)
    |> Enum.reduce([],
          fn
            (link, []) -> link
            (link, acc) -> [acc, ", ", link]
          end
        )
  end
end
