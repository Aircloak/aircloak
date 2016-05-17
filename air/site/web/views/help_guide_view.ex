defmodule Air.HelpGuideView do
  @moduledoc false
  use Air.Web, :view

  for {page, title} <- %{
    getting_started: "Getting started",
    writing_tasks: "Writing tasks"
  } do
    def page_title(unquote(page)), do: unquote(title)
  end
end
