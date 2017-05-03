defmodule Air.Admin.WarningsView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Schemas.DataSource

  defp type(%DataSource{}), do: "Data source"

  defp name(%DataSource{} = resource), do: resource.name

  defp resource_link(conn, %DataSource{} = resource), do:
    link_to(admin_data_source_path(conn, :show, resource.name))

  defp link_to(path), do: link("More", to: path)
end
