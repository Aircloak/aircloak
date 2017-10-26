defmodule Air.Admin.WarningsView do
  @moduledoc false
  use Air.Web, :view
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Schemas.DataSource

  defp type(%DataSource{}), do: "Data source"

  defp name(%DataSource{} = resource), do: resource.name

  defp resource_link(conn, %DataSource{} = resource), do:
    link("More", to: admin_data_source_path(conn, :show, resource.name))

  defp severity(type), do:
    type
    |> Atom.to_string()
    |> String.capitalize()
end
