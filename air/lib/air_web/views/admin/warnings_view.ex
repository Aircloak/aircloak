defmodule AirWeb.Admin.WarningsView do
  @moduledoc false
  use Air.Web, :view
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Schemas.DataSource

  defp type(:license), do: "License"
  defp type(:privacy_policy), do: ""
  defp type(%DataSource{}), do: "Data source"

  defp name(:license), do: ""
  defp name(:privacy_policy), do: "Privacy policy"
  defp name(%DataSource{} = resource), do: resource.name

  defp resource_link(_conn, :license), do: ""

  defp resource_link(conn, :privacy_policy), do: link("More", to: admin_privacy_policy_path(conn, :new))

  defp resource_link(conn, %DataSource{} = resource),
    do: link("More", to: admin_data_source_path(conn, :show, resource.name))

  defp severity(type),
    do:
      type
      |> Atom.to_string()
      |> String.capitalize()
end
