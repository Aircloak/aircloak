defmodule Air.ViewHelpers do
  @moduledoc "Common helper functions for views."

  import Phoenix.HTML.Tag, only: [content_tag: 3]
  import Phoenix.HTML.Link, only: [link: 2]

  @doc "Verifies if the currently logged-in user has permissions on the given action."
  @spec permitted?(Plug.Conn.t, module, atom) :: boolean
  def permitted?(conn, controller, action) do
    case Air.VerifyPermissions.check_permission(conn, controller, action) do
      :ok -> true
      {:error, _formatted_error} -> false
    end
  end

  @doc "Returns true if the currently logged-in user is an administrator."
  @spec admin?(Plug.Conn.t | Air.Schemas.User.t) :: boolean
  def admin?(nil), do: false
  def admin?(%Air.Schemas.User{} = user), do: Air.Schemas.User.admin?(user)
  def admin?(%Plug.Conn{} = conn), do: admin?(conn.assigns.current_user)

  @doc """
  Generates a navbar link, and highlights the active one
  """
  @spec navbar_link(Plug.Conn.t, String.t, String.t, Keyword.t) :: {:safe, [any]}
  def navbar_link(%{request_path: request_path}, name, desired_path, options \\ []) do
    content_tag(:li, role: "presentation", class: active_class(request_path, desired_path)) do
      link(name, to: desired_path, target: Keyword.get(options, :target, "_self"))
    end
  end

  @doc "Returns an embeddable json representing selectable tables and views."
  @spec selectables(Plug.Conn.t, Air.Schema.DataSource.t, [Air.Schema.View.t]) :: {:safe, iodata}
  def selectables(conn, data_source, views) do
    Air.Schemas.DataSource.tables(data_source)
    |> Kernel.++(
          Enum.map(views, &%{
            "id" => &1.name,
            "broken" => &1.broken,
            "columns" => Map.fetch!(&1.result_info, "columns"),
            "edit_link" => Air.Router.Helpers.data_source_view_path(conn, :edit, data_source.name, &1.id),
            "delete_html" =>
              Phoenix.HTML.safe_to_string(link("delete",
                to: Air.Router.Helpers.data_source_view_path(conn, :delete, data_source.name, &1.id),
                method: :delete,
                "data-confirm": "Delete #{&1.name}?",
                class: "btn btn-danger btn-xs"
              ))
          })
        )
    |> Enum.sort_by(&Map.fetch(&1, "id"))
    |> to_json()
  end

  @doc "Encodes the given term to json which can be safely embedded in .eex templates."
  @spec to_json(any) :: {:safe, iodata}
  def to_json(term) do
    {:safe, Poison.encode!(term)}
  end

  defp active_class(path, "/admin/activity_monitor") when path in ["/admin", "/admin/"], do: "active"
  defp active_class("/admin/queries/" <> _, "/admin/activity_monitor"), do: "active"
  defp active_class(request_path, link_path) do
    if String.starts_with?(request_path, link_path) do
      "active"
    else
      nil
    end
  end

  def logged_in?(conn) do
    conn.assigns.current_user != nil
  end

  def expiry_date(), do: Date.to_string(Air.Service.Version.expiry_date())

  def expiry_message(), do: expiry_message_for_status(Air.Service.Version.expiry_status())

  defp expiry_message_for_status(:will_expire), do:
    """
    Please note that the Aircloak version you are using expires on #{expiry_date()}.
    Please update to a newer version.
    """
  defp expiry_message_for_status(:expires_shortly), do:
    """
    The version of Aircloak you are using expires in #{Air.Service.Version.days_until_expiry()} days.
    Please update to a newer version. Not updating in time will cause disruption to service.
    """
  defp expiry_message_for_status(:imminent), do:
    """
    Your Aircloak expires in #{Air.Service.Version.days_until_expiry()} days!
    Please update to the latest version.
    Failing to do so will leave you without the ability to query your data sources.
    """
end
