defmodule AirWeb.ViewHelpers do
  @moduledoc "Common helper functions for views."

  import Phoenix.HTML.Tag, only: [content_tag: 3]
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Service.Warnings
  alias Air.{Schemas, Service}

  @doc "Verifies if the currently logged-in user has permissions on the given action."
  @spec permitted?(Plug.Conn.t, module, atom) :: boolean
  def permitted?(conn, controller, action) do
    case AirWeb.VerifyPermissions.check_permission(conn, controller, action) do
      :ok -> true
      {:error, _formatted_error} -> false
    end
  end

  @doc "True if audit logging is enabled"
  @spec audit_log_enabled?() :: boolean
  def audit_log_enabled?(), do:
    Air.Service.Settings.read().audit_log_enabled

  @doc "Returns true if the currently logged-in user is an administrator."
  @spec admin?(Plug.Conn.t | Air.Schemas.User.t) :: boolean
  def admin?(nil), do: false
  def admin?(%Air.Schemas.User{} = user), do: Air.Schemas.User.admin?(user)
  def admin?(%Plug.Conn{} = conn), do: admin?(conn.assigns.current_user)

  @doc "Returns an embeddable json representing selectable tables and views."
  @spec selectables(Plug.Conn.t, Schemas.DataSource.t, non_neg_integer | nil) :: [Map.t]
  def selectables(conn, data_source, view_to_exclude \\ nil) do
    view_to_exclude = case view_to_exclude do
      nil -> :do_not_exclude_any
      id -> id
    end

    Service.DataSource.views_and_tables(conn.assigns[:current_user], data_source)
    |> Enum.reject(& &1.internal_id == view_to_exclude)
    |> Enum.map(fn(table) ->
      if table.view do
        additional_data = %{
          edit_link: AirWeb.Router.Helpers.data_source_view_path(conn, :edit, data_source.name,
            table.internal_id),
          delete_html: Phoenix.HTML.safe_to_string(link("delete",
            to: AirWeb.Router.Helpers.data_source_view_path(conn, :delete, data_source.name,
              table.internal_id),
            method: :delete,
            "data-confirm": "Delete #{table.id}?",
            class: "btn btn-danger btn-xs"
          )),
        }
        Map.merge(table, additional_data)
      else
        table
      end
    end)
    |> Enum.sort_by(& &1.id)
  end

  @doc "Encodes the given term to json which can be safely embedded in .eex templates."
  @spec to_json(any) :: {:safe, iodata}
  def to_json(term) do
    {:safe, Poison.encode!(term)}
  end

  @doc "Conditionally creates a navbar link if there are warnings"
  @spec warning_navbar_link(Plug.Conn.t) :: {:safe, [any]}
  def warning_navbar_link(conn) do
    problems = Warnings.problems()
    if length(problems) > 0 and admin?(conn) do
      path = AirWeb.Router.Helpers.admin_warnings_path(conn, :index)
      navbar_class = problems |> Warnings.highest_severity_class() |> severity_class()
      navbar_link(conn, warnings_title(length(problems)), path, class: navbar_class)
    else
      {:safe, []}
    end
  end

  @doc "Warnings title"
  @spec warnings_title(non_neg_integer) :: [any]
  def warnings_title(num_problems), do:
    [content_tag(:span, [class: "badge"], do: num_problems), " ", Inflex.inflect("Warning", num_problems)]

  @doc """
  Generates a navbar link, and highlights the active one
  """
  @spec navbar_link(Plug.Conn.t, any, String.t, Keyword.t) :: {:safe, [any]}
  def navbar_link(%{request_path: request_path}, name, desired_path, options \\ []) do
    content_tag(:li, role: "presentation", class: navbar_link_classes(request_path, desired_path, options)) do
      options = [{:to, desired_path} | options]
      link(name, options)
    end
  end

  defp navbar_link_classes(request_path, desired_path, options) do
    active = active_class(request_path, desired_path)
    provided = Keyword.get(options, :class)
    [active, provided]
    |> Enum.join(" ")
    |> String.trim()
    |> case do
      "" -> nil
      content -> content
    end
  end

  defp active_class(path, "/admin/activity_monitor") when path in ["/admin", "/admin/"], do: "active"
  defp active_class("/admin/queries/failed" <> _, "/admin/activity_monitor"), do: nil
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

  def severity_class(:high), do: "danger"
  def severity_class(:medium), do: "warning"
  def severity_class(_), do: ""
end
