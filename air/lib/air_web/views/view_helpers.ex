defmodule AirWeb.ViewHelpers do
  @moduledoc "Common helper functions for views."

  import Phoenix.HTML.Tag, only: [content_tag: 3]
  import Phoenix.HTML.Link, only: [link: 2]

  alias Air.Service.Warnings

  @doc "Produces all the necessary info for setting up the selectables sidebar as a JSON string."
  @spec selectable_setup(Plug.Conn.t()) :: {:safe, iodata}
  def selectable_setup(conn) do
    %{
      data_source: data_source,
      selectables: selectables,
      current_user: current_user,
      number_format: number_format
    } = conn.assigns

    to_json(%{
      selectables: Enum.sort_by(selectables, & &1.id),
      selectablesEditUrl: AirWeb.Router.Helpers.data_source_selectable_path(conn, :edit, data_source.name),
      newTableURL: AirWeb.Router.Helpers.data_source_selectable_path(conn, :new, data_source.name, :analyst_table),
      newViewURL: AirWeb.Router.Helpers.data_source_selectable_path(conn, :new, data_source.name, :view),
      userId: current_user.id,
      dataSourceName: data_source.name,
      dataSourceDescription: data_source.description,
      dataSourceStatus: Air.Service.DataSource.status(data_source),
      socketToken: AirWeb.Plug.Session.current_token(conn),
      browserSocketTransport: Air.browser_socket_transport(),
      supportsCreateTable: data_source.supports_analyst_tables,
      selectableToExclude: conn.assigns[:view_to_exclude_from_selectables],
      numberFormat: number_format
    })
  end

  @doc "Verifies if the currently logged-in user has permissions on the given action."
  @spec permitted?(Plug.Conn.t(), module, atom) :: boolean
  def permitted?(conn, controller, action) do
    case AirWeb.VerifyPermissions.check_permission(conn, controller, action) do
      :ok -> true
      {:error, _formatted_error} -> false
    end
  end

  @doc "True if audit logging is enabled"
  @spec audit_log_enabled?() :: boolean
  def audit_log_enabled?(), do: Air.Service.Settings.read().audit_log_enabled

  @doc "Returns true if the currently logged-in user is an administrator."
  @spec admin?(nil | Plug.Conn.t() | Air.Schemas.User.t()) :: boolean
  def admin?(nil), do: false
  def admin?(%Air.Schemas.User{} = user), do: Air.Schemas.User.admin?(user)
  def admin?(%Plug.Conn{} = conn), do: admin?(conn.assigns.current_user)

  @doc "Returns an embeddable json representing selectable tables, views, and analyst created tables."
  @spec order_by_id([Map.t()]) :: [Map.t()]
  def order_by_id(items), do: Enum.sort_by(items, & &1.id)

  @doc "Encodes the given term to json which can be safely embedded in .eex templates."
  @spec to_json(any) :: {:safe, iodata}
  def to_json(term) do
    {:safe, Jason.encode!(term)}
  end

  @doc "Conditionally creates a navbar link if there are warnings"
  @spec warning_navbar_link(Plug.Conn.t()) :: {:safe, [any]}
  def warning_navbar_link(conn) do
    problems = Warnings.problems()

    if admin?(conn) do
      if length(problems) > 0 do
        path = AirWeb.Router.Helpers.admin_warnings_path(conn, :index)
        navbar_class = problems |> Warnings.highest_severity_class() |> severity_class()
        navbar_link(conn, admin_title(length(problems), navbar_class), path)
      else
        navbar_link(conn, "Admin", "/admin")
      end
    else
      {:safe, []}
    end
  end

  @doc "Conditionally creates a sidebar link if there are warnings"
  @spec warning_sidebar_link(Plug.Conn.t()) :: {:safe, [any]}
  def warning_sidebar_link(conn) do
    problems = Warnings.problems()

    if length(problems) > 0 and admin?(conn) do
      path = AirWeb.Router.Helpers.admin_warnings_path(conn, :index)
      navbar_class = problems |> Warnings.highest_severity_class() |> severity_class()
      sidebar_link(conn, warnings_title(length(problems), navbar_class), "exclamation-triangle", path)
    else
      {:safe, []}
    end
  end

  @doc "Warnings title"
  @spec admin_title(non_neg_integer, String.t()) :: [any]
  def admin_title(num_problems, severity_class),
    do: [
      "Admin ",
      content_tag(:span, [class: "badge badge-" <> severity_class], do: num_problems)
    ]

  @doc "Warnings title"
  @spec warnings_title(non_neg_integer, String.t()) :: [any]
  def warnings_title(num_problems, severity_class),
    do: [
      Inflex.inflect("Warning", num_problems),
      " ",
      content_tag(:span, [class: "badge badge-" <> severity_class], do: num_problems)
    ]

  @doc """
  Generates a dropdown link, and highlights the active one
  """
  @spec dropdown_link(Plug.Conn.t(), any, String.t(), Keyword.t()) :: {:safe, [any]}
  def dropdown_link(%{request_path: request_path}, name, desired_path, options \\ []) do
    active = active_class(request_path, desired_path)
    options = [{:class, "dropdown-item #{active}"} | [{:to, desired_path} | options]]
    link(name, options)
  end

  @doc """
  Generates a navbar link, and highlights the active one
  """
  @spec navbar_link(Plug.Conn.t(), any, String.t(), Keyword.t()) :: {:safe, [any]}
  def navbar_link(%{request_path: request_path}, name, desired_path, options \\ []) do
    content_tag(
      :li,
      role: "presentation",
      class: add_active_class("nav-item", request_path, desired_path, options)
    ) do
      options =
        Keyword.put(
          [{:to, desired_path} | options],
          :class,
          add_active_class("nav-link", request_path, desired_path, [])
        )

      link(name, options)
    end
  end

  @doc """
  Generates a link with an icon, highlighting the active one.
  """
  @spec sidebar_link(Plug.Conn.t(), any, String.t(), String.t(), Keyword.t()) :: {:safe, [any]}
  def sidebar_link(conn, name, icon, path, options \\ []) do
    navbar_link(conn, [content_tag(:i, "", class: "fas fa-#{icon}"), name], path, options)
  end

  defp add_active_class(base_class, request_path, desired_path, options) do
    active = active_class(request_path, desired_path)
    provided = Keyword.get(options, :class)

    [active, provided, base_class]
    |> Enum.join(" ")
    |> String.trim()
  end

  defp active_class(path, "/admin/system_status") when path in ["/admin", "/admin/"],
    do: "active"

  defp active_class("/admin/queries/failed" <> _, "/admin/system_status"), do: nil
  defp active_class("/admin/queries/performance" <> _, "/admin/system_status"), do: nil
  defp active_class("/admin/queries/" <> _, "/admin/system_status"), do: "active"
  defp active_class("/settings/" <> _, "/settings"), do: nil

  defp active_class(request_path, link_path) do
    if String.starts_with?(request_path, link_path) do
      "active"
    else
      nil
    end
  end

  def logged_in?(conn), do: AirWeb.Plug.Session.authenticated?(conn)

  def severity_class(:high), do: "danger"
  def severity_class(:medium), do: "warning"
  def severity_class(_), do: ""
end
