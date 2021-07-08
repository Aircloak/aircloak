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

  @doc "Create a link to the admin section, conditionally with a badge showing the number of warnings and errors."
  @spec admin_navbar_link(Plug.Conn.t()) :: {:safe, [any]}
  def admin_navbar_link(conn) do
    if admin?(conn) do
      problems = Warnings.problems()

      link_title =
        if Enum.empty?(problems) do
          "Admin"
        else
          [
            "Admin ",
            warning_badge(problems)
          ]
        end

      navbar_link(conn, link_title, AirWeb.Router.Helpers.admin_system_status_path(conn, :index))
    else
      {:safe, []}
    end
  end

  @doc "Returns a warning severity badge for a set of problems"
  @spec warning_badge([Warnings.problem()]) :: {:safe, [any]}
  def warning_badge(problems) do
    num_problems = Enum.count(problems)
    severity_class = problems |> Warnings.highest_severity_class() |> severity_class()
    content_tag(:span, [class: "badge badge-" <> severity_class], do: num_problems)
  end

  @doc "Creates a system status side bar link conditionally with a warning indicator"
  @spec system_status_sidebar_link(Plug.Conn.t()) :: {:safe, [any]}
  def system_status_sidebar_link(conn) do
    problems = Warnings.problems()

    link_content =
      if Enum.count(problems) > 0 do
        [
          "System Status ",
          warning_badge(problems)
        ]
      else
        "System Status"
      end

    sidebar_link(conn, link_content, "chart-line", AirWeb.Router.Helpers.admin_system_status_path(conn, :index))
  end

  @doc "Warnings title"
  @spec admin_title([Warning.problems()]) :: [any]
  def admin_title(problems),
    do: [
      "Admin ",
      warning_badge(problems)
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

  # Note: the parameters are active_class(<Path of page being rendered>, <Path being linked to>)
  defp active_class(link, link), do: "active"
  defp active_class("/admin/system_status" <> _, "/admin"), do: "active"
  defp active_class(_, "/admin"), do: nil
  defp active_class("/admin/queries" <> _, "/admin/queries" <> _), do: "active"
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

  def render_in(module, template, assigns \\ %{}, do: children),
    do: Phoenix.View.render(module, template, put_in(assigns[:children], children))

  def glyph(), do: glyph(true)
  def glyph(true), do: {:safe, "<span class='fas fa-times' aria-hidden='true'></span> "}
  def glyph(false), do: {:safe, ""}
end
