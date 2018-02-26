defmodule Air.Web do
  @moduledoc """
  A module that keeps using definitions for controllers,
  views and so on.

  This can be used in your application as:

      use Air.Web, :controller
      use Air.Web, :view

  The definitions below will be executed for every view,
  controller, etc, so keep them short and clean, focused
  on imports, uses and aliases.

  Do NOT define functions inside the quoted expressions
  below.
  """

  def controller do
    quote do
      use Phoenix.Controller, namespace: AirWeb

      alias Air.Repo
      import Ecto
      import Ecto.Query, only: [from: 1, from: 2]

      import AirWeb.Router.Helpers
      import AirWeb.Gettext

      # Each controller must verify permissions
      @behaviour AirWeb.VerifyPermissions
      plug AirWeb.VerifyPermissions, controller: __MODULE__

      @doc false
      def audit_log(conn, event, metadata \\ []) do
        Air.Service.AuditLog.log(conn.assigns.current_user, event,
          conn
          |> audit_log_meta()
          |> Map.merge(Enum.into(metadata, %{}))
        )
      end

      @doc false
      def audit_log_meta(conn) do
        %{
          peer:
            case conn.peer do
              {{a, b, c, d}, port} -> "#{a}.#{b}.#{c}.#{d}:#{port}"
              _ -> "Unknown"
            end,

          remote_ip:
            case conn.remote_ip do
              {a, b, c, d} -> "#{a}.#{b}.#{c}.#{d}"
              _ -> "Unknown"
            end
        }
      end

      defp not_found(conn) do
        conn
        |> put_layout(false)
        |> put_status(:not_found)
        |> put_view(AirWeb.ErrorView)
        |> render("404.html")
        |> halt()
      end
    end
  end

  def admin_controller do
    quote do
      use Air.Web, :controller

      plug :put_layout, "admin.html"
    end
  end

  def view do
    quote do
      use Phoenix.View, root: "lib/air_web/templates", namespace: AirWeb

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2, view_module: 1]

      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML

      import AirWeb.Router.Helpers
      import AirWeb.ErrorHelpers
      import AirWeb.ViewHelpers
      import AirWeb.Gettext
    end
  end

  def router do
    quote do
      use Phoenix.Router
    end
  end

  def channel do
    quote do
      use Phoenix.Channel

      alias Air.Repo
      import Ecto
      import Ecto.Query, only: [from: 1, from: 2]
      import AirWeb.Gettext
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
