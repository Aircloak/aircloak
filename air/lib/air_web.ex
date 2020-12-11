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

  def live_view do
    quote do
      use Phoenix.LiveView,
        layout: {AirWeb.LayoutView, "live.html"}

      unquote(view_helpers())

      @doc false
      def audit_log(socket, event, metadata \\ []) do
        audit_log_for_user(socket.assigns.current_user, event, metadata)
      end

      @doc false
      def audit_log_for_user(user, event, metadata \\ []) do
        Air.Service.AuditLog.log(
          user,
          event,
          Enum.into(metadata, %{})
        )
      end

      def current_user!(%{"_air_session_token" => token}) do
        {:ok, user_id} = Air.Service.RevokableToken.verify(token, :session)
        {:ok, user} = Air.Service.User.load_enabled(user_id)
        user
      end
    end
  end

  def controller do
    quote do
      unquote(controller_helpers())
      # Each controller must verify permissions
      @behaviour AirWeb.VerifyPermissions
      plug(AirWeb.VerifyPermissions, controller: __MODULE__)
    end
  end

  def admin_controller do
    quote do
      unquote(controller_helpers())

      plug(:put_layout, "admin.html")
    end
  end

  def view do
    quote do
      use Phoenix.View, root: "lib/air_web/templates", namespace: AirWeb

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2, view_module: 1]

      unquote(view_helpers())
    end
  end

  def router do
    quote do
      use Phoenix.Router
      import Phoenix.LiveView.Router
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

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    import Aircloak, only: [in_env: 1]

    Aircloak.ChildSpec.supervisor(
      [
        {Phoenix.PubSub, name: AirWeb.PubSub},
        AirWeb.Endpoint,
        in_env(
          test: nil,
          else:
            Periodic.child_spec(
              run: {AirWeb.Socket.Frontend.DataSourceChannel, :push_updates, []},
              every: :timer.seconds(10)
            )
        )
      ]
      |> Enum.reject(&is_nil/1),
      name: __MODULE__,
      strategy: :rest_for_one
    )
  end

  defp view_helpers do
    quote do
      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML
      import Phoenix.View
      import AirWeb.Router.Helpers
      import AirWeb.ErrorHelpers
      import AirWeb.ViewHelpers
      import AirWeb.Gettext
      import Phoenix.LiveView.Helpers
    end
  end

  defp controller_helpers do
    quote do
      use Phoenix.Controller, namespace: AirWeb

      alias Air.Repo
      import Ecto
      import Ecto.Query, only: [from: 1, from: 2]

      import AirWeb.Router.Helpers
      import AirWeb.Gettext
      import Phoenix.LiveView.Controller

      @doc false
      def audit_log(conn, event, metadata \\ []) do
        audit_log_for_user(conn, conn.assigns.current_user, event, metadata)
      end

      @doc false
      def audit_log_for_user(conn, user, event, metadata \\ []) do
        Air.Service.AuditLog.log(
          user,
          event,
          conn
          |> audit_log_meta()
          |> Map.merge(Enum.into(metadata, %{}))
        )
      end

      @doc false
      def audit_log_meta(conn) do
        %{
          peer:
            case Plug.Conn.get_peer_data(conn) do
              %{address: {a, b, c, d}, port: port} -> "#{a}.#{b}.#{c}.#{d}:#{port}"
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
end
