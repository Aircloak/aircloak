defmodule AirWeb.VerifyPermissions do
  @moduledoc """
  Verifies the permission for the current user on the pending Phoenix controller action.

  This plug is automatically included in all of Air controllers. It assumes that
  the user has been properly loaded with `Air.Guardian.Plug.LoadResource`. If the user
  is not logged in (or not loaded), it is assumed to be in the role `:anonymous`.

  The plug verifies if the user has permissions to run the pending action, according
  to controller's specification in the `permissions/0` callback, which must return
  a map of roles to allowed actions. For example:

  ```
  %{
    anonymous: [:action_1, :action_2],
    admin: :all % all actions are permitted
  }
  ```

  If allowed actions are not specified for some role, the empty list is assumed.
  Keep in mind that a user implicitly belongs to multiple roles. So for example,
  if no permissions are listed for the `:admin` role, administrators will still
  be permitted to performed valid actions of included roles (e.g. `:user`).
  """
  require Logger

  @doc "Called to retrieve the permissions for the current controller"
  @callback permissions :: Air.Schemas.User.permissions()

  @behaviour Plug

  @doc "Verifies if the currently logged-in user has permissions on the given action."
  @spec check_permission(Plug.Conn.t(), module, atom) :: :ok | {:error, formatted_error :: String.t()}
  def check_permission(conn, controller, action) do
    user = conn.assigns.current_user

    if Air.Schemas.User.permitted?(user, action, permissions(controller)) do
      :ok
    else
      {:error, "action #{controller}.#{action} not permitted for #{inspect(user || :anonymous)}"}
    end
  end

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    case check_permission(conn, opts[:controller], Phoenix.Controller.action_name(conn)) do
      :ok ->
        conn

      {:error, formatted_error} ->
        Logger.info(formatted_error)

        conn
        |> Phoenix.Controller.redirect(to: "/")
        |> Plug.Conn.halt()
    end
  end

  defp permissions(controller) do
    controller.permissions()
  rescue
    UndefinedFunctionError ->
      Logger.error(fn ->
        "#{inspect(controller)} doesn't implement the `permissions/0` function"
      end)

      %{}
  end
end
