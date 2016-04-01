defmodule Air.VerifyPermissions do
  @moduledoc """
  Verifies the permission for the current user on the pending Phoenix controller action.

  This plug is automatically included in all of Air controllers. It assumes that
  the user has been properly loaded with `Guardian.Plug.LoadResource`. If the user
  is not logged in (or not loaded), it is assumed to be in the role `:anonymous`.

  The plug verifies if the user has permissions to run the pending action, according
  to controller's specification in the `permissions/0` callback, which must return
  a map of roles to allowed actions. For example:

  ```
  %{
    anonymous: [:action_1, :action_2],
    org_admin: [:action_3],
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
  @callback permissions :: Air.User.permissions

  @doc false
  def init(opts), do: opts

  @doc false
  def call(conn, opts) do
    user = Guardian.Plug.current_resource(conn)
    action = Phoenix.Controller.action_name(conn)
    controller = opts[:controller]
    if Air.User.permitted?(user, action, permissions(controller)) do
      conn
    else
      Logger.info(fn -> "action #{controller}.#{action} not permitted for #{inspect user}" end)

      conn
      |> Phoenix.Controller.redirect(to: "/")
      |> Plug.Conn.halt()
    end
  end

  defp permissions(controller) do
    try do
      controller.permissions()
    rescue UndefinedFunctionError ->
      Logger.error(fn -> "#{inspect controller} doesn't implement the `permissions/0` function" end)
      %{}
    end
  end
end
