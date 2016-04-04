defmodule Air.ViewHelpers do
  @moduledoc "Common helper functions for views."

  @doc "Verifies if the currently logged-in user has permissions on the given action."
  @spec permitted?(Plug.Conn.t, module, atom) :: boolean
  def permitted?(conn, controller, action) do
    case Air.VerifyPermissions.check_permission(conn, controller, action) do
      :ok -> true
      {:error, _formatted_error} -> false
    end
  end

  @doc """
  Generates the link for managing users.

  The administrator link will lead to the page where all users can be edited.
  For org admins, the link will point to their organisation.
  """
  @spec manage_users_link(Plug.Conn.t) :: String.t
  def manage_users_link(conn) do
    user = Guardian.Plug.current_resource(conn)
    if Air.User.admin?(user) do
      Air.Router.Helpers.user_path(conn, :index)
    else
      Air.Router.Helpers.organisation_path(conn, :show, user.organisation_id)
    end
  end
end
