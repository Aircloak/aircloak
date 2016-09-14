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

  @doc "Returns true if the currently logged-in user is an administrator."
  @spec admin?(Plug.Conn.t) :: boolean
  def admin?(conn),
    do: Air.User.admin?(conn.assigns.current_user)

  @doc """
  Generates the link for managing users.

  The administrator link will lead to the page where all users can be edited.
  For org admins, the link will point to their organisation.
  """
  @spec manage_users_link(Plug.Conn.t) :: String.t
  def manage_users_link(conn) do
    if Air.User.admin?(conn.assigns.current_user) do
      Air.Router.Helpers.user_path(conn, :index)
    else
      Air.Router.Helpers.organisation_path(conn, :show, conn.assigns.current_user.organisation_id)
    end
  end

  @doc """
  Generates a navbar link, and highlights the active one
  """
  @spec navbar_link(Plug.Conn.t, String.t, String.t) :: {:safe, String.t}
  def navbar_link(%{request_path: request_path}, name, desired_path) do
    link_html = if active?(request_path, desired_path) do
      "<li role=\"presentation\" class=\"active\"><a href=\"#{desired_path}\">#{name}</a></li>"
    else
      "<li><a href=\"#{desired_path}\">#{name}</a></li>"
    end
    {:safe, link_html}
  end

  defp active?(request_path, link_path)
  defp active?("/", "/"), do: true
  defp active?("/admin", "/admin/cloaks"), do: true
  defp active?(_, "/"), do: false
  defp active?(request_path, link_path), do: request_path =~ link_path
end
