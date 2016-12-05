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

  defp active_class("/admin", "/admin/cloaks"), do: "active"
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
end
