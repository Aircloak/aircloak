defmodule CentralWeb.ViewHelpers do
  @moduledoc "Common helper functions for views."

  @doc """
  Generates a navbar link, and highlights the active one
  """
  @spec navbar_link(Plug.Conn.t(), String.t(), String.t()) :: {:safe, String.t()}
  def navbar_link(%{request_path: request_path}, name, desired_path) do
    link_html =
      if active?(request_path, desired_path) do
        ~s[<li role="presentation" class="active"><a href="#{desired_path}">#{name}</a></li>]
      else
        ~s[<li><a href="#{desired_path}">#{name}</a></li>]
      end

    {:safe, link_html}
  end

  defp active?("/", "/customers"), do: true
  defp active?(request_path, link_path), do: String.starts_with?(request_path, link_path)

  def logged_in?(conn) do
    conn.assigns != nil && conn.assigns.current_user != nil
  end
end
