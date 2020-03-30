defmodule AirWeb.SessionView do
  @moduledoc false
  use Air.Web, :view

  defp panel_class(conn) do
    cond do
      has_flash(conn, :error) -> "border-danger"
      has_flash(conn, :info) -> "border-info"
      true -> ""
    end
  end

  defp has_flash(conn, flash), do: not is_nil(get_flash(conn, flash))
end
