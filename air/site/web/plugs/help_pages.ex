defmodule Air.Plug.HelpPages do
  @moduledoc """
  Plug for declaring help pages related to particular actions.

  Example usage in a Phoenix controller:

  ```
  defmodule MyController do
    plug Air.Plug.HelpPages, [
      index: [:page_one, :page_two],
      new: [:page_three],
      ...
    ]

    # ...
  end
  ```
  """
  @behaviour Plug
  alias Plug.Conn


  # -------------------------------------------------------------------
  # Plug callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(opts), do: opts

  @doc false
  def call(conn, opts) do
    Conn.assign(conn, :help_pages,
      Keyword.get(opts, Phoenix.Controller.action_name(conn), [])
    )
  end

  def help_pages(conn) do
    Map.get(conn.assigns, :help_pages, [])
  end
end
