defmodule Air.Plug.Expiration do
  @moduledoc false

  alias Air.Service.Version


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  defmodule Api do
    @moduledoc """
    This plug terminates the request with an error if the Aircloak installation
    runs an expired version.
    """
    @behaviour Plug

    import Plug.Conn

    @doc false
    def init(opts), do: opts

    @doc false
    def call(conn, _opts) do
      if Version.expired?() do
        conn
        |> put_status(Plug.Conn.Status.code(:upgrade_required))
        |> Phoenix.Controller.json(%{success: false, description:
          "The version of this Aircloak installation has expired"})
        |> halt()
      else
        conn
      end
    end
  end


  # -------------------------------------------------------------------
  # Browser
  # -------------------------------------------------------------------

  defmodule Browser do
    @moduledoc """
    This plug terminates the request with a redirect to the upgrade page
    if the Aircloak installation runs an expired version.
    """
    @behaviour Plug

    import Plug.Conn

    @doc false
    def init(opts), do: opts

    @doc false
    def call(conn, _opts) do
      if Version.expired?() do
        conn
        |> Phoenix.Controller.render(Air.OutOfDateView, "index.html", layout: false)
        |> halt()
      else
        conn
      end
    end
  end
end
