defmodule AirWeb.SharedView do
  @moduledoc false;
  use Air.Web, :view

  def version() do
    :air
    |> Aircloak.Version.for_app()
    |> Aircloak.Version.to_string()
  end
end
