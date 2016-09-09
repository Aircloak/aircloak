defmodule Air.LicenseView do
  @moduledoc false
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  def type_counts(packages) do
    packages
    |> Enum.group_by(&(&1.license_type))
    |> Enum.map(fn({type, packages}) -> {type, length(packages)} end)
    |> Enum.sort_by(fn({type, count}) -> count end, &>=/2)
  end
end
