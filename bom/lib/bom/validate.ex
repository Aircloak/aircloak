defmodule BOM.Validate do
  def run(package) do
    %{package | error: errors(package)}
  end

  def errors(%{license: nil}), do: "No license found"
  def errors(_), do: nil
end
