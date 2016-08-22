defmodule BOM.Validate do
  def run(package) do
    %{package | error: errors(package)}
  end

  def errors(%{license: nil}), do: "No license found"
  def errors(%{license: license}) do
    text = (license.text || "") |> String.trim()
    if text == "", do: "License empty", else: nil
  end
  def errors(_), do: nil
end
