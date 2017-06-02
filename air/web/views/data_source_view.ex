defmodule Air.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.Service.DataSource

  defdelegate status(data_source), to: DataSource

  def number_of_tables(data_source), do: length(DataSource.tables(data_source))

  defp sample_of_tables(data_source) do
    DataSource.tables(data_source)
    |> Enum.map(fn(%{"id" => name}) -> name end)
    |> limited_join(nil, 64)
  end

  defp limited_join([], nil, _length), do: "."
  defp limited_join([value | rest], nil, length), do: limited_join(rest, value, length)
  defp limited_join(_values, accumulator, length) when byte_size(accumulator) > length, do: accumulator <> ", ..."
  defp limited_join([], accumulator, _length), do: accumulator <> "."
  defp limited_join([value | rest], accumulator, length), do: limited_join(rest, "#{accumulator}, #{value}", length)

  def availability_label(data_source) do
    case status(data_source) do
      :broken -> content_tag(:span, "Broken", class: "label label-warning")
      :online -> content_tag(:span, "Online", class: "label label-success")
      :offline -> content_tag(:span, "Offline", class: "label label-danger")
    end
  end

  defp any_with_description?(data_sources), do:
    Enum.any?(data_sources, & &1.description || "" |> String.trim() |> String.length() > 0)
end
