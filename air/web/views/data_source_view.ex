defmodule Air.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{Schemas.DataSource, DataSourceManager}

  defp available?(data_source), do: DataSourceManager.available?(data_source.global_id)

  defp number_of_tables(data_source) do
    length(DataSource.tables(data_source))
  end

  defp sample_of_tables(data_source) do
    DataSource.tables(data_source)
    |> Enum.map(fn(%{"id" => name}) -> name end)
    |> limited_join(nil, 64)
  end

  defp limited_join([value | rest], nil, length), do: limited_join(rest, value, length)
  defp limited_join(_values, accumulator, length) when byte_size(accumulator) > length, do: accumulator <> ", ..."
  defp limited_join([], accumulator, _length), do: accumulator <> "."
  defp limited_join([value | rest], accumulator, length), do: limited_join(rest, "#{accumulator}, #{value}", length)

  def availability_label(data_source) do
    if available?(data_source) do
      content_tag(:span, "Online", class: "label label-success")
    else
      content_tag(:span, "Offline", class: "label label-danger")
    end
  end
end
