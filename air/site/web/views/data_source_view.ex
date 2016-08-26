defmodule Air.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{DataSource, DataSourceManager}

  def available?(data_source) do
    DataSourceManager.available?(data_source.unique_id)
  end

  def number_of_tables(data_source) do
    length(DataSource.tables(data_source))
  end

  def sample_of_tables(data_source) do
    DataSource.tables(data_source)
    |> Enum.take(3)
    |> Enum.map(fn(%{"id" => name}) -> name end)
    |> Enum.join(", ")
  end
end
