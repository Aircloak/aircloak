defmodule Air.Admin.DataSourceView do
  @moduledoc false;
  use Air.Web, :view
  # bug in the current Phoenix
  @dialyzer :no_match

  alias Air.{DataSource, DataSourceManager}

  def available?(data_source), do: DataSourceManager.available?(data_source.global_id)

  def number_of_tables(data_source), do: length(DataSource.tables(data_source))
end
