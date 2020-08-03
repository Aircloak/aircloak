defmodule Mix.Tasks.Cloak.Query do
  @moduledoc "Queries a data source."
  @shortdoc @moduledoc

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task

  # -------------------------------------------------------------------
  # Mix task interface
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(args) do
    {_options, [data_source_name, query]} = OptionParser.parse!(args, strict: [])
    Application.ensure_all_started(:cloak)
    {:ok, data_source} = Cloak.DataSource.fetch(data_source_name)
    result = run_query(data_source, query)
    IO.puts("Query result: #{inspect(result)}")
  end

  defp run_query(data_source, query) do
    Cloak.Query.Runner.run_sync("1", nil, data_source, query, [], %{})
  end
end
