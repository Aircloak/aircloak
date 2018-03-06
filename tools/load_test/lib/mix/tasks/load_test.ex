defmodule Mix.Tasks.Aircloak.LoadTest do
  @shortdoc "Performs a load test of the Aircloak system."
  @moduledoc ~S{
  Performs a load test of the Aircloak system.

  ## Usage

      mix run aircloak.load_test [switch ...]

  ## Switches

    - `--no-ssl` - disables SSL when connecting to the system
    - `--host`, `-h` - the address of the Air system
    - `--port`, `-p` - the port on which the Air system accepts PostgreSQL connections
    - `--user`, `-u` - email address which is used to login to the Air system
    - `--password` - password for the Air system
    - `--database`, `-d` - name of the datasource which is queried
    - `--query`, `-q` - the query to execute
    - `--num-clients` - the number of concurrent client processes used, defaults to 1
    - `--num-queries`, `-n` - the number of queries issued per each client process, defaults to 1

  The total number of queries issued is `num-clients * num_queries`.

  ## Example

  The following example invokes 1000 queries (using 10 client processes) on demo.aircloak.com.

      mix aircloak.load_test \
        --no-ssl \
        -h demo.aircloak.com \
        -p 8432 \
        -u user@aircloak.com \
        --password password \
        -d banking \
        -n 100 \
        --num-clients 10 \
        -q "select count(*) from loans"
  }

  use Mix.Task

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks


  # -------------------------------------------------------------------
  # Mix.Task callbacks
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(args) do
    Mix.Task.run("compile")
    Mix.Task.run("app.start")

    {options, _args} = OptionParser.parse!(args, switches: switches(), aliases: aliases())
    ensure_required_switches!(options)

    {connection_params, options} = Keyword.split(options, [:ssl, :hostname, :port, :username, :password, :database])

    Enum.each(1..num_clients(options), fn(_) -> start_client(connection_params, options, query(options)) end)

    total_queries_count = num_queries(options) * num_clients(options)
    IO.puts "Running #{total_queries_count} queries..."
    await_results(total_queries_count)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp switches() do
    [
      ssl: :boolean, hostname: :string, port: :integer, username: :string, password: :string, database: :string,
      num_queries: :integer, num_clients: :integer, query: :string
    ]
  end

  defp aliases, do: [h: :hostname, p: :port, u: :username, d: :database, n: :num_queries, q: :query]

  defp ensure_required_switches!(options) do
    [:hostname, :port, :username, :password, :database, :query]
    |> Enum.reject(&Keyword.has_key?(options, &1))
    |> case do
        [] -> :ok
        missing -> Mix.raise("Missing required switches: #{Enum.join(missing, ", ")}")
      end
  end

  defp num_clients(options), do: Keyword.get(options, :num_clients, 1)

  defp num_queries(options), do: Keyword.get(options, :num_queries, 1)

  defp query(options), do: Keyword.fetch!(options, :query)

  defp start_client(connection_params, options, query) do
    owner = self()

    Task.start_link(
      fn ->
        {:ok, conn} = Postgrex.start_link(connection_params)
        Enum.each(
          1..num_queries(options),
          fn(_) ->
            Postgrex.query!(conn, query, [], timeout: :timer.hours(1))
            send(owner, :query_completed)
          end
        )
      end
    )
  end

  defp await_results(total_queries_count, completed \\ 0)

  defp await_results(total_queries_count, total_queries_count), do: IO.puts("\rDone!")
  defp await_results(total_queries_count, completed) do
    IO.write("\r#{round(completed/total_queries_count * 100)}%")
    receive do
      :query_completed -> await_results(total_queries_count, completed + 1)
    end
  end
end
