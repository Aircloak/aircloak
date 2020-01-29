defmodule Mix.Tasks.Cloak.PingDb do
  @moduledoc "Checks connection to the specified data source."
  @shortdoc @moduledoc

  # Mix.Task behaviour is not in PLT since Mix is not a runtime dep, so we disable the warning
  @dialyzer :no_undefined_callbacks

  use Mix.Task

  @switches [
    name: :string,
    driver: :string,
    attempts: :integer,
    interval: :float
  ]

  @switches_keys Keyword.keys(@switches)

  @default_options [
    attempts: 10,
    interval: 3
  ]

  @connect_timeout 5000

  # -------------------------------------------------------------------
  # Mix task interface
  # -------------------------------------------------------------------

  @impl Mix.Task
  def run(args) do
    {options, pos_args} = OptionParser.parse!(args, switches: @switches, allow_nonexistent_atoms: true)

    data_source_base = load_config!(pos_args)
    {overrides, parameters_overrides} = options |> Map.new() |> Map.split(@switches_keys)
    {data_source_overrides, option_overrides} = Map.split(overrides, [:name, :driver])
    option_overrides = Keyword.new(option_overrides)

    data_source =
      data_source_base
      |> Map.merge(data_source_overrides)
      |> Map.update(:driver, nil, &map_driver!/1)

    parameters =
      driver_defaults(data_source.driver)
      |> Map.merge(Map.get(data_source, :parameters, %{}))
      |> Map.merge(parameters_overrides)

    data_source = Map.put(data_source, :parameters, parameters)
    task_options = Keyword.merge(@default_options, option_overrides)

    case ping(data_source, task_options) do
      :ok -> exit(:normal)
      :error -> exit({:shutdown, 1})
    end
  end

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec ping!(Cloak.DataSource.t(), keyword) :: :ok
  def ping!(data_source, options \\ @default_options) do
    case ping(data_source, options) do
      :ok -> :ok
      :error -> raise "Connection to data source failed."
    end
  end

  @spec ping(Cloak.DataSource.t(), keyword) :: :ok | :error
  def ping(data_source, options \\ @default_options) do
    options = Keyword.merge(@default_options, options)

    load_dependencies(data_source.driver)

    ping_attempt(
      data_source.driver,
      data_source.parameters,
      data_source.name || data_source.driver,
      options[:attempts],
      options[:interval]
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp ping_attempt(driver, parameters, name, attempts, interval, attempt \\ 1) do
    IO.write("Connecting to #{name} (#{attempt}) ... ")

    case try_ping(driver, parameters) do
      :ok ->
        IO.puts("OK.")
        :ok

      {:error, _} when attempt >= attempts ->
        IO.puts("failed after #{attempts} attempts.")
        :error

      {:error, _} ->
        IO.puts("error, retrying...")
        Process.sleep(1000 * interval)
        ping_attempt(driver, parameters, name, attempts, interval, attempt + 1)
    end
  end

  defp try_ping(driver, parameters) do
    Task.async(fn ->
      try do
        case driver.connect(parameters) do
          {:ok, connection} ->
            try do
              driver.health_check(connection)
            after
              driver.disconnect(connection)
            end

          {:error, reason} ->
            {:error, reason}

          _ ->
            {:error, :unknown}
        end
      catch
        :error, reason -> {:error, reason}
        :exit, reason -> {:error, reason}
      end
    end)
    |> Task.await(@connect_timeout)
  end

  defp map_driver!(driver) when is_atom(driver), do: driver

  defp map_driver!(driver) when is_binary(driver) do
    case Cloak.DataSource.Utility.name_to_driver(driver) do
      {:ok, driver} -> driver
      {:error, _} -> raise "Unknown or missing data source driver."
    end
  end

  defp map_driver!(_), do: raise("Unknown or missing data source driver.")

  defp load_dependencies(driver) do
    driver
    |> driver_dependencies()
    |> Enum.each(&Application.ensure_all_started/1)
  end

  defp driver_defaults(driver) do
    case driver do
      Cloak.DataSource.Oracle -> %{port: 1521}
      Cloak.DataSource.PostgreSQL -> %{port: 5432}
      Cloak.DataSource.SQLServer -> %{port: 1433}
      Cloak.DataSource.MySQL -> %{port: 3306}
      Cloak.DataSource.MongoDB -> %{port: 27_017}
      _ -> %{}
    end
  end

  defp driver_dependencies(driver) do
    case driver do
      Cloak.DataSource.Oracle -> [:odbc]
      Cloak.DataSource.PostgreSQL -> [:postgrex]
      Cloak.DataSource.SQLServer -> [:odbc]
      Cloak.DataSource.MySQL -> [:mariaex]
      Cloak.DataSource.MongoDB -> [:mongo]
      _ -> []
    end
  end

  defp load_config!(pos_args) do
    case pos_args do
      [path] -> parse_config!(path)
      [] -> %{}
      _ -> raise OptionParser.ParseError, message: "Invalid arguments."
    end
  end

  defp parse_config!(file_path) do
    file_path
    |> File.read!()
    |> Aircloak.Json.permissive_decode()
    |> case do
      {:ok, data} -> Aircloak.atomize_keys(data)
      {:error, reason} -> raise reason
    end
  end
end
