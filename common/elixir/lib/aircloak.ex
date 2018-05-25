defmodule Aircloak do
  @moduledoc "Common helper functions used in different projects."
  use Application
  require Logger

  # -------------------------------------------------------------------
  # Macros
  # -------------------------------------------------------------------

  @doc """
  This macro can be used to conditionally inject some code, based on mix env.

  Example:

  ```
  mix_env_specific(dev: foo(), prod: bar(), else: baz())
  ```

  The invocation above will generate the code which invokes `foo/0` in dev,
  `bar/0` in prod, and `baz/0` in all other environments.
  """
  defmacro mix_env_specific(config) do
    quote do
      unquote(Keyword.get_lazy(config, Mix.env(), fn -> Keyword.fetch!(config, :else) end))
    end
  end

  @doc """
  Helper macro to indicate that a variable is not used in particular environments.

  A typical example is when you use `mix_env_specific/1` with a variable:

  ```
  def foo(bar) do
    mix_env_specific(dev: nil, else: do_something_with(bar))
  end
  ```

  This code would generate a warning in dev, because the variable is not used, but the variable can't be anonymous,
  because it's used in other envs. You can fix this with `unused/2`:

  ```
  def foo(bar) do
    unused(bar, in: [:dev])
    mix_env_specific(dev: nil, else: do_something_with(bar))
  end
  ```
  """
  defmacro unused(term, in: environments) do
    if Mix.env() in environments, do: quote(do: _ = unquote(term))
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Recursively atomizes the keys in a map, leaving values untouched."
  @spec atomize_keys(Map.t()) :: Map.t()
  def atomize_keys(%{} = map) do
    for {key, value} <- map, into: %{}, do: {String.to_atom(key), atomize_keys(value)}
  end

  def atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end

  def atomize_keys(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> atomize_keys()
    |> List.to_tuple()
  end

  def atomize_keys(other), do: other

  @doc """
  Executes the function, logs the running time, and returns the function result.

  Options:

    - `:threshold` - The minimum running time in milliseconds required to make a log entry. Defaults to 0.
    - `:level` - Logger level used to make a log entry. Defaults to `:info`.
  """
  @spec measure(
          any,
          (() -> result),
          threshold: non_neg_integer,
          level: :debug | :info | :warn | :error
        ) :: result
        when result: var
  def measure(id, fun, opts \\ []) do
    {time, result} = :timer.tc(fun)

    if time >= Keyword.get(opts, :threshold, 0) * 1000 do
      Logger.log(
        Keyword.get(opts, :level, :info),
        "operation `#{inspect(id)}` took #{div(time, 1000)}ms"
      )
    end

    result
  end

  @doc """
  Executes the operation, and logs the running time if it exceeds some threshold.

  The `:threshold` option can be used to set the threshold in milliseconds. The default value is `10`.
  """
  @spec report_long(any, (() -> result), threshold: non_neg_integer) :: result when result: var
  def report_long(id, fun, opts \\ []), do: measure(id, fun, level: :warn, threshold: Keyword.get(opts, :threshold, 10))

  @doc "Waits for the service on the given host/port to become available."
  @spec await_service!(String.t(), :inet.port_number()) :: :ok
  def await_service!(host, port) do
    task =
      Task.async(fn ->
        fn -> :gen_tcp.connect(to_charlist(host), port, []) end
        |> Stream.repeatedly()
        |> Stream.drop_while(&match?({:error, _}, &1))
        |> Enum.take(1)
      end)

    case Task.yield(task, :timer.minutes(2)) do
      nil -> raise("#{host}:#{port} is not available")
      _ -> :ok
    end
  end

  @doc "Validates a decoded json against the given schema."
  @spec validate_decoded_json(atom, String.t(), any, String.t()) :: :ok | {:error, String.t()}
  def validate_decoded_json(app, file_name, data, main_error_message) do
    schema =
      app
      |> Application.app_dir("priv")
      |> Path.join(file_name)
      |> File.read!()
      |> Aircloak.Json.safe_decode!()
      |> ExJsonSchema.Schema.resolve()

    with {:error, errors} <- ExJsonSchema.Validator.validate(schema, data) do
      formatted_errors =
        errors
        |> Stream.map(fn {error, field} -> "  #{field}: #{error}" end)
        |> Enum.join("\n")

      {:error, "#{main_error_message}:\n#{formatted_errors}"}
    end
  end

  # -------------------------------------------------------------------
  # Application callbacks
  # -------------------------------------------------------------------

  @impl Application
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [supervisor(Aircloak.ProcessMonitor, [])],
      strategy: :one_for_one,
      name: Aircloak.Supervisor
    )
  end
end
