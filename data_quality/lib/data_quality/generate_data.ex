defmodule DataQuality.GenerateData do
  @moduledoc """
  Generates a dataset based on the planned Distributions
  and writes the data to a database table after having
  cleared all existing data.
  """
  alias Aircloak.OutputStatus
  alias DataQuality.Distributions
  alias DataQuality.Distributions.Beta

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec run(Map.t()) :: :ok
  @doc "Creates database tables and populates them with test data from the configured beta distributions."
  def run(config) do
    open_connection(config)
    |> create_table()
    |> insert_data()

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_table(conn) do
    conn
    |> execute!("DROP TABLE IF EXISTS data_quality")
    |> execute!("CREATE TABLE IF NOT EXISTS data_quality (uid integer, number integer, distribution text)")
    |> execute!("TRUNCATE TABLE data_quality")
    |> execute!("CREATE INDEX IF NOT EXISTS uid_idx ON data_quality (uid)")
    |> execute!("CREATE INDEX IF NOT EXISTS number_idx ON data_quality (number)")
    |> execute!("CREATE INDEX IF NOT EXISTS distribution_idx ON data_quality (distribution)")
  end

  defp insert_data(conn) do
    for distribution <- Distributions.list() do
      name = Distributions.distribution_name(distribution)
      OutputStatus.new_line(name, :pending, "creating data")

      success =
        Beta.generate(
          distribution[:min],
          distribution[:max],
          distribution[:users] * distribution[:entries_per_user],
          distribution[:alpha],
          distribution[:beta]
        )
        |> Stream.chunk_every(distribution[:entries_per_user])
        |> Stream.zip(1..distribution[:users])
        |> Stream.flat_map(&to_rowspecs(name, &1))
        |> Stream.chunk_every(1000)
        |> Stream.map(&insert(conn, &1))
        |> Enum.all?(& &1)

      if success do
        OutputStatus.done(name)
      else
        OutputStatus.end_line(name, :error)
      end
    end
  end

  defp insert(conn, row_data) do
    sql = "INSERT INTO data_quality (uid, number, distribution) VALUES #{row_data |> Enum.join(", ")}"

    case Postgrex.query!(conn, sql, []) do
      %Postgrex.Result{command: :insert} -> true
      _ -> false
    end
  end

  defp to_rowspecs(dist_name, {rows, user}), do: Enum.map(rows, &to_rowspec(dist_name, user, &1))

  defp to_rowspec(dist_name, user, value), do: "(#{user}, #{value}, '#{dist_name}')"

  defp execute!(conn, query_text, params \\ []) do
    query = Postgrex.prepare!(conn, "", query_text)
    {:ok, _} = Postgrex.execute(conn, query, params)
    conn
  end

  defp open_connection(config) do
    {:ok, pid} =
      Postgrex.start_link(
        database: config.database[:database] || "data_quality",
        hostname: config.database[:host] || "localhost",
        username: config.database[:user] || "data_quality",
        port: config.database[:port] || 5432,
        password: config.database[:password] || ""
      )

    pid
  end
end
