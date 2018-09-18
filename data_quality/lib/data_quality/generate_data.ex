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

  @spec run(Mapt.t()) :: :ok
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
    for({name, params} <- Distributions.list()) do
      OutputStatus.new_line(name, :pending, "creating data")

      rows =
        Beta.generate(
          params[:min],
          params[:max],
          params[:users] * params[:entries_per_user],
          params[:alpha],
          params[:beta]
        )
        |> Enum.chunk_every(params[:entries_per_user])
        |> Enum.zip(1..params[:users])
        |> Enum.flat_map(&to_rowspecs(name, &1))

      OutputStatus.update_state(name, :pending, "inserting data")

      rows
      |> Enum.chunk_every(1000)
      |> Enum.each(&insert(conn, &1))

      OutputStatus.done(name)
    end
  end

  defp insert(conn, row_data),
    do:
      Postgrex.query!(
        conn,
        "INSERT INTO data_quality (uid, number, distribution) VALUES #{row_data |> Enum.join(", ")}",
        []
      )

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
