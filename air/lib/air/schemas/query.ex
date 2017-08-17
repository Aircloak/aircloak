defmodule Air.Schemas.Query do
  @moduledoc "The query schema."
  use Air.Schemas.Base

  alias Air.{Repo, PsqlServer.Protocol}
  alias Air.Schemas.{DataSource, User, ResultChunk}

  require EctoEnum

  EctoEnum.defenum QueryState, :query_state, [
    :started, :parsing, :compiling, :awaiting_data, :ingesting_data, :processing, :post_processing, :completed,
    :error, :cancelled
  ]

  EctoEnum.defenum Context, :query_context, [:http, :psql, :api]

  @type t :: %__MODULE__{}
  @type cloak_query :: %{
    id: String.t,
    statement: String.t,
    parameters: [Protocol.db_value],
    data_source: String.t,
    views: %{String.t => String.t},
    query_state: __MODULE__.QueryState.t,
    context: __MODULE__.Context.t,
  }

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :statement, :string
    field :tables, {:array, :string}
    field :execution_time, :integer
    field :users_count, :integer
    field :features, :map
    field :session_id, Ecto.UUID
    field :parameters, :map
    field :cloak_id, :string
    field :query_state, __MODULE__.QueryState
    field :context, Context
    field :result, :map

    belongs_to :user, User
    belongs_to :data_source, DataSource
    has_many :result_chunks, ResultChunk

    timestamps usec: true
  end

  @required_fields ~w()a
  @optional_fields ~w(
    cloak_id statement data_source_id tables execution_time users_count
    features session_id parameters query_state context result
  )a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, Map.t) :: Ecto.Changeset.t
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:data_source_id)
  end

  @doc "Produces a JSON blob of the query and its result for rendering"
  @spec for_display(t, nil | [map]) :: Map.t
  def for_display(query, buckets \\ nil) do
    query = Repo.preload(query, [:user, :data_source])
    query
    |> Repo.preload([:user, :data_source])
    |> Map.take([:id, :data_source_id, :statement, :session_id, :inserted_at, :query_state])
    |> Map.merge(query.result || %{})
    |> add_result(query, buckets)
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
    |> Map.put(:completed, completed?(query))
  end

  @doc "Exports the query as CSV"
  @spec to_csv_stream(t, Enumerable.t) :: Enumerable.t
  def to_csv_stream(query, buckets), do:
    [query.result["columns"]]
    |> Stream.concat(ResultChunk.rows_stream(buckets))
    |> CSV.encode()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp data_source_info(query), do:
    %{data_source: %{name: Map.get(query.data_source || %{}, :name, "Unknown data source")}}

  defp user_info(query), do:
    %{user: %{name: Map.get(query.user || %{}, :name, "Unknown user")}}

  defp completed?(query), do:
    query.query_state in [:error, :completed, :cancelled]

  defp add_result(result, _query, nil), do:
    result
  defp add_result(result, query, buckets), do:
    result
    |> Map.put(:columns, query.result["columns"])
    |> Map.put(:rows, buckets)
end
