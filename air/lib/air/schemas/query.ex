defmodule Air.Schemas.Query do
  @moduledoc "The query schema."
  use Air.Schemas.Base

  alias Air.{Schemas.DataSource, Schemas.Query.Rows, Schemas.User, Repo, PsqlServer.Protocol}

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

    # Result is a field in the table, but we're fetching it from a separate schema. Thus, when querying through this
    # schema, result won't be loaded by default (unless preload or join is done). This is done to avoid needlessly
    # retrieving a potentially large field.
    has_one :rows, Rows, foreign_key: :id

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
  @spec for_display(t) :: Map.t
  def for_display(query) do
    query = Repo.preload(query, [:user, :data_source])
    query
    |> Repo.preload([:user, :data_source])
    |> Map.take([:id, :data_source_id, :statement, :session_id, :inserted_at, :query_state])
    |> Map.merge(result_map(query))
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
    |> Map.put(:completed, completed?(query))
  end

  @doc "Exports the query as CSV"
  @spec to_csv_stream(t) :: Enumerable.t
  def to_csv_stream(query) do
    query = Repo.preload(query, [:rows])
    result = result(query)
    header = result["columns"]
    rows =
      result["rows"]
      |> Stream.map(&normalize_row/1)
      |> Enum.flat_map(&List.duplicate(&1.row, &1.occurrences))

    CSV.encode([header | rows])
  end

  defp normalize_row(%{row: _, occurrences: _} = normalized_row), do:
    normalized_row
  defp normalize_row(%{"row" => row, "occurrences" => occurrences}), do:
    # old format, where results were stored as json, and decoded keys are therefore stringified
    %{row: row, occurrences: occurrences}

  def result(%__MODULE__{result: nil}), do:
    nil
  def result(query) do
    case decode_rows(query) do
      :not_loaded -> query.result
      decoded_rows -> Map.put(query.result, "rows", decoded_rows)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp result_map(query), do:
    Map.merge(%{"rows" => [], "columns" => []}, result(query) || %{})

  defp data_source_info(query), do:
    %{data_source: %{name: Map.get(query.data_source || %{}, :name, "Unknown data source")}}

  defp user_info(query), do:
    %{user: %{name: Map.get(query.user || %{}, :name, "Unknown user")}}

  defp completed?(query), do:
    query.query_state in [:error, :completed, :cancelled]

  # For backwards compatibility reasons, we need to handle the old format, where rows have been encoded directly in
  # the result map.
  defp decode_rows(%__MODULE__{result: %{"rows" => rows}}), do:
    # old format
    rows
  defp decode_rows(query), do:
    # new format
    Rows.decode(query.rows)
end
