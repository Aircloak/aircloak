defmodule Air.Schemas.Query do
  @moduledoc "The query model."
  use Air.Schemas.Base

  alias Air.{Schemas.DataSource, Schemas.User, Repo, PsqlServer.Protocol}

  require EctoEnum
  EctoEnum.defenum QueryState, :query_state, [
    :started, :parsing, :compiling, :awaiting_data, :processing, :completed, :error, :cancelled
  ]

  @type t :: %__MODULE__{}
  @type cloak_query :: %{
    id: String.t,
    statement: String.t,
    parameters: [Protocol.db_value],
    data_source: String.t,
    views: %{String.t => String.t},
    query_state: __MODULE__.QueryState.t,
  }

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :statement, :string
    field :tables, {:array, :string}
    field :result, :map
    field :execution_time, :integer
    field :users_count, :integer
    field :features, :map
    field :session_id, Ecto.UUID
    field :parameters, :map
    field :cloak_id, :string
    field :query_state, __MODULE__.QueryState

    belongs_to :user, User
    belongs_to :data_source, DataSource

    timestamps usec: true
  end

  @required_fields ~w()a
  @optional_fields ~w(
    cloak_id statement data_source_id tables result execution_time users_count
    features session_id parameters query_state
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

  @doc "Produces a JSON blob of the query and it's result for rendering"
  @spec for_display(t) :: Map.t
  def for_display(query) do
    query = Repo.preload(query, [:user, :data_source])
    query
    |> Repo.preload([:user, :data_source])
    |> Map.take([:id, :data_source_id, :statement, :session_id, :inserted_at, :query_state])
    |> Map.merge(result_map(query))
    |> Map.merge(data_source_info(query))
    |> Map.merge(user_info(query))
  end

  @doc "Exports the query as CSV"
  @spec to_csv_stream(t) :: Enumerable.t
  def to_csv_stream(%{result: result}) do
    header = result["columns"]
    rows = Enum.flat_map(result["rows"],
      fn(%{"occurrences" => occurrences, "row" => row}) -> List.duplicate(row, occurrences) end)
    CSV.encode([header | rows])
  end


  # -------------------------------------------------------------------
  # Query functions
  # -------------------------------------------------------------------

  @doc "Adds a query filter selecting only those for the given data source"
  @spec for_data_source(Ecto.Queryable.t, DataSource.t) :: Ecto.Queryable.t
  def for_data_source(query \\ __MODULE__, data_source) do
    from q in query,
    where: q.data_source_id == ^data_source.id
  end

  @doc "Adds a query filter limiting the number of selected queries"
  @spec recent(Ecto.Queryable.t, pos_integer, NaiveDateTime.t) :: Ecto.Queryable.t
  def recent(query \\ __MODULE__, count, before) do
    from q in query,
    where: q.inserted_at < ^before,
    order_by: [desc: q.inserted_at],
    limit: ^count
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp result_map(%{result: nil}), do: %{rows: [], columns: [], completed: false}
  defp result_map(%{result: result_json}), do: Map.put(result_json, :completed, true)

  defp data_source_info(query), do:
    %{data_source: %{name: Map.get(query.data_source || %{}, :name, "Unknown data source")}}

  defp user_info(query), do:
    %{user: %{name: Map.get(query.user || %{}, :name, "Unknown user")}}
end
