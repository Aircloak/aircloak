defmodule Air.Query do
  @moduledoc "The query model."
  use Air.Web, :model

  alias Air.{User, Repo, DataSource}

  @type t :: %__MODULE__{}
  @type cloak_query :: %{id: String.t, statement: String.t, data_source: String.t}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :statement, :string
    field :tables, {:array, :string}
    field :result, :string
    field :execution_time, :integer
    field :users_count, :integer
    field :features, :map
    field :session_id, Ecto.UUID

    belongs_to :user, User
    belongs_to :data_source, DataSource

    timestamps usec: true
  end

  @required_fields ~w()a
  @optional_fields ~w(statement data_source_id tables result execution_time users_count features session_id)a


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

  @doc "Converts the query model to the cloak compliant data."
  @spec to_cloak_query(t) :: cloak_query
  def to_cloak_query(query) do
    %{
      id: query.id,
      statement: query.statement,
      data_source: query.data_source.global_id
    }
  end

  @doc "Produces a JSON blob of the query and it's result for rendering"
  @spec for_display(t) :: %{}
  def for_display(query) do
    query
    |> Map.take([:id, :data_source_id, :statement, :session_id])
    |> Map.merge(result_map(query))
  end

  @doc "Exports the query as CSV"
  @spec to_csv_stream(t) :: Enumerable.t
  def to_csv_stream(%{result: result_json}) do
    result = Poison.decode!(result_json)
    header = result["columns"]
    rows = Enum.flat_map(result["rows"],
      fn(%{"occurrences" => occurrences, "row" => row}) -> List.duplicate(row, occurrences) end)
    CSV.encode([header | rows])
  end

  @doc "Loads the most recent queries for a given user"
  @spec load_recent_queries(User.t, DataSource.t, integer) :: [Query.t]
  def load_recent_queries(user, data_source, recent_count) do
    user
    |> for_user()
    |> for_data_source(data_source)
    |> recent(recent_count)
    |> Repo.all
    |> Enum.map(&for_display/1)
  end


  # -------------------------------------------------------------------
  # Query functions
  # -------------------------------------------------------------------

  @doc "Adds a query filter selecting only those for the given user"
  @spec for_user(Ecto.Queryable.t, User.t) :: Ecto.Queryable.t
  def for_user(query \\ __MODULE__, user) do
    from q in query,
    where: q.user_id == ^user.id
  end

  @doc "Adds a query filter selecting only those for the given user"
  @spec for_data_source(Ecto.Queryable.t, DataSource.t) :: Ecto.Queryable.t
  def for_data_source(query \\ __MODULE__, data_source) do
    from q in query,
    where: q.data_source_id == ^data_source.id
  end

  @doc "Adds a query filter limiting the number of selected queries"
  @spec recent(Ecto.Queryable.t, non_neg_integer) :: Ecto.Queryable.t
  def recent(query \\ __MODULE__, count) do
    from q in query,
    order_by: [desc: q.inserted_at],
    limit: ^count
  end

  @doc """
  Adds a query filter returning recent queries which failed on cloak.

  The queryable returned by this function will select a map with fields
  `id`, `statement`, and `error`.
  """
  @spec failed(Ecto.Queryable.t) :: Ecto.Queryable.t
  def failed(query \\ __MODULE__) do
    from q in query,
    join: ds in assoc(q, :data_source),
    select: %{
      id: q.id,
      inserted_at: q.inserted_at,
      data_source: ds.name,
      statement: q.statement,
      error: fragment("?::json->>'error'", q.result)
    },
    where:
      not is_nil(q.statement) and q.statement != "" and
      q.inserted_at > fragment("(CURRENT_DATE - INTERVAL '7 day')::date") and
      fragment("?::json->>'error' <> ''", q.result),
    order_by: [desc: q.inserted_at]
  end

  @doc "Return the last query made by a user"
  @spec last(Ecto.Queryable.t) :: Ecto.Queryable.t
  def last(query \\ __MODULE__) do
    from q in query,
    order_by: [desc: q.inserted_at],
    limit: 1
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp result_map(%{result: nil}), do: %{rows: [], columns: [], completed: false}
  defp result_map(%{result: result_json}) do
    Poison.decode!(result_json)
    |> Map.put(:completed, true)
  end
end
