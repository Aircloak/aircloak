defmodule Air.Schemas.Query do
  @moduledoc "The query schema."
  use Air.Schemas.Base

  alias Air.PsqlServer.Protocol
  alias Air.Schemas.{DataSource, User, ResultChunk}

  require EctoEnum

  @states [
    :created,
    :started,
    :parsing,
    :compiling,
    :awaiting_data,
    :ingesting_data,
    :processing,
    :post_processing,
    :completed,
    :error,
    :cancelled
  ]

  # The last argument to `EctoEnum.defenum` needs to be a list literal
  quote do
    EctoEnum.defenum(QueryState, :query_state, unquote(@states))
  end
  |> Code.eval_quoted([], __ENV__)

  EctoEnum.defenum(Context, :query_context, [:http, :psql, :api])

  @inherited_pkey_name :tasks_pkey

  @type id :: String.t()
  @type statement :: String.t()
  @type parameters :: Map.t()
  @type session_id :: Ecto.UUID
  @type t :: %__MODULE__{}
  @type cloak_query :: %{
          id: id,
          statement: statement,
          parameters: [Protocol.db_value()],
          data_source: String.t(),
          views: %{String.t() => String.t()},
          query_state: __MODULE__.QueryState.t(),
          context: __MODULE__.Context.t()
        }

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field(:statement, :string)
    field(:tables, {:array, :string})
    field(:execution_time, :integer)
    field(:users_count, :integer)
    field(:features, :map)
    field(:session_id, Ecto.UUID)
    field(:parameters, :map)
    field(:cloak_id, :string)
    field(:query_state, __MODULE__.QueryState)
    field(:context, Context)
    field(:result, :map)
    field(:time_spent, :map, default: @states |> Enum.map(&{to_string(&1), 0}) |> Enum.into(%{}))
    field(:last_state_change_at, :naive_datetime)

    belongs_to(:user, User)
    belongs_to(:data_source, DataSource)
    has_many(:result_chunks, ResultChunk)

    timestamps(usec: true)
  end

  @required_fields ~w()a
  @optional_fields ~w(
    cloak_id statement data_source_id tables execution_time users_count features session_id parameters query_state
    context result last_state_change_at time_spent
  )a

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, Map.t()) :: Ecto.Changeset.t()
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:data_source_id)
  end

  @doc "Adds a pre-created ID to a query changeset."
  @spec add_id_to_changeset(Ecto.Changeset.t(), String.t()) :: Ecto.Changeset.t()
  def add_id_to_changeset(model, id) do
    model
    |> cast(%{id: id}, [:id])
    |> unique_constraint(:id, name: @inherited_pkey_name)
  end

  @doc "Exports the query as CSV"
  @spec to_csv_stream(t, Enumerable.t()) :: Enumerable.t()
  def to_csv_stream(query, buckets),
    do:
      [query.result["columns"]]
      |> Stream.concat(ResultChunk.rows_stream(buckets))
      |> CSV.encode()
end
