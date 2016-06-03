defmodule Air.Query do
  @moduledoc "The query model."
  use Air.Web, :model

  alias Air.User

  @type t :: %__MODULE__{}
  @type cloak_query :: %{id: String.t, prefetch: [%{table: String.t}], statement: String.t}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :statement, :string
    field :cloak_id, :string
    field :data_source, :string
    field :tables, {:array, :string}
    field :result, :string

    belongs_to :user, User

    timestamps
  end

  @required_fields ~w()
  @optional_fields ~w(statement cloak_id data_source tables result)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t, %{binary => term} | %{atom => term} | :empty) :: Ecto.Changeset.t
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  @doc "Converts the query model to the cloak compliant data."
  @spec to_cloak_query(t) :: cloak_query
  def to_cloak_query(model) do
    %{
      id: model.id,
      prefetch: Enum.map(model.tables, &%{table: "#{model.data_source}/#{&1}"}),
      statement: model.statement
    }
  end

  @doc "Produces a JSON blob of the query and it's result for rendering"
  @spec for_display(t, [{atom, any}]) :: %{}
  def for_display(query, options \\ []) do
    base_query = %{
      statement: query.statement,
      id: query.id
    }
    Map.merge(base_query, result_map(query, options[:complete] || false))
  end


  # -------------------------------------------------------------------
  # Query functions
  # -------------------------------------------------------------------

  @doc "Adds a query filter selecting only those for the given user"
  @spec for_user(__MODULE__, User.t) :: __MODULE__
  def for_user(query \\ __MODULE__, user) do
    from q in query,
    where: q.user_id == ^user.id
  end

  @doc "Adds a query filter limiting the number of selected queries"
  @spec recent(__MODULE__, non_neg_integer) :: __MODULE__
  def recent(query \\ __MODULE__, count) do
    from q in query,
    order_by: [desc: q.inserted_at],
    limit: ^count
  end

  @doc "Adds a query filter limiting the returned queries to that with a given ID"
  @spec with_id(__MODULE__, String.t) :: __MODULE__
  def with_id(query \\ __MODULE__, id) do
    from q in query,
    where: q.id == ^id
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp result_map(%{result: nil}, _complete), do: %{rows: [], columns: []}
  defp result_map(%{result: result_json}, complete) do
    result = Poison.decode!(result_json)
    {rows, row_count} = case result["rows"] do
      nil -> {[], 0}
      rows ->
        if complete do
          {rows, length(rows)}
        else
          {Enum.take(rows, 10), length(rows)}
        end
    end

    %{
      columns: result["columns"],
      rows: rows,
      error: result["error"],
      row_count: row_count,
    }
  end
end
