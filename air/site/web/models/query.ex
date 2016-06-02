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
  @spec for_display(t) :: %{}
  def for_display(query) do
    base_query = %{
      statement: query.statement,
      id: query.id
    }
    Map.merge(base_query, result_map(query))
  end


  # -------------------------------------------------------------------
  # Query functions
  # -------------------------------------------------------------------

  def for_user(query \\ __MODULE__, user) do
    from t in query,
    where: t.user_id == ^user.id
  end

  def recent(query \\ __MODULE__, count) do
    from t in query,
    order_by: [desc: t.inserted_at],
    limit: ^count
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp result_map(%{result: nil}), do: %{rows: [], columns: []}
  defp result_map(%{result: result_json}) do
    result = Poison.decode!(result_json)

    %{
      columns: result["columns"],
      rows: result["rows"],
      error: result["error"]
    }
  end
end
