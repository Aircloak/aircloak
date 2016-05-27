defmodule Air.Query do
  @moduledoc "The query model."
  use Air.Web, :model

  alias Air.{User, Result}

  @type t :: %__MODULE__{}
  @type cloak_query :: %{id: String.t, prefetch: [%{table: String.t}], statement: String.t}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "queries" do
    field :statement, :string
    field :cloak_id, :string
    field :data_source, :string
    field :tables, {:array, :string}

    belongs_to :user, User
    has_many :results, Result

    timestamps
  end

  @required_fields ~w()
  @optional_fields ~w(statement cloak_id data_source tables)


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


  # -------------------------------------------------------------------
  # Functions by which to query for a queries
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
end
