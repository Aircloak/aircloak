defmodule Air.Task do
  @moduledoc "The task model."
  use Air.Web, :model

  alias Air.{User, Result}

  @type t :: %__MODULE__{}
  @type cloak_query :: %{id: String.t, prefetch: [%{table: String.t}], query: String.t}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "tasks" do
    field :name, :string
    field :query, :string
    field :cloak_id, :string
    field :data_source, :string
    field :tables, {:array, :string}
    # Tasks start out as temporary until they are explicitly
    # saved as permanent. A temporary task doesn't show
    # in the list of tasks, and all temporary tasks are automatically
    # cleaned up and removed after a period.
    field :permanent, :boolean

    belongs_to :user, User
    has_many :results, Result

    timestamps
  end

  @required_fields ~w()
  @optional_fields ~w(name query cloak_id data_source tables permanent)


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

  @doc "Returns the task display name."
  @spec display_name(t) :: String.t
  def display_name(task) do
    if task.name == nil do
      "Unnamed task"
    else
      task.name
    end
  end

  @doc "Converts the task model to the cloak compliant data."
  @spec to_cloak_query(t) :: cloak_query
  def to_cloak_query(model) do
    %{
      id: model.id,
      prefetch: Enum.map(model.tables, &%{table: "#{model.data_source}/#{&1}"}),
      query: model.query
    }
  end


  # -------------------------------------------------------------------
  # Task query functions
  # -------------------------------------------------------------------

  def permanent(query) do
    from t in query,
    where: t.permanent == true
  end

  def for_user(query, user) do
    from t in query,
    where: t.user_id == ^user.id
  end


  # -------------------------------------------------------------------
  # Utility functions
  # -------------------------------------------------------------------

  @doc """
  Removes all temporary tasks that are older than a week.
  In this context, a temporary task is one that was added,
  but was never marked as permanent. An example of this would
  be a new task that was never explicitly saved by the user,
  but which never the less got saved to the database.
  """
  def remove_temporary_tasks do
    Air.Repo.delete_all(
      from t in Air.Task,
      where: t.inserted_at < datetime_add(^Ecto.DateTime.utc, -1, "week"),
      where: t.permanent == false
    )
  end
end
