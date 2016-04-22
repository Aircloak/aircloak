defmodule Air.Task do
  @moduledoc "The task model."
  use Air.Web, :model

  alias Air.User

  @type t :: %__MODULE__{}
  @type result_row :: %{:label => String.t, :value => String.t, :count => integer}
  @type result :: %{:data => [result_row], :created_at => integer}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "tasks" do
    field :name, :string
    field :query, :string
    # Tasks start out as temporary until they are explicitly
    # saved as permanent. A temporary task doesn't show
    # in the list of tasks, and all temporary tasks are automatically
    # cleaned up and removed after a period.
    field :permanent, :boolean

    belongs_to :user, User

    timestamps
  end

  @required_fields ~w()
  @optional_fields ~w(name query permanent)

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

  def display_name(task) do
    if task.name == nil do
      "Unnamed task"
    else
      task.name
    end
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
