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
    belongs_to :user, User

    timestamps
  end

  @required_fields ~w()
  @optional_fields ~w(name query)

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
end
