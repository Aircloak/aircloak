defmodule Air.Schemas.DataSource do
  @moduledoc "Represents data sources made available through the cloaks"
  use Air.Schemas.Base

  alias Air.Schemas.Group

  @type t :: %__MODULE__{}

  schema "data_sources" do
    field :global_id, :string
    field :name, :string
    field :description, :string
    field :tables, :string
    field :errors, :string

    has_many :queries, Air.Schemas.Query
    many_to_many :groups, Group,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete

    timestamps()
  end
end
