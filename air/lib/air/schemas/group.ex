defmodule Air.Schemas.Group do
  @moduledoc """
  Groups allows the air system to manage authorization of data sources.
  A group can have many users and likewise give access to many distinct data sources.
  A user in turn can be a member of many groups, and a data source can be made
  available to users of any number of groups. There is therefore a many to many to many
  relationship between users, groups and data sources.
  """
  use Air.Schemas.Base

  alias Air.{Schemas.User, Schemas.DataSource}

  require EctoEnum

  EctoEnum.defenum(GroupSource, :group_source, [:native, :ldap])

  @type t :: %__MODULE__{}

  schema "groups" do
    field(:name, :string)
    field(:source, __MODULE__.GroupSource)
    field(:ldap_dn, :string)
    field(:admin, :boolean)

    many_to_many(
      :users,
      User,
      join_through: "groups_users",
      on_delete: :delete_all,
      on_replace: :delete
    )

    many_to_many(
      :data_sources,
      DataSource,
      join_through: "data_sources_groups",
      on_delete: :delete_all,
      on_replace: :delete
    )

    timestamps()
  end
end
