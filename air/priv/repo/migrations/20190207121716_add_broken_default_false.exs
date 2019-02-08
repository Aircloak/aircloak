defmodule Air.Repo.Migrations.AddBrokenDefaultFalse do
  use Ecto.Migration

  def up do
    execute("alter table user_selectables alter column broken set default false")
    execute("update user_selectables set broken = false where broken is NULL")
  end

  def down do
    # NOOP. There was no default set before. false is as good as any other.
  end
end
