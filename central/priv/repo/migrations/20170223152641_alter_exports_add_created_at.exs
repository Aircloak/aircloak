defmodule Central.Repo.Migrations.AlterExportsAddCreatedAt do
  use Ecto.Migration

  def up() do
    alter table(:customer_exports) do
      # use default to populate existing fields
      add(:created_at, :naive_datetime, null: false, default: fragment("now()"))
    end

    alter table(:customer_exports) do
      # remove the default, so we need to explicitly provide it
      modify(:created_at, :naive_datetime, default: nil)
    end
  end

  def down() do
    alter table(:customer_exports) do
      remove(:created_at)
    end
  end
end
