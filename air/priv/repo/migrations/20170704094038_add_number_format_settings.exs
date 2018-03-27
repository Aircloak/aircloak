defmodule Air.Repo.Migrations.AddNumberFormatToUsers do
  use Ecto.Migration

  def up do
    alter table(:users) do
      add(:decimal_sep, :string, size: 1)
      add(:thousand_sep, :string, size: 1)
      add(:decimal_digits, :integer)
    end

    alter table(:settings) do
      add(:decimal_sep, :string, default: ".", size: 1, null: false)
      add(:thousand_sep, :string, default: " ", size: 1)
      add(:decimal_digits, :integer, default: 3, null: false)
    end
  end

  def down do
    alter table(:users) do
      remove(:decimal_sep)
      remove(:thousand_sep)
      remove(:decimal_digits)
    end

    alter table(:settings) do
      remove(:decimal_sep)
      remove(:thousand_sep)
      remove(:decimal_digits)
    end
  end
end
