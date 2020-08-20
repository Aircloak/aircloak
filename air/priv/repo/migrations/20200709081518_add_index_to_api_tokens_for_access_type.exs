defmodule Air.Repo.Migrations.AddIndexToApiTokensForAccessType do
  use Ecto.Migration

  def change do
    create(index(:api_tokens, [:id, :access]))
  end
end
