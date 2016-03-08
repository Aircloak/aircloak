defmodule Air.Repo.Migrations.AddDefaultUser do
  use Ecto.Migration

  def change do
    # We need to bypass model and changeset, since they change with time, but
    # this migration is fixed to the current point. Thus, we're running the
    # insert statement directly.
    %{num_rows: 1} = Ecto.Adapters.SQL.query!(Air.Repo, sql(), params())
  end

  defp sql,
    do: "
      INSERT INTO users(email, hashed_password, name, inserted_at, updated_at)
      VALUES($1, $2, $3, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
    "

  defp params,
    do: ["admin@aircloak.com", Comeonin.Pbkdf2.hashpwsalt("1234"), "Aircloak test user"]
end
