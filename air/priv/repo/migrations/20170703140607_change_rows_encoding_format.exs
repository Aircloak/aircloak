defmodule Air.Repo.Migrations.ChangeRowsEncodingFormat do
  use Ecto.Migration

  def up,
    # NULL-ify encoded rows, since they are in the old format (ETF). This is fine,
    # since the ETF format hasn't yet been deployed to customers, so we're the only
    # ones who can lose some results.
    do: Air.Repo.update_all("queries", set: [rows: nil])

  def down do
    # nothing we can do here
    :ok
  end
end
