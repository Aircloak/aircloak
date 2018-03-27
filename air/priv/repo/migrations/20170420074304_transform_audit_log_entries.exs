defmodule Air.Repo.Migrations.TransformAuditLogEntries do
  use Ecto.Migration

  alias Air.{Repo, Schemas.AuditLog, Schemas.DataSource}

  def up do
    data_sources_by_id =
      Repo.all(DataSource)
      |> Enum.map(&{&1.id, &1})
      |> Enum.into(%{})

    Repo.all(AuditLog)
    |> Enum.each(fn entry ->
      data_source = data_sources_by_id[entry.metadata["data_source"]]

      if data_source do
        params = %{metadata: Map.put(entry.metadata, "data_source", data_source.name)}
        changeset = AuditLog.changeset(entry, params)
        Repo.update!(changeset)
      else
        # There is no data source in the entry, or we couldn't find it.
        # Either the data source has been deleted, or the entry is not related
        # to a data source.
      end
    end)
  end

  def down do
    data_sources_by_name =
      Repo.all(DataSource)
      |> Enum.map(&{&1.name, &1})
      |> Enum.into(%{})

    Repo.all(AuditLog)
    |> Enum.each(fn entry ->
      data_source = data_sources_by_name[entry.metadata["data_source"]]

      if data_source do
        params = %{metadata: Map.put(entry.metadata, "data_source", data_source.id)}
        changeset = AuditLog.changeset(entry, params)
        Repo.update!(changeset)
      else
        # There is no data source in the entry, or we couldn't find it.
        # Either the data source has been deleted, or the entry is not related
        # to a data source.
      end
    end)
  end
end
