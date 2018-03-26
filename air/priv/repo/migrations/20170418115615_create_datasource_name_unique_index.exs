defmodule DataSource do
  use Air.Schemas.Base

  schema "data_sources" do
    field(:name, :string)
    timestamps()
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, [:name])
    |> validate_required([:name])
  end
end

defmodule Air.Repo.Migrations.CreateDatasourceNameUniqueIndex do
  use Ecto.Migration

  alias Air.Repo

  def change do
    Repo.all(DataSource)
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn
      {_name, [_data_source]} ->
        :ok

      {name, data_sources} ->
        # There are multiple data sources with the same name.
        # This is an issue, as adding the unique_index is going to fail.
        # We therefore forcefully alter the names, to make them unique.
        Enum.zip(data_sources, 1..length(data_sources))
        |> Enum.each(fn {data_source, index} ->
          Air.Service.DataSource.update!(data_source, %{name: "#{name} #{index}"})
        end)
    end)

    create(unique_index(:data_sources, [:name]))
  end
end
