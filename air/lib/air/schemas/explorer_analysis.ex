defmodule Air.Schemas.ExplorerAnalysis do
  @moduledoc "Represents Diffix Explorer analysis results for each column"
  use Ecto.Schema
  import Ecto.Changeset
  alias Air.Schemas.DataSource
  require EctoEnum

  EctoEnum.defenum(Status, :explorer_status, [:new, :processing, :complete, :canceled, :error])

  schema "explorer_analyses" do
    field(:column, :string)
    field(:job_id, :string)
    field(:metrics, :string)
    field(:status, __MODULE__.Status)
    field(:table_name, :string)
    belongs_to(:data_source, DataSource, on_replace: :delete)

    timestamps(type: :naive_datetime_usec)
  end

  @doc false
  def changeset(explorer_analysis, attrs) do
    explorer_analysis
    |> cast(attrs, [:table_name, :column, :status, :metrics, :job_id])
    |> validate_required([:table_name, :column, :status, :metrics, :job_id])
  end

  def from_result_json(explorer_analysis, json_struct) do
    explorer_analysis
    |> changeset(%{
      status: String.downcase(json_struct["status"]),
      job_id: json_struct["id"],
      metrics: Jason.encode!(json_struct["metrics"])
    })
  end
end
