defmodule Air.Schemas.ExplorerAnalysis do
  use Ecto.Schema
  import Ecto.Changeset
  alias Air.Schemas.DataSource
  require EctoEnum

  EctoEnum.defenum(Status, :explorer_status, [:new, :processing, :complete, :canceled, :error])

  schema "explorer_analyses" do
    field(:column, :string)
    field(:job_id, :string)
    field(:last_request, :naive_datetime)
    field(:metrics, :string)
    field(:status, __MODULE__.Status)
    field(:table_name, :string)
    belongs_to(:data_source, DataSource)
  end

  @doc false
  def changeset(explorer_analysis, attrs) do
    explorer_analysis
    |> cast(attrs, [:table_name, :column, :status, :metrics, :job_id, :last_request])
    |> validate_required([:table_name, :column, :status, :metrics, :job_id, :last_request])
  end

  def from_result_json(explorer_analysis, json_struct) do
    explorer_analysis
    |> changeset(%{
      status: String.downcase(json_struct["status"]),
      job_id: json_struct["id"],
      metrics: Jason.encode!(json_struct["metrics"]),
      last_request: NaiveDateTime.utc_now()
    })
  end
end
