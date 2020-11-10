defmodule Air.Schemas.ExplorerAnalysis do
  @moduledoc "Represents Diffix Explorer analysis results for each column"
  use Ecto.Schema
  import Ecto.Changeset
  alias Air.Schemas.DataSource
  require EctoEnum

  EctoEnum.defenum(Status, :explorer_status, [:new, :processing, :complete, :canceled, :error])

  schema "explorer_analyses" do
    field(:job_id, :string)
    field(:results, :map, default: %{"columns" => [], "sampleData" => []})
    field(:status, __MODULE__.Status)
    field(:table_name, :string)
    field(:errors, {:array, :string}, default: [])
    field(:version, :string)
    field(:soft_delete, :boolean, default: false)
    belongs_to(:data_source, DataSource, on_replace: :delete)

    timestamps(type: :naive_datetime_usec)
  end

  @doc false
  def changeset(explorer_analysis, attrs) do
    explorer_analysis
    |> cast(attrs, [:table_name, :status, :results, :job_id, :errors, :version])
    |> validate_required([:table_name, :status])
  end

  def from_decoded_result_json(explorer_analysis, json_struct) do
    explorer_analysis
    |> changeset(%{
      status: String.downcase(json_struct["status"]),
      job_id: json_struct["id"],
      results: Map.take(json_struct, ["columns", "sampleData"]),
      errors: json_struct["errors"] || [],
      version: get_in(json_struct, ["versionInfo", "commitHash"])
    })
  end
end
