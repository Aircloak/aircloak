defmodule AirWeb.ExplorerAnalysisView do
  @moduledoc false
  use Air.Web, :view

  def status_to_badge_class(status) do
    case status do
      :new -> "badge-primary"
      :pending -> "badge-info"
      :complete -> "badge-success"
      :error -> "badge-danger"
      :canceled -> "badge-secondary"
    end
  end

  def metrics(analysis) do
    Enum.map(Jason.decode!(analysis.metrics), fn %{"name" => k, "value" => v} -> {k, v} end)
  end

  def to_pretty_json(val) do
    Jason.encode!(val, pretty: true)
  end
end
