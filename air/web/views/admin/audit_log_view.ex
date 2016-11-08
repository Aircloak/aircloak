defmodule Air.Admin.AuditLogView do
  @moduledoc false
  use Air.Web, :view

  import Scrivener.HTML

  def humanize_names(metadata) do
    Enum.map(metadata, fn({name, value}) ->
      {Phoenix.Naming.humanize(name), value}
    end)
  end

  def time_ago(entry), do: Air.Utils.DateTime.time_ago(entry.inserted_at)
end
