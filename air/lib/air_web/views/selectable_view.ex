defmodule AirWeb.SelectableView do
  @moduledoc false
  use Air.Web, :view

  def statement(changeset) do
    Ecto.Changeset.get_field(changeset, :sql)
  end

  def selectable_kind_name("analyst_table"), do: "table"
  def selectable_kind_name(name), do: name
end
