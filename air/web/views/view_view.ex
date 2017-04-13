defmodule Air.ViewView do
  @moduledoc false
  use Air.Web, :view

  def statement(changeset) do
    Ecto.Changeset.get_field(changeset, :sql)
  end
end
