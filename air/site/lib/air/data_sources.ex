defmodule Air.DataSource do
  @moduledoc "Functions for dealing with data sources without caring about cloaks."

  alias Air.{CloakInfo, Token, Organisation}

  @type data_source :: %{
    id: String.t,
    display: String.t,
    tables: [CloakInfo.table],
    token: String.t
  }

  @doc "Returns a flat list of all data sources available to the given organisation."
  @spec all(Organisation.t) :: [data_source]
  def all(organisation) do
    for cloak <- CloakInfo.all(organisation),
        data_source <- cloak.data_sources
    do
      %{
        id: data_source.id,
        display: "#{data_source.id} (#{cloak.name})",
        tables: data_source.tables,
        token: Token.data_source_token(cloak.id, data_source.id)
      }
    end
  end
end
