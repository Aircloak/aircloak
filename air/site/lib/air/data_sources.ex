defmodule Air.DataSource do
  alias Air.{CloakInfo, Token}

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
