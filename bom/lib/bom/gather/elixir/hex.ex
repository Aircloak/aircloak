defmodule BOM.Gather.Elixir.Hex do
  def package(name) do
    case Hex.API.Package.get(name) do
      {200, result, _headers} -> {:ok, result}
      {404, _, _headers} -> {:error, :not_found}
    end
  end
end
