defmodule Air.Service.RevokableToken do
  alias Air.Repo
  alias Air.Schemas.RevokableToken
  alias Air.Service.Salts

  def sign(payload, user, type, options \\ []) do
    token = create_token!(payload, user, type, options)
    Phoenix.Token.sign(AirWeb.Endpoint, Salts.get(type), token.id)
  end

  def verify(token, type, options \\ []) do
    with {:ok, id} <- Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(type), token, max_age: :infinity),
         token when not is_nil(token) <- Air.Repo.get(RevokableToken, id) do
      {:ok, :erlang.binary_to_term(token.payload)}
    else
      _ -> {:error, :invalid_token}
    end
  end

  def revoke(token, type) do
    :ok
  end

  defp create_token!(payload, user, type, options) do
    Ecto.build_assoc(user, :revokable_tokens, %{
      type: type,
      payload: :erlang.term_to_binary(payload)
    })
    |> Repo.insert!()
  end
end
