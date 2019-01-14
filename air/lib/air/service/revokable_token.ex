defmodule Air.Service.RevokableToken do
  alias Air.Repo
  alias Air.Schemas.RevokableToken
  alias Air.Service.Salts

  def sign(payload, user, type, options \\ []) do
    payload = Phoenix.Token.sign(AirWeb.Endpoint, Salts.get(type), payload)
    token = create_token!(payload, user, type, options)
    token.id
  end

  def verify(token_id, type, options \\ []) do
    with token when not is_nil(token) <- Air.Repo.get(RevokableToken, token_id),
         {:ok, payload} <- Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(type), token.payload, max_age: :infinity) do
      {:ok, payload}
    else
      _ -> {:error, :invalid_token}
    end
  end

  def revoke(token, type) do
    :ok
  end

  defp create_token!(payload, user, type, options) do
    Ecto.build_assoc(user, :revokable_tokens, type: type, payload: payload)
    |> Repo.insert!()
  end
end
