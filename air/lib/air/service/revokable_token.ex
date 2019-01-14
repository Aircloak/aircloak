defmodule Air.Service.RevokableToken do
  alias Air.Repo
  alias Air.Schemas.RevokableToken
  alias Air.Service.Salts

  def sign(payload, user, type) do
    token = create_token!(payload, user, type)
    Phoenix.Token.sign(AirWeb.Endpoint, Salts.get(type), token.id)
  end

  def verify(token, type, options \\ []) do
    now = Keyword.get(options, :now, NaiveDateTime.utc_now())
    max_age = Keyword.fetch!(options, :max_age)

    with {:ok, id} <- Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(type), token, max_age: :infinity),
         token when not is_nil(token) <- Air.Repo.get(RevokableToken, id),
         true <- NaiveDateTime.diff(now, token.inserted_at) < max_age do
      {:ok, :erlang.binary_to_term(token.payload)}
    else
      _ -> {:error, :invalid_token}
    end
  end

  def revoke(token, type) do
    :ok
  end

  defp create_token!(payload, user, type) do
    Ecto.build_assoc(user, :revokable_tokens, %{
      type: type,
      payload: :erlang.term_to_binary(payload)
    })
    |> Repo.insert!()
  end
end
