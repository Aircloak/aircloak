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

    with {:ok, token} <- find_record(token, type),
         true <- NaiveDateTime.diff(now, token.inserted_at) < max_age do
      {:ok, :erlang.binary_to_term(token.payload)}
    else
      _ -> {:error, :invalid_token}
    end
  end

  def revoke(token, type) do
    with {:ok, token} <- find_record(token, type) do
      Repo.delete!(token)
    end

    :ok
  end

  def revoke_all(user, type) do
    import Ecto.Query

    RevokableToken
    |> where([q], q.user_id == ^user.id)
    |> where([q], q.type == ^type)
    |> Repo.delete_all()

    :ok
  end

  defp create_token!(payload, user, type) do
    Ecto.build_assoc(user, :revokable_tokens, %{
      type: type,
      payload: :erlang.term_to_binary(payload)
    })
    |> Repo.insert!()
  end

  defp find_record(token, type) do
    with {:ok, id} <- Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(type), token, max_age: :infinity) do
      case Air.Repo.get(RevokableToken, id) do
        nil -> {:error, :not_found}
        token -> {:ok, token}
      end
    end
  end
end
