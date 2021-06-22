defmodule Air.Service.RevokableToken do
  @moduledoc """
  This service provides a way of giving the user a token that we can later verify for authenticity and expiry and that
  is associated with some arbitrary data.

  The tokens are stored in the database to make it possible to revoke them on the server side. This is in contrast to
  the default Phoenix.Token tokens for example, where we can be sure of the token's authenticity, but there is no way to
  mark the token as invalid later on. An example use might be tokens that allow a certain action, but only a single time
  (like resetting a password). The drawback of this approach is that each creation or verification of a token requires a
  database query.

  The token given out to the user is the id of the actual token in the database signed with `Phoenix.Token`. This is
  in order to ascertain that the user has indeed been given the token by us, instead of guessing it. The ids are UUIDs
  and it might have been enough to give the UUID directly, because they are essentially long random strings, however,
  the UUID implementation gives no guarantees of cryptographic security (as far as I know), while `Phoenix.Token` is
  available for exactly this purpose.

  The tokens are always associated with a user to facilitate their cleanup once a user is deleted.
  """

  alias Air.Repo
  alias Air.Schemas.{User, RevokableToken}
  alias Air.Service.Salts

  import Ecto.Query

  @type options :: [now: NaiveDateTime.t()]
  @type seconds :: non_neg_integer() | :infinity

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns a new token of the given type for the user."
  @spec sign(term(), User.t(), RevokableToken.RevokableTokenType.t(), seconds()) :: String.t()
  def sign(payload, user, type, valid_for_seconds) do
    token = create_token!(payload, user, type, valid_for_seconds)
    Phoenix.Token.sign(AirWeb.Endpoint, Salts.get(type), token.id)
  end

  @doc """
  Decodes the provided token.

  Checks that:

  1. It's a valid token we have given out.
  2. The token has not expired according to the provided `max_age`.
  3. The token has not been revoked.

  Returns `{:ok, data}` for valid tokens and `{:error, :invalid_token}` otherwise.
  """
  @spec verify(String.t(), RevokableToken.RevokableTokenType.t(), options()) :: {:ok, term()} | {:error, :invalid_token}
  def verify(token, type, options \\ []) do
    now = Keyword.get(options, :now, NaiveDateTime.utc_now())

    with {:ok, token} <- find_record(token, type),
         :ok <- verify_age(token, now) do
      {:ok, :erlang.binary_to_term(token.payload)}
    else
      _ -> {:error, :invalid_token}
    end
  end

  @doc "Marks the given token as revoked."
  @spec revoke(String.t(), RevokableToken.RevokableTokenType.t()) :: :ok
  def revoke(token, type) do
    with {:ok, token} <- find_record(token, type) do
      Repo.delete!(token)
    end

    :ok
  end

  @doc """
  Atomically verifies and revokes the token.

  Note that the revocation is a database operation, so if you rollback a transaction including this operation you will
  also undo the revocation.
  """
  @spec verify_and_revoke(String.t(), RevokableToken.RevokableTokenType.t()) :: {:ok, term()} | {:error, :invalid_token}
  def verify_and_revoke(token, type) do
    :global.trans({__MODULE__, self()}, fn ->
      result = verify(token, type)
      revoke(token, type)
      result
    end)
  end

  @doc "Revokes all of the given user's tokens with the given type."
  @spec revoke_all(User.t(), RevokableToken.RevokableTokenType.t()) :: :ok
  def revoke_all(user, type) do
    token_scope(user, type) |> Repo.delete_all()
    :ok
  end

  @doc "Returns the number of stored tokens with the given user and type."
  @spec count(User.t(), RevokableToken.RevokableTokenType.t()) :: non_neg_integer
  def count(user, type) do
    token_scope(user, type) |> Repo.aggregate(:count, :id)
  end

  @doc "Revokes all tokens that have a validity date in the past."
  @spec cleanup() :: :ok
  def cleanup(now \\ NaiveDateTime.utc_now()) do
    RevokableToken
    |> where([q], q.valid_until < ^now)
    |> Repo.delete_all()

    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp verify_age(token, now), do: if(NaiveDateTime.diff(now, token.valid_until) <= 0, do: :ok, else: :error)

  defp create_token!(payload, user, type, valid_for_seconds) do
    Ecto.build_assoc(user, :revokable_tokens, %{
      type: type,
      payload: :erlang.term_to_binary(payload),
      valid_until: valid_for_seconds |> valid_until() |> NaiveDateTime.truncate(:second)
    })
    |> Repo.insert!()
  end

  defp valid_until(:infinity), do: ~N[9999-12-12 23:59:59]
  defp valid_until(seconds), do: NaiveDateTime.utc_now() |> NaiveDateTime.add(seconds)

  defp find_record(token, type) do
    with {:ok, id} <- Phoenix.Token.verify(AirWeb.Endpoint, Salts.get(type), token, max_age: :infinity) do
      case Air.Repo.get(RevokableToken, id) do
        nil -> {:error, :not_found}
        token -> {:ok, token}
      end
    end
  end

  defp token_scope(user, type) do
    RevokableToken
    |> where([q], q.user_id == ^user.id)
    |> where([q], q.type == ^type)
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Periodic.child_spec(run: &cleanup/0, every: :timer.hours(1), overlap?: false)
  end
end
