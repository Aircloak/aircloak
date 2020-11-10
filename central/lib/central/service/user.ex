defmodule Central.Service.User do
  @moduledoc "Service module for working with users"

  alias Ecto.Changeset
  import Ecto.Query, only: [where: 3]
  alias Central.Repo
  alias Central.Schemas.User

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Authenticates the given user."
  @spec login(String.t(), String.t()) :: {:ok, User.t()} | {:error, :invalid_email_or_password}
  def login(email, password) do
    normalized_email = String.downcase(email)

    user =
      User
      |> where([user], fragment("lower(?)", user.email) == ^normalized_email)
      |> Repo.one()

    case validate_password(user, password) do
      :ok -> {:ok, user}
      error -> error
    end
  end

  @doc "Returns all registered users"
  @spec all() :: [User.t()]
  def all() do
    Repo.all(User)
  end

  @doc "Creates a user"
  @spec create(Map.t()) :: {:ok, User.t()} | {:error, Changeset.t()}
  def create(params) do
    changeset = User.new_user_changeset(%User{}, params)
    Repo.insert(changeset)
  end

  @doc "Updates a user"
  @spec update(User.t(), Map.t()) :: {:ok, User.t()} | {:error, Changeset.t()}
  def update(user, params) do
    changeset = User.changeset(user, params)
    Repo.update(changeset)
  end

  @doc "Removes a user, silently fails"
  @spec delete(User.t()) :: :ok | :error
  def delete(user) do
    case Repo.delete(user) do
      {:ok, _} -> :ok
      {:error, _changeset} -> :error
    end
  end

  @doc "Returns a user by user id"
  @spec get(non_neg_integer) :: {:ok, User.t()} | {:error, :not_found}
  def get(id) do
    case Repo.get(User, id) do
      nil -> {:error, :not_found}
      user -> {:ok, user}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp validate_password(user, password) do
    if User.validate_password(user, password) do
      :ok
    else
      {:error, :invalid_email_or_password}
    end
  end
end
