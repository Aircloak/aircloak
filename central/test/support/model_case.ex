defmodule Central.ModelCase do
  @moduledoc """
  This module defines the test case to be used by
  model tests.

  You may define functions here to be used as helpers in
  your model tests. See `errors_on/2`'s definition as reference.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      alias Central.Repo

      import Ecto
      import Ecto.Changeset
      import Ecto.Query, only: [from: 1, from: 2]
      import Central.ModelCase
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Central.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(Central.Repo, {:shared, self()})
    end

    :ok
  end

  @doc """
  Helper for returning list of errors in model when passed certain data.

  ## Examples

  Given a User model that lists `:name` as a required field and validates
  `:password` to be safe, it would return:

      iex> errors_on(%User{}, %{password: "password"})
      [password: "is unsafe", name: "is blank"]

  You could then write your assertion like:

      assert {:password, "is unsafe"} in errors_on(%User{}, %{password: "password"})

  You can also create the changeset manually and retrieve the errors
  field directly:

      iex> changeset = User.changeset(%User{}, password: "password")
      iex> {:password, "is unsafe"} in changeset.errors
      true
  """
  def errors_on(struct, data) do
    struct.__struct__.changeset(struct, data)
    |> Ecto.Changeset.traverse_errors(&CentralWeb.ErrorHelpers.translate_error/1)
    |> Enum.flat_map(fn {key, errors} -> for msg <- errors, do: {key, msg} end)
  end

  @doc """
  Helper for validating that there is an error on a particular field.
  The error itself doesn't matter that much, as it is too implementation
  dependent. Instead, all we care about is that there is in fact an error.

  ## Examples

  Given a User model that lists `:name` as a required field of more than
  5 characters, the following would validate that an error is in the changeset.

      iex> errors_on(%User{}, :name, %{name: "short"})
      true

  You could then write your assertion like:

      assert errors_on(%User{}, :name, %{name: "short"})
  """
  def errors_on(model, field, data) do
    errors = errors_on(model, data)
    Keyword.has_key?(errors, field)
  end
end
