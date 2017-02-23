defmodule Central.Schemas.Customer do
  @moduledoc "The customer schema."

  use Central.Web, :model

  alias Ecto.Changeset
  alias Central.Schemas.{Air, Query, Cloak}

  @type t :: %__MODULE__{}

  schema "customers" do
    field :name, :string

    has_many :queries, Query
    has_many :airs, Air

    timestamps()
  end

  @required_fields ~w(name)a
  @optional_fields ~w()a


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  @spec changeset(t | Changeset.t, Map.t) :: Changeset.t
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields ++ @optional_fields)
    |> unique_constraint(:name)
  end

  @doc "Validates a changeset for insertion"
  @spec new_customer_changeset(t | Changeset.t, Map.t) :: Changeset.t
  def new_customer_changeset(model, params \\ %{}) do
    model
    |> changeset(params)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 2)
  end

  @doc "Returns an empty changeset to use in forms"
  @spec empty_changeset() :: Changeset.t
  def empty_changeset() do
    changeset(%__MODULE__{})
  end

  @doc "Returns the list of online airs."
  @spec online_airs(t) :: [Air.t]
  def online_airs(customer), do:
    onlines(customer.airs)

  @doc "Returns the list of all cloaks."
  @spec cloaks(t) :: [Cloak.t]
  def cloaks(customer), do:
    all_cloaks(customer.airs)

  @doc "Returns the list of online cloaks."
  @spec online_cloaks(t) :: [Cloak.t]
  def online_cloaks(customer), do:
    customer
    |> online_airs()
    |> all_cloaks()
    |> onlines()

  @doc "Returns the list of all known data sources."
  @spec data_sources(t) :: [String.t]
  def data_sources(customer), do:
    customer
    |> cloaks()
    |> all_data_sources()
    |> Enum.uniq()

  @doc "Returns the list of online data sources."
  @spec online_data_sources(t) :: [String.t]
  def online_data_sources(customer), do:
    customer
    |> online_cloaks()
    |> all_data_sources()
    |> Enum.uniq()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp all_cloaks(airs), do:
    Enum.flat_map(airs, &(&1.cloaks))

  defp all_data_sources(cloaks), do:
    Enum.flat_map(cloaks, &(&1.data_source_names))

  defp onlines(collection), do:
    Enum.filter(collection, &(&1.status == :online))
end
