defmodule Air.Cloak do
  @moduledoc "Contains a record of connected cloak's, and their online status."
  use Air.Web, :model

  alias Air.{Repo, Cloak, DataSource}

  @type t :: %__MODULE__{}

  schema "cloaks" do
    field :name, :string
    field :state_int, :integer, default: 0
    field :raw_pid, :string

    has_many :data_sources, DataSource

    timestamps
  end

  @required_fields ~w(name)
  @optional_fields ~w(state_int raw_pid)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    params = convert_state_param(params)
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def state(model) do
    int_to_state(model.state_int)
  end

  @doc """
  Registers a cloak and sets marks it as online.
  If the cloak already exists, then it's online status is updated,
  rather than a new cloak being created.
  """
  @spec register(String.t, [Map.t], pid | nil) :: Cloak.t
  def register(name, data_sources, channel_pid \\ nil) do
    params = %{name: name, state: :online, raw_pid: encode_data(channel_pid)}
    cloak = case Repo.one(from c in Cloak, where: c.name == ^name) do
      nil ->
        %Cloak{}
        |> Cloak.changeset(params)
        |> Repo.insert!
      cloak ->
        Cloak.changeset(cloak, params)
        |> Repo.update!
    end
    Enum.each(data_sources, &DataSource.register(cloak, &1))
    cloak
  end

  @doc """
  Marks a registered cloak as offline.
  Raises if no cloak exists under the name.
  """
  @spec unregister!(String.t) :: :ok
  def unregister!(name) do
    params = %{name: name, state: :offline, raw_pid: encode_data(nil)}
    case Repo.one(from c in Cloak, where: c.name == ^name) do
      nil ->
        raise RuntimeError, message: "Tried unregistering unknown cloak: #{name}"
      cloak ->
        Cloak.changeset(cloak, params)
        |> Repo.update!
    end
  end

  @doc "Returns the pid of the cloak channel if it exists, or nil"
  @spec channel_pid(Cloak.t) :: pid | nil
  def channel_pid(cloak) do
    decode_data(cloak.raw_pid)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp convert_state_param(:empty), do: :empty
  defp convert_state_param(params) do
    case params[:state] do
      nil -> params
      state_val -> Map.merge(params, %{state_int: state_to_int(state_val)})
    end
  end

  defp decode_data(nil), do: nil
  defp decode_data(data) do
    data
    |> Base.decode64!()
    |> :erlang.binary_to_term()
  end

  defp encode_data(nil), do: nil
  defp encode_data(data) do
    data
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  @state_map %{0 => :unknown, 1 => :online, 2 => :offline}

  defp state_to_int(state) do
    @state_map
    |> Enum.to_list()
    |> Enum.find_value(fn
      ({number, ^state}) -> number
      (_) -> nil
    end)
  end

  defp int_to_state(number) do
    @state_map[number]
  end
end
