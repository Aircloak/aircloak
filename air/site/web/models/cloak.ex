defmodule Air.Cloak do
  use Air.Web, :model

  schema "cloaks" do
    field :name, :string
    field :state_int, :integer, default: 0

    timestamps
  end

  @required_fields ~w(name)
  @optional_fields ~w(state_int)


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
