defmodule Cloak.Query.Error do
  @moduledoc """
  Holds data related to a query execution failure that can be used to provide more
  context to the error in the air as well as in central.
  """

  alias Cloak.Query.Error

  @type error_type :: :unspecified | :ambiguous | :incomplete | :invalid | :type_error | :crash | :illegal

  @type t :: %__MODULE__{
    type: error_type,
    # The context in which it happened.
    context: String.t,
    # In which module the error happened.
    location: atom,
    # A description that can be shown to the analyst to allow them to debug the problem,
    # or deside what to do next.
    human_description: String.t,
  }

  defstruct type: :unspecified, context: "not specified", location: :unspecified,
    human_description: "Not specified"


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Convenience method for creating an unknown cloak error struct."
  @spec unknown_cloak_error() :: %Error{}
  def unknown_cloak_error(context \\ "not specified", location \\ :unspecified), do:
    %Error{
      type: :crash,
      context: context,
      location: location,
      human_description: "Unknown cloak error.",
    }

  @doc "Converts the error to a map that can be reported sent to air"
  @spec to_map(t) :: Map.t
  def to_map(error), do:
    %{
      type: error.type,
      context: error.context,
      location: error.location,
      human_description: error.human_description,
    }
end
