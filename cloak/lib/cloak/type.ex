defmodule Cloak.Type do
  defmodule User do
    @moduledoc false
    @type id :: binary
  end

  defmodule Property do
    @moduledoc false
    @type t :: [binary] | binary
  end
end
