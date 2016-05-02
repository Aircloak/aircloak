defmodule Air.ErlangLogger do
  @moduledoc false
  # TODO: replace this temp workaround by extracting Erlang -> Logger bridge from cloak
  #       to common, and reusing it here.

  require Logger

  defdelegate error(msg), to: Logger
end
