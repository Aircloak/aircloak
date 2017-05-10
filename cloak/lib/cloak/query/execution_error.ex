defmodule Cloak.Query.ExecutionError do
  @moduledoc """
  An error that occurred while processing the query.

  This error can be used to signal an error that will be caught by the query engine
  and reported to the user.

  You're advised to not overuse this mechanism. However, sometimes it can be
  quite complicated to bubble up an error from a deep nested stack of maps,
  reduces, and other transformations. In such cases, you can raise this error
  with a descriptive message which will be reported to the end-user.
  """

  defexception [:message]
end
