defmodule Cloak.Type do
  defmodule UserId do
    @moduledoc false
    @type t :: String.t
  end

  defmodule Bucket do
    @moduledoc false
    @type t :: %{row: [Cloak.DataSource.field], occurrences: pos_integer}
  end

  defmodule GroupedRows do
    @moduledoc false
    @type t :: %{[Cloak.DataSource.field] => %{User.t => [Cloak.DataSource.Row.t]}}
  end
end
