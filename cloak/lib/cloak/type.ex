defmodule Cloak.Type do
  defmodule UserId do
    @moduledoc false
    @type t :: String.t
  end

  defmodule Property do
    @moduledoc false
    @type t :: Cloak.DataSource.row
  end

  defmodule Bucket do
    @moduledoc false
    @type t :: {Property.t, [number]}
  end

  defmodule Row do
    @moduledoc false
    @type t :: %{row: Property.t, occurrences: pos_integer}
  end

  defmodule GroupedRows do
    @moduledoc false
    @type t :: %{Property.t => %{User.t => [[Property.t]]}}
  end
end
