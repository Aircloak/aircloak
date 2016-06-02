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
    import Record, only: [defrecord: 2, extract: 2]
    defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")
    @type t :: record(:bucket, property: Property.t, count: pos_integer, noisy_count: pos_integer, users_hash: binary)
  end

  defmacro __using__(_) do
    quote do
      alias User
      alias Property
      alias Bucket
      import Bucket
    end
  end
end
