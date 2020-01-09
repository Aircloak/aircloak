defmodule Aircloak.Functions do
  @moduledoc """
  Includes the spec definition of the SQL functions Aircloak supports.
  These spec definitions are amongst other things used to generate documentation
  and auto-completion.
  """

  numeric = {:or, [:integer, :real]}

  arithmetic_operation = %{
    [:integer, :integer] => :integer,
    [:real, :integer] => :real,
    [:integer, :real] => :real,
    [:real, :real] => :real
  }

  @deprecated_functions %{
    "div" => %{alternative: "/"}
  }

  @pow_specs %{[numeric, numeric] => :real}

  @add_specs Map.merge(arithmetic_operation, %{
               [:date, :interval] => :datetime,
               [:time, :interval] => :time,
               [:datetime, :interval] => :datetime,
               [:interval, :date] => :datetime,
               [:interval, :time] => :time,
               [:interval, :datetime] => :datetime,
               [:interval, :interval] => :interval
             })
  @sub_specs Map.merge(arithmetic_operation, %{
               [:date, :date] => :interval,
               [:time, :time] => :interval,
               [:datetime, :datetime] => :interval,
               [:date, :interval] => :datetime,
               [:time, :interval] => :time,
               [:datetime, :interval] => :datetime,
               [:interval, :interval] => :interval
             })

  @mul_specs Map.merge(arithmetic_operation, %{
               [:interval, numeric] => :interval,
               [numeric, :interval] => :interval
             })

  @div_specs %{
    [numeric, numeric] => :real,
    [:interval, {:or, [:integer, :real]}] => :interval
  }

  @mod_specs %{[:integer, :integer] => :integer}

  @functions %{
               ~w(count) => %{attributes: [:aggregator], type_specs: %{[:any] => :integer}},
               ~w(count_noise) => %{
                 attributes: [:aggregator],
                 type_specs: %{[:any] => :real}
               },
               ~w(sum) => %{
                 attributes: [:aggregator],
                 type_specs: %{
                   [:integer] => :integer,
                   [:real] => :real
                 }
               },
               ~w(sum_noise) => %{
                 attributes: [:aggregator],
                 type_specs: %{[numeric] => :real}
               },
               ~w(min max) => %{
                 attributes: [:aggregator],
                 type_specs: %{
                   [:integer] => :integer,
                   [:real] => :real,
                   [:date] => :date,
                   [:time] => :time,
                   [:datetime] => :datetime,
                   [:text] => :text,
                   [:unknown] => :unknown
                 }
               },
               ~w(avg stddev variance) => %{
                 attributes: [:aggregator],
                 type_specs: %{[numeric] => :real}
               },
               ~w(avg_noise stddev_noise variance_noise) => %{
                 attributes: [:aggregator],
                 type_specs: %{[numeric] => :real}
               },
               ~w(hour minute second) => %{
                 attributes: [:implicit_range, :restricted],
                 type_specs: %{[{:or, [:datetime, :time]}] => :integer}
               },
               ~w(year quarter month day weekday) => %{
                 attributes: [:implicit_range, :restricted],
                 type_specs: %{[{:or, [:datetime, :date]}] => :integer}
               },
               ~w(date_trunc) => %{
                 attributes: [:implicit_range, :restricted],
                 type_specs: %{
                   [{:constant, :text}, :time] => :time,
                   [{:constant, :text}, :datetime] => :datetime,
                   [{:constant, :text}, :date] => :date
                 }
               },
               ~w(floor ceil) => %{
                 type_specs: %{[numeric] => :integer},
                 attributes: [:math, :restricted]
               },
               ~w(round trunc) => %{
                 attributes: [:implicit_range, :math, :restricted],
                 type_specs: %{
                   [numeric] => :integer,
                   [numeric, {:constant, :integer}] => :real
                 }
               },
               [{:bucket, :lower}, {:bucket, :upper}, {:bucket, :middle}] => %{
                 attributes: [:implicit_range, :restricted],
                 type_specs: %{[numeric, numeric] => :real}
               },
               ~w(abs) => %{
                 type_specs: %{[:real] => :real, [:integer] => :integer},
                 attributes: [:math, :restricted]
               },
               ~w(sqrt) => %{type_specs: %{[numeric] => :real}},
               ~w(^) => %{type_specs: @pow_specs, attributes: [:math]},
               ~w(checked_pow unsafe_pow) => %{type_specs: @pow_specs, attributes: [:math, :unsafe]},
               ~w(+) => %{type_specs: @add_specs, attributes: [:math]},
               ~w(unsafe_add) => %{type_specs: @add_specs, attributes: [:math, :unsafe]},
               ~w(-) => %{type_specs: @sub_specs, attributes: [:math]},
               ~w(unsafe_sub) => %{type_specs: @sub_specs, attributes: [:math, :unsafe]},
               ~w(*) => %{type_specs: @mul_specs, attributes: [:math]},
               ~w(unsafe_mul) => %{type_specs: @mul_specs, attributes: [:math, :unsafe]},
               ~w(/) => %{type_specs: @div_specs, attributes: [:math]},
               ~w(unsafe_div checked_div) => %{type_specs: @div_specs, attributes: [:math, :unsafe]},
               ~w(%) => %{type_specs: @mod_specs, attributes: [:math, :restricted]},
               ~w(unsafe_mod checked_mod) => %{type_specs: @mod_specs, attributes: [:math, :restricted, :unsafe]},
               ~w(length) => %{type_specs: %{[:text] => :integer}, attributes: [:restricted]},
               ~w(lower upper) => %{type_specs: %{[:text] => :text}},
               ~w(left right) => %{
                 type_specs: %{[:text, {:constant, :integer}] => :text},
                 attributes: [:restricted, :string_manipulation]
               },
               ~w(btrim ltrim rtrim) => %{
                 type_specs: %{[:text, {:optional, {:constant, :text}}] => :text},
                 attributes: [:restricted, :string_manipulation]
               },
               ~w(substring) => %{
                 type_specs: %{
                   [:text, {:constant, :integer}, {:optional, {:constant, :integer}}] => :text
                 },
                 attributes: [:restricted, :string_manipulation]
               },
               ~w(concat) => %{type_specs: %{[{:many1, :text}] => :text}},
               ~w(hex) => %{type_specs: %{[:text] => :text}},
               ~w(hash) => %{
                 type_specs: %{[{:or, [:text, :integer, :real]}] => :text}
               },
               [{:cast, :integer}] => %{
                 type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :integer},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :real}] => %{
                 type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :real},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :boolean}] => %{
                 type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :boolean},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :datetime}] => %{
                 type_specs: %{[{:or, [:text, :datetime, :date]}] => :datetime},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :time}] => %{
                 type_specs: %{[{:or, [:text, :datetime, :time]}] => :time},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :date}] => %{
                 type_specs: %{[{:or, [:text, :datetime, :date]}] => :date},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :text}] => %{
                 type_specs: %{[:any] => :text},
                 attributes: [:restricted, :cast]
               },
               [{:cast, :interval}] => %{
                 type_specs: %{[{:or, [:text, :interval]}] => :interval},
                 attributes: [:restricted, :cast]
               },
               ~w(dec_b64) => %{type_specs: %{[:text] => :text}, attributes: [:unsafe]},
               ~w(coalesce) => %{
                 type_specs: %{{:many1, :any} => :any},
                 attributes: [:unsafe]
               },
               ~w(current_datetime) => %{
                 type_specs: %{[] => :datetime},
                 attributes: [:pseudoconstant]
               },
               ~w(current_date) => %{
                 type_specs: %{[] => :date},
                 attributes: [:pseudoconstant]
               },
               ~w(current_time) => %{
                 type_specs: %{[] => :time},
                 attributes: [:pseudoconstant]
               },
               ~w(grouping_id) => %{
                 type_specs: %{[{:many1, :any}] => :integer}
               },
               ~w(case) => %{
                 type_specs: %{[{:many1, :any}] => :any},
                 attributes: [:unsafe]
               },
               ~w(< > = <> >= <=) => %{
                 type_specs: %{
                   [:text, :text] => :boolean,
                   [:numeric, :numeric] => :boolean,
                   [:date, :date] => :boolean,
                   [:date, :text] => :boolean,
                   [:text, :date] => :boolean,
                   [:time, :time] => :boolean,
                   [:time, :text] => :boolean,
                   [:text, :time] => :boolean,
                   [:datetime, :datetime] => :boolean,
                   [:datetime, :text] => :boolean,
                   [:text, :datetime] => :boolean,
                   [:interval, :interval] => :boolean,
                   [:boolean, :boolean] => :boolean
                 },
                 attributes: [:condition]
               },
               ~w(like ilike) => %{
                 type_specs: %{[:text, :like_pattern] => :boolean},
                 attributes: [:condition]
               },
               ~w(and or) => %{
                 type_specs: %{[:boolean, :boolean] => :boolean},
                 attributes: []
               },
               ~w(in) => %{
                 type_specs: %{[{:many1, :any}] => :boolean},
                 attributes: [:condition]
               },
               ~w(is_null) => %{
                 type_specs: %{[:any] => :boolean},
                 attributes: [:condition]
               },
               ~w(not) => %{
                 type_specs: %{[:boolean] => :boolean},
                 attributes: []
               }
             }
             |> Enum.flat_map(fn {functions, traits} -> Enum.map(functions, &{&1, traits}) end)
             |> Enum.into(%{})

  @aliases %{
    "lcase" => "lower",
    "ucase" => "upper",
    "ceiling" => "ceil",
    "pow" => "^",
    "mod" => "%",
    "dow" => "weekday",
    "now" => "current_datetime",
    "current_timestamp" => "current_datetime"
  }

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the spec definitions of the SQL functions supported by Aircloak"
  @spec function_spec() :: Map.t()
  def function_spec(), do: @functions

  @doc "Returns a map from supported aliases to canonical names."
  @spec aliases() :: Map.t()
  def aliases(), do: @aliases

  @doc "Returns a set of deprecated functions along with information on relevant alternatives"
  @spec deprecated_functions() :: Map.t()
  def deprecated_functions(), do: @deprecated_functions
end
