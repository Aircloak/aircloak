defmodule Cloak.Sql.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.Sql.{Expression, Parser}
  alias Cloak.DataSource

  import Kernel, except: [apply: 2]

  numeric = {:or, [:integer, :real]}
  arithmetic_operation = %{
    [:integer, :integer] => :integer,
    [:real, :integer] => :real,
    [:integer, :real] => :real,
    [:real, :real] => :real,
  }

  @functions %{
    ~w(count) => %{attributes: [:aggregator], type_specs: %{[:any] => :integer}},
    ~w(count_noise) => %{attributes: [:aggregator, :not_in_subquery], type_specs: %{[:any] => :real}},
    ~w(sum) => %{attributes: [:aggregator], type_specs: %{
      [:integer] => :integer,
      [:real] => :real,
    }},
    ~w(sum_noise) => %{attributes: [:aggregator, :not_in_subquery], type_specs: %{[numeric] => :real}},
    ~w(median) => %{attributes: [:aggregator, :emulated], type_specs: %{
      [:integer] => :integer,
      [:real] => :real,
      [:date] => :date,
      [:time] => :time,
      [:datetime] => :datetime,
      [:text] => :text,
    }},
    ~w(min max) => %{attributes: [:aggregator], type_specs: %{
      [:integer] => :integer,
      [:real] => :real,
      [:date] => :date,
      [:time] => :time,
      [:datetime] => :datetime,
      [:text] => :text,
    }},
    ~w(avg stddev) => %{attributes: [:aggregator], type_specs: %{[numeric] => :real}},
    ~w(avg_noise stddev_noise) => %{attributes: [:aggregator, :not_in_subquery], type_specs: %{[numeric] => :real}},
    ~w(hour minute second) =>
      %{type_specs: %{[{:or, [:datetime, :time]}] => :integer}},
    ~w(year quarter month day weekday) =>
      %{type_specs: %{[{:or, [:datetime, :date]}] => :integer}},
    ~w(date_trunc) => %{type_specs: %{
      [{:constant, :text}, :time] => :time,
      [{:constant, :text}, {:or, [:datetime, :date]}] => :datetime
    }},
    ~w(floor ceil ceiling) => %{type_specs: %{[numeric] => :integer}},
    ~w(round trunc) => %{type_specs: %{
      [numeric] => :integer,
      [numeric, :integer] => :real,
    }},
    [{:bucket, :lower}, {:bucket, :upper}, {:bucket, :middle}] => %{type_specs: %{
      [numeric, numeric] => :real,
    }},
    ~w(abs) => %{type_specs: %{[:real] => :real, [:integer] => :integer}},
    ~w(sqrt) => %{type_specs: %{[numeric] => :real}},
    ~w(div mod %) => %{type_specs: %{[:integer, :integer] => :integer}},
    ~w(pow ^) => %{type_specs: %{[numeric, numeric] => :real}},
    ~w(+) => %{type_specs: Map.merge(arithmetic_operation, %{
      [:date, :interval] => :datetime,
      [:time, :interval] => :time,
      [:datetime, :interval] => :datetime,
      [:interval, :date] => :datetime,
      [:interval, :time] => :time,
      [:interval, :datetime] => :datetime,
      [:interval, :interval] => :interval,
    })},
    ~w(-) => %{type_specs: Map.merge(arithmetic_operation, %{
      [:date, :date] => :interval,
      [:time, :time] => :interval,
      [:datetime, :datetime] => :interval,
      [:date, :interval] => :datetime,
      [:time, :interval] => :time,
      [:datetime, :interval] => :datetime,
      [:interval, :interval] => :interval,
    })},
    ~w(*) => %{type_specs: Map.merge(arithmetic_operation, %{
       [:interval, numeric] => :interval,
       [numeric, :interval] => :interval,
     })},
    ~w(/) => %{type_specs: %{
      [numeric, numeric] => :real,
      [:interval, {:or, [:integer, :real]}] => :interval,
    }},
    ~w(length) => %{type_specs: %{[:text] => :integer}},
    ~w(lower lcase upper ucase) => %{type_specs: %{[:text] => :text}},
    ~w(left right) => %{type_specs: %{[:text, :integer] => :text}},
    ~w(btrim ltrim rtrim) => %{type_specs: %{[:text, {:optional, :text}] => :text}},
    ~w(substring substring_for) =>
      %{type_specs: %{[:text, :integer, {:optional, :integer}] => :text}},
    ~w(||) => %{type_specs: %{[:text, :text] => :text}},
    ~w(concat) => %{type_specs: %{[{:many1, :text}] => :text}},
    ~w(hex) => %{type_specs: %{[:text] => :text}},
    ~w(hash) => %{type_specs: %{[:text] => :integer, [:integer] => :integer, [:real] => :integer}},
    # NOTICE: The `not_in_subquery` needs to be set for `extract_match` and `extract_matches`
    # (whether or not we can implement it in a subquery). The reason for this is what we have called:
    # WYSIWYC (what you see is what you count). If `extract_match` is allowed in a subquery it can
    # effectively be used as an `OR`, whereas if it is used at the top-level the output of
    # the function is directly anonymised, and hence safe.
    # An example attack could look like:
    #
    #   SELECT count(*)
    #   FROM (
    #     SELECT uid, extract_match(name, 'Pattern1|Pattern2|Pattern3|...') as match
    #     FROM table
    #     WHERE match is not null
    #   )
    #
    ~w(extract_match) => %{type_specs: %{[:text, :text] => :text},
      attributes: [:not_in_subquery, :precompiled, :emulated]},
    ~w(extract_matches) => %{type_specs: %{[:text, :text] => :text},
      attributes: [:not_in_subquery, :precompiled, :row_splitter, :emulated]},
    [{:cast, :integer}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :integer}},
    [{:cast, :real}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :real}},
    [{:cast, :boolean}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :boolean}},
    [{:cast, :datetime}] =>
      %{type_specs: %{[{:or, [:text, :datetime]}] => :datetime}},
    [{:cast, :time}] =>
      %{type_specs: %{[{:or, [:text, :datetime, :time]}] => :time}},
    [{:cast, :date}] =>
      %{type_specs: %{[{:or, [:text, :datetime, :date]}] => :date}},
    [{:cast, :text}] =>
      %{type_specs: %{[:any] => :text}},
    [{:cast, :interval}] =>
      %{type_specs: %{[{:or, [:text, :interval]}] => :interval}},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type t :: Parser.column | Expression.t
  @type data_type :: :any | DataSource.data_type
  @type argument_type :: data_type | {:optional, data_type} | {:many1, data_type} | {:or, [data_type]}


  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(t) :: boolean
  def function?({:function, _, _}), do: true
  def function?(_), do: false

  @doc "Returns true if the function has the specified attribute, false otherise."
  @spec has_attribute?(t | String.t | nil, atom) :: boolean
  def has_attribute?("coalesce", _attribute), do: false # coalesce is only used internally
  def has_attribute?({:function, name, _}, attribute), do: has_attribute?(name, attribute)
  def has_attribute?(%Expression{function?: true, function: name}, attribute), do: has_attribute?(name, attribute)
  def has_attribute?(name, attribute) when is_binary(name), do: attribute in Map.get(@functions[name], :attributes, [])
  def has_attribute?(_, _attribute), do: false

  @doc "Returns true if the given function call is a cast, false otherwise."
  @spec cast?(t) :: boolean
  def cast?({:function, {:cast, _}, _}), do: true
  def cast?(_), do: false

  @doc "Returns the target type of the given cast."
  @spec cast_target(t) :: argument_type
  def cast_target({:function, {:cast, target}, _}), do: target

  @doc "Returns a list of possible argument lists required by the given function call."
  @spec argument_types(t) :: [[argument_type]]
  def argument_types({:function, function, _}), do: @functions[function].type_specs |> Map.keys()

  @doc "Returns the argument specifiaction of the given function call."
  @spec arguments(t) :: [Expression.t]
  def arguments({:function, _, arguments}), do: arguments
  def arguments(_), do: []

  @doc "Returns a stringified version of the given function identifier."
  @spec readable_name(Parser.function_name) :: String.t
  def readable_name({:cast, _}), do: "cast"
  def readable_name({:bucket, _}), do: "bucket"
  def readable_name(name), do: name

  @doc "Returns the return type of the given function call or nil if it is badly typed."
  @spec return_type(t) :: data_type | nil
  def return_type(%Expression{function?: true, function: name, function_args: args}), do:
    return_type({:function, name, args})
  def return_type(function = {:function, name, _}) do
    @functions[name].type_specs
    |> Enum.find(fn({arguments, _}) -> do_well_typed?(function, arguments) end)
    |> case do
      {_arguments, return_type} -> return_type
      nil -> nil
    end
  end

  @doc "Returns the type of the given expression."
  @spec type(t) :: data_type
  def type(function = {:function, _, _}), do: return_type(function)
  def type({column, :as, _}), do: type(column)
  def type({:distinct, column}), do: type(column)
  def type(%Expression{type: type}), do: type
  def type(:*), do: :any

  @doc "Returns true if the arguments to the given function call match the expected argument types, false otherwise."
  @spec well_typed?(t) :: boolean
  def well_typed?(column), do:
    if function?(column),
      do: Enum.any?(argument_types(column), &do_well_typed?(column, &1)),
      else: true

  @doc "Returns true if the argument is a call to a 'bucket' function call, false otherwise."
  @spec bucket?(t) :: boolean
  def bucket?(%Expression{function: {:bucket, _}}), do: true
  def bucket?(_), do: false

  @doc "Updates the bucket size argument of the given 'bucket' function with the given function call."
  @spec update_bucket_size(t, (number -> number)) :: t
  def update_bucket_size(%Expression{function: {:bucket, _}, function_args: [arg1, size]} = expression, fun), do:
    %Expression{expression | function_args: [arg1, %Expression{size | value: fun.(size.value)}]}

  @doc "Returns the value of the bucket size argument of the given 'bucket' function call."
  @spec bucket_size(t) :: number
  def bucket_size(%Expression{function: {:bucket, _}, function_args: [_arg1, size]}), do: size.value

  @doc "Returns true if the function is a valid cloak function"
  @spec exists?(t) :: boolean
  def exists?({:function, function, _}), do: @functions[function] !== nil


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_well_typed?(function, [{:many1, type}]), do:
    Enum.all?(arguments(function), &type_matches?(type, &1))
  defp do_well_typed?(function, argument_types) do
    length(arguments(function)) <= length(argument_types) &&
      argument_types
      |> Enum.with_index()
      |> Enum.all?(fn({type, index}) -> type_matches?(type, Enum.at(arguments(function), index)) end)
  end

  defp type_matches?(type, function = {:function, _, _}), do: type_matches?(type, %{type: return_type(function)})
  defp type_matches?(type, {:distinct, column}), do: type_matches?(type, column)
  defp type_matches?({:optional, _}, nil), do: true
  defp type_matches?(_, nil), do: false
  defp type_matches?({:optional, type}, argument), do: type_matches?(type, argument)
  defp type_matches?({:or, types}, argument), do: Enum.any?(types, &type_matches?(&1, argument))
  defp type_matches?(:any, _), do: true
  defp type_matches?(_, :*), do: false
  defp type_matches?({:constant, expected}, %{constant?: true, type: actual}), do: expected == actual
  defp type_matches?(expected_type, %{type: actual_type}), do: expected_type == actual_type
end
