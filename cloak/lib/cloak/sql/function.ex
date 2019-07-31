defmodule Cloak.Sql.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.Sql.{Expression, Parser}
  alias Cloak.DataSource

  import Kernel, except: [apply: 2]

  @type t :: Parser.column() | Expression.t()
  @type data_type :: :any | DataSource.data_type()
  @type argument_type :: data_type | {:optional, data_type} | {:many1, data_type} | {:or, [data_type]}

  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(t) :: boolean
  def function?({:function, _, _, _}), do: true
  def function?(_), do: false

  @doc "Returns true if the function has the specified attribute, false otherise."
  @spec has_attribute?(t | Parser.function_name() | nil, atom | tuple) :: boolean
  def has_attribute?({:function, name, _, _}, attribute),
    do: has_attribute?(canonical_name(name), attribute)

  def has_attribute?(%Expression{function?: true, function: name}, attribute),
    do: has_attribute?(name, attribute)

  def has_attribute?(name, attribute) do
    case Map.get(Aircloak.Functions.function_spec(), name) do
      nil -> false
      function -> attribute in Map.get(function, :attributes, [])
    end
  end

  @doc "Returns the target type of the given cast."
  @spec cast_target(Parser.function_spec()) :: argument_type
  def cast_target({:function, {:cast, target}, _, _}), do: target

  @doc "Returns a list of possible argument lists required by the given function call."
  @spec argument_types(t) :: [[argument_type]]
  def argument_types({:function, function, _, _}),
    do: Aircloak.Functions.function_spec()[canonical_name(function)].type_specs |> Map.keys()

  @doc "Returns the argument specification of the given function call."
  @spec arguments(t) :: [Expression.t()]
  def arguments({:function, _, arguments, _}), do: arguments
  def arguments(_), do: []

  @doc "Returns a stringified version of the given function identifier."
  @spec readable_name(Parser.function_name()) :: String.t()
  def readable_name({:cast, _}), do: "cast"
  def readable_name({:bucket, _}), do: "bucket"
  def readable_name(%{canonical_name: _, synonym_used: synonym}), do: synonym
  def readable_name(name), do: name

  @doc "Returns the return type of the given function call or nil if it is badly typed."
  @spec return_type(t) :: data_type | nil
  def return_type(%Expression{function?: true, function: name, function_args: args}),
    do: return_type({:function, name, args, nil})

  def return_type(function = {:function, name, _, _}) do
    Aircloak.Functions.function_spec()[canonical_name(name)].type_specs
    |> Enum.find(fn {arguments, _} -> do_well_typed?(function, arguments) end)
    |> case do
      {_arguments, return_type} -> return_type
      nil -> nil
    end
  end

  @doc "Returns the type of the given expression."
  @spec type(t) :: data_type
  def type(function = {:function, _, _, _}), do: return_type(function)
  def type({column, :as, _}), do: type(column)
  def type({:distinct, column}), do: type(column)
  def type(%Expression{type: type}), do: type
  def type(:*), do: :any

  @doc "Returns true if the arguments to the given function call match the expected argument types, false otherwise."
  @spec well_typed?(t) :: boolean
  def well_typed?(column),
    do:
      if(
        function?(column),
        do: Enum.any?(argument_types(column), &do_well_typed?(column, &1)),
        else: true
      )

  @doc "Returns true if the argument is a call to a 'bucket' function call, false otherwise."
  @spec bucket?(t) :: boolean
  def bucket?(%Expression{function: {:bucket, _}}), do: true
  def bucket?(_), do: false

  @doc "Updates the bucket size argument of the given 'bucket' function with the given function call."
  @spec update_bucket_size(t, (number -> number)) :: t
  def update_bucket_size(
        %Expression{function: {:bucket, _}, function_args: [arg1, size]} = expression,
        fun
      ),
      do: %Expression{
        expression
        | function_args: [arg1, %Expression{size | value: fun.(size.value)}]
      }

  @doc "Returns the value of the bucket size argument of the given 'bucket' function call."
  @spec bucket_size(t) :: number
  def bucket_size(%Expression{function: {:bucket, _}, function_args: [_arg1, size]}),
    do: size.value

  @doc "Returns true if the function is a valid cloak function"
  @spec exists?(t) :: boolean
  def exists?({:function, function, _, _}),
    do: Aircloak.Functions.function_spec()[canonical_name(function)] !== nil

  @doc "Returns true if a function is a math function"
  @spec math_function?(t | Parser.function_name() | nil) :: boolean
  def math_function?(param), do: has_attribute?(param, :math)

  @doc "Returns true if a function is restricted"
  @spec restricted_function?(t | Parser.function_name() | nil) :: boolean
  def restricted_function?(param), do: has_attribute?(param, :restricted)

  @doc "Returns true if a function is a string manipulation function"
  @spec string_manipulation_function?(t | Parser.function_name() | nil) :: boolean
  def string_manipulation_function?(param), do: has_attribute?(param, :string_manipulation)

  @doc "Returns true if a function is an aggregator"
  @spec aggregator?(t | Parser.function_name() | nil) :: boolean
  def aggregator?(param), do: has_attribute?(param, :aggregator)

  @doc "Returns true if the given function call is a cast, false otherwise."
  @spec cast?(t | Parser.function_name() | nil) :: boolean
  def cast?(param), do: has_attribute?(param, :cast)

  @doc "Returns true if the given function exhibits implicit range behaviour, false otherwise."
  @spec implicit_range?(t | Parser.function_name() | nil) :: boolean
  def implicit_range?(param), do: has_attribute?(param, :implicit_range)

  @doc "Returns true if the given function is unsafe, false otherwise."
  @spec unsafe?(t | Parser.function_name() | nil) :: boolean
  def unsafe?(param), do: has_attribute?(param, :unsafe)

  @doc "Provides information about alternatives for deprecated functions."
  @spec deprecation_info(t) :: {:error, :function_exists | :not_found} | {:ok, %{alternative: String.t()}}
  def deprecation_info({:function, name, _, _} = function) do
    case {unsafe?(function), exists?(function), Aircloak.Functions.deprecated_functions()[canonical_name(name)]} do
      {true, _, _} -> {:error, :unsafe_function}
      {_, true, _} -> {:error, :function_exists}
      {_, false, nil} -> {:error, :not_found}
      {_, false, value} -> {:ok, value}
    end
  end

  @doc "Resolves synonyms to canonical names."
  @spec canonical_name(Parser.function_name()) :: Parser.function_name()
  def canonical_name(%{canonical_name: name, synonym_used: _}), do: name
  def canonical_name(other), do: other

  @doc "Returns all the type specs for the given function. Used by QueryGenerator."
  @spec type_specs(Parser.function_name()) :: map
  def type_specs(function),
    do: Aircloak.Functions.function_spec()[canonical_name(function)].type_specs

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_well_typed?(function, [{:many1, type}]) do
    case arguments(function) do
      [] -> false
      args -> Enum.all?(args, &type_matches?(type, &1))
    end
  end

  defp do_well_typed?(function, argument_types) do
    length(arguments(function)) <= length(argument_types) &&
      argument_types
      |> Enum.with_index()
      |> Enum.all?(fn {type, index} ->
        type_matches?(type, Enum.at(arguments(function), index))
      end)
  end

  defp type_matches?(type, function = {:function, _, _, _}),
    do: type_matches?(type, %{type: return_type(function)})

  defp type_matches?(type, {:distinct, column}), do: type_matches?(type, column)
  defp type_matches?({:optional, _}, nil), do: true
  defp type_matches?(_, nil), do: false
  defp type_matches?({:optional, type}, argument), do: type_matches?(type, argument)
  defp type_matches?({:or, types}, argument), do: Enum.any?(types, &type_matches?(&1, argument))
  defp type_matches?(:any, _), do: true
  defp type_matches?(_, :*), do: false

  defp type_matches?({:constant, expected}, %{constant?: true, type: actual}),
    do: expected == actual

  defp type_matches?({:constant, expected}, %{function?: true, function_args: args, type: actual}),
    do: expected == actual and Enum.all?(args, &Expression.constant?/1)

  defp type_matches?(_expected_type, %Expression{value: nil, type: nil}), do: true

  defp type_matches?(expected_type, %{type: actual_type}), do: expected_type == actual_type
end
