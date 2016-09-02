defmodule Cloak.DataSource.SqlBuilder.DbFunction do
  @moduledoc "SQL code generation for database function invocations"


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Generates SQL for a function invocation.

  Provided arguments list must contain SQL fragments.
  """
  @spec sql(String.t | {:cast, Cloak.DataSource.data_type}, [iodata], Cloak.DataSource.data_type) :: iodata
  def sql({:cast, type}, [arg], _parsed_type), do: sql("cast", [arg, sql_type(type)], type)
  def sql(fun_name, fun_args, parsed_type) do
    cast_spec = cast_spec(fun_name, fun_args)

    fun_name
    |> function_call(casted_args(fun_args, cast_spec[:args]))
    |> cast(return_type(cast_spec[:return], parsed_type))
  end


  #-----------------------------------------------------------------------------------------------------------
  # SQL generation
  #-----------------------------------------------------------------------------------------------------------

  for binary_operator <- ~w(+ - * ^ /) do
    defp function_call(unquote(binary_operator), [arg1, arg2]),
      do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  for datepart <- ~w(year month day hour minute second) do
    defp function_call(unquote(datepart), args),
      do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  for {fun, delegate_to} <- %{
    "ceiling" => "ceil",
    "lcase" => "lower",
    "ucase" => "upper",
    "||" => "concat"
  } do
    defp function_call(unquote(fun), args), do: function_call(unquote(delegate_to), args)
  end
  defp function_call("substring_for", [arg1, arg2]),
    do: function_call("substring", [arg1, "1", arg2])
  defp function_call("cast", [arg1, arg2]),
    do: ["CAST(", arg1, " AS ", arg2, ")"]
  defp function_call(name, args),
    do: [name, "(", Enum.intersperse(args, ",") ,")"]

  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "bool"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)


  #-----------------------------------------------------------------------------------------------------------
  # Transformation helpers
  #-----------------------------------------------------------------------------------------------------------

  defp cast_spec(fun_name, fun_args) do
    case Enum.find(function_casts(), &casts?(&1, fun_name, fun_args)) do
      nil -> []
      {^fun_name, opts} -> opts
    end
  end

  defp casts?({fun_name, opts}, fun_name, fun_args),
    do: (opts[:args] == nil || length(opts[:args]) == length(fun_args))
  defp casts?(_, _, _), do: false

  defp casted_args(args, nil), do: args
  defp casted_args(args, casts) do
    for {arg, cast} <- Enum.zip(args, casts), do: cast(arg, cast)
  end

  defp cast(expr, :pass), do: expr
  defp cast(expr, type), do: function_call("cast", [expr, sql_type(type)])

  defp return_type(nil, _), do: :pass
  defp return_type(:pass, _), do: :pass
  defp return_type(:parsed_type, parsed_type), do: parsed_type
  defp return_type(type, _), do: type

  # Specifies transformations of function arguments and return values
  defp function_casts() do
    [
      # aggregate functions
      {"avg", return: :parsed_type},
      {"sum", return: :parsed_type},
      {"count", return: :integer},
      # math functions
      {"/", args: [:real, :pass]},
      {"trunc", args: [:pass], return: :integer},
      {"trunc", args: [:numeric, :pass], return: :real},
      {"round", args: [:pass], return: :integer},
      {"round", args: [:numeric, :pass], return: :real},
      {"div", return: :integer},
      {"mod", return: :integer},
      {"floor", return: :integer},
      {"ceil", return: :integer},
      {"ceiling", return: :integer},
      #datetime functions
      {"year", return: :integer},
      {"month", return: :integer},
      {"day", return: :integer},
      {"hour", return: :integer},
      {"minute", return: :integer},
      {"second", return: :integer}
    ]
  end
end
