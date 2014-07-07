module PrefetchHelper
  def age_table_double
    double({
      id: 1,
      table_name: "age",
      table_data: "[
        {\"name\":\"age\",\"constraints\":[],\"type\":\"integer\"},
        {\"name\":\"city\",\"constraints\":[],\"type\":\"varchar(100)\"}
      ]"
    })
  end

  def prefetch_conversions
    conversions = {
      table_data([]) =>
        "[{\"table\":\"age\",\"time_limit\":null,\"user_rows\":null}]",

      table_data([[["age", "=", "1"], ["city", "=", "New York"]]]) =>
        "[{\"table\":\"age\",\"time_limit\":null,\"user_rows\":null,\"where\":{\"$or\":[{\"$$age\":[{\"$eq\":1}],\"$$city\":[{\"$eq\":\"New York\"}]}]}}]",

      table_data([[["age", "=", "1"]], [["city", "=", "New York"]]]) =>
        "[{\"table\":\"age\",\"time_limit\":null,\"user_rows\":null,\"where\":{\"$or\":[{\"$$age\":[{\"$eq\":1}]},{\"$$city\":[{\"$eq\":\"New York\"}]}]}}]"
    }

    PrefetchFilter::OperatorSqlToCloak.each do |sql_operator, cloak_operator|
      conversions.merge!(
        table_data([[["age", sql_operator, "1"]]]) =>
          "[{\"table\":\"age\",\"time_limit\":null,\"user_rows\":null,\"where\":{\"$or\":[{\"$$age\":[{\"#{cloak_operator}\":1}]}]}}]"
      )
    end

    conversions.merge(
      table_data([], time_limit: 5) => "[{\"table\":\"age\",\"time_limit\":5,\"user_rows\":null}]",
      table_data([], user_rows: 10) => "[{\"table\":\"age\",\"time_limit\":null,\"user_rows\":10}]"
    )
  end

  def table_data(groups, additional_attrs = {})
    [{"tableId" => 1, time_limit: nil, user_rows: nil, filter: {groups: groups.map do |filters|
      {filters: filters.map do |filter|
        {column: filter[0], operator: filter[1], value: filter[2]}
      end}
    end}}.merge(additional_attrs)].to_json
  end

  def analyst_double(tables)
    double(analyst_tables: double(where: tables))
  end
end