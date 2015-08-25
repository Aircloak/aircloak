class InvalidPrefetchFilter < Exception; end

class PrefetchFilter
  OperatorSqlToCloak = {
    "=" => "$eq",
    "<>" => "$neq",
    ">" => "$gt",
    ">=" => "$gte",
    "<" => "$lt",
    "<=" => "$lte"
  }

  OperatorCloakToSql = OperatorSqlToCloak.invert

  # See Task.data for explanation
  def self.prefetch_to_data(task, prefetch, cluster_id)
    return "" if prefetch.nil? || prefetch.empty?

    data = JSON.parse(prefetch).map do |prefetch_table|
      table = task.analyst.user_tables.where(
            cluster_id: cluster_id, table_name: prefetch_table["table"], deleted: false
          ).first
      {
        tableId: table.id,
        time_limit: prefetch_table["time_limit"],
        user_rows: prefetch_table["user_rows"],
        columns: prefetch_table["columns"] || [],
        filter: parse_where(prefetch_table["where"] || {})
      }.delete_if {|key, value| value.blank?} # compact filter by removing empty fields
    end

    data.to_json
  rescue Exception => e
    # We log, but ignore exceptions, since this is a retrieval operation.
    # The consequence is that UI will simply receive an empty filter if anything
    # goes wrong.
    Rails.logger.error(["Error: #{e.message}"].concat(e.backtrace).join("\n"))
    ""
  end

  def self.parse_where(where)
    groups = where.map do |where_group|
      filter_group = []

      where_group.each do |field, filter|
        column = field.gsub(/^\$\$/, "")
        filter.each do |comparisons|
          comparisons.each do |operator, value|
            filter_group << {column: column, operator: OperatorCloakToSql[operator], value: value.to_s}
          end
        end
      end

      {filters: filter_group}
    end.select {|group| group[:filters].length > 0}
    {groups: groups}
  end

  # See Task.data for explanation
  def self.data_to_prefetch(task, data)
    filter_error("can't be blank") if data.nil? || data.empty?

    data = JSON.parse(data)
    filter_error("can't be blank") if data.empty?

    prefetch =
      data.map do |prefetch_table|
        table = task.analyst.user_tables.where(id: prefetch_table["tableId"], deleted: false)
        if table.length != 1
          # This usually shouldn't happen, since we're getting the data
          # we forwarded to client.
          filter_error("invalid table")
        end
        table = table.first
        {
          table: table.table_name,
          time_limit: prefetch_table["time_limit"],
          user_rows: prefetch_table["user_rows"],
          columns: prefetch_table["columns"],
          where: convert_filter(table, prefetch_table["filter"])
        }.delete_if {|key, value| value.blank?} # compact filter by removing empty fields
      end

    prefetch.to_json
  end

  def self.convert_filter(table, filter)
    table_columns = JSON.parse(table.table_data).inject({}) do |memo, column|
      memo.merge(column["name"] => column["type"])
    end

    filter["groups"].select {|filter_group| filter_group && !filter_group.empty?}.map do |filter_group|
      filter_group["filters"].inject({}) do |memo, filter|
        column_name = filter["column"]
        converted_value = value_for_cloak(filter["value"], column_name, table_columns[column_name])
        column_filters = (memo["$$#{column_name}"] ||= [])
        column_filters << {operator_for_cloak(filter["operator"]) => converted_value}
        memo
      end
    end
  end

  def self.operator_for_cloak(operator)
    result = OperatorSqlToCloak[operator]
    filter_error("invalid operator #{operator}") if result.nil?
    result
  end

  def self.value_for_cloak(value, column_name, type)
    filter_error("invalid column #{column_name}") if type.nil?

    type = type.gsub(/\(.*\)/, "")  # remove (length) from type

    transformed = case type
      when "integer" then Integer(value)
      when "float" then Float(value)
      when "boolean" then value.to_s.downcase == "true"
      when "varchar" then value
    end

    transformed
  rescue ArgumentError
    filter_error("invalid value #{value} for column #{column_name}")
  end

  def self.filter_error(message)
    raise InvalidPrefetchFilter.new(message)
  end
end
