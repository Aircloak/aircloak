require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Returns a property for the query and property.
  # If there already exists a property, then it is reused,
  # otherwise a new one will be created
  def self.get_property! query_id, property_proto
    # Return existing properties if they exist
    p = Property.where(query_id: query_id, property: property_proto.name).first
    p = Property.create(query_id: query_id, property: property_proto.name) unless p
    return p
  end

  # Whether or not a prototype aggregate property is
  # numeric or a string property
  def self.is_numeric? property_proto
    property_proto.str_answer == nil && property_proto.long_answer != nil
  end

  # Returns an existing property result if one
  # exists, and otherwise creates and returns a new one
  def self.get_property_result! property, property_proto
    pr = if is_numeric? property_proto
      property.property_results.where(numeric: true, long_value: property_proto.long_answer).first
    else
      property.property_results.where(numeric: false, str_value: property_proto.str_answer).first
    end

    unless pr
      numeric = is_numeric? property_proto
      vals = {}
      vals[:numeric] = numeric
      vals[:str_value] = property_proto.str_answer unless numeric
      vals[:long_value] = property_proto.long_answer if numeric
      pr = property.property_results.create(vals)
    end

    pr
  end

  # Persists a property result to the database,
  # updating existing property results by extending them
  # with new counts, if possible
  def self.add_property_result query_id, property_proto
    prop = get_property! query_id, property_proto
    result = get_property_result! prop, property_proto
    result.property_result_counts.create(count: property_proto.count) != nil
  end
end
