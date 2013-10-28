require './lib/proto/air/aggregate_results.pb'

class ResultHandler
  # Returns a property for the query and property.
  # If there already exists a property, then it is reused,
  # otherwise a new one will be created
  def self.get_property! task_id, index, property_proto
    # Return existing properties if they exist
    p = Property.where(query_id: task_id, index: index, property: property_proto.label).first
    p = Property.create(query_id: task_id, index: index, property: property_proto.label) unless p
    return p
  end

  # Returns an existing property result if one
  # exists, and otherwise creates and returns a new one
  def self.get_property_result! property, property_proto
    result = if property_proto.range.blank?
      r = property.property_results.where(str_value: property_proto.string, has_range: false).first
      r = property.property_results.create(str_value: property_proto.string, has_range: false) unless r
      r
    else
      r = property.property_results.where(str_value: property_proto.string, has_range: true,
          range_min: property_proto.range.min, range_max: property_proto.range.max).first
      r = property.property_results.create(str_value: property_proto.string, has_range: true,
          range_min: property_proto.range.min, range_max: property_proto.range.max) unless r
      r
    end
    return result
  end

  # Persists a property result to the database,
  # updating existing property results by extending them
  # with new counts, if possible
  def self.add_property_result task_id, index, property_proto
    prop = get_property! task_id, index, property_proto
    result = get_property_result! prop, property_proto
    if property_proto.joiners_leavers.blank?
      true
    else
      result.property_result_counts.create(joiners: property_proto.joiners_leavers.joiners, leavers:
          property_proto.joiners_leavers.leavers) != nil
    end
  end
end
