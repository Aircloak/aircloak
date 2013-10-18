## Generated from aircloak/air/aggregate_results.proto for aircloak
require "beefcake"


class PropertyProto
  include Beefcake::Message


  optional :name, :string, 1
  optional :str_answer, :string, 2
  optional :long_answer, :int64, 3
  optional :count, :int32, 4

end

class PercentileProto
  include Beefcake::Message


  class PointProto
    include Beefcake::Message


    optional :percent, :double, 1
    optional :value, :sint64, 2
    optional :percent_noise, :float, 3

  end

  repeated :points, PercentileProto::PointProto, 1
  optional :name, :string, 2
  optional :min, :sint64, 3
  optional :max, :sint64, 4

end

class ExceptionProto
  include Beefcake::Message


  repeated :stackEntry, :string, 1
  optional :count, :int32, 2

end

class ResultProto
  include Beefcake::Message


  optional :analyst_id, :string, 1
  optional :query_id, :fixed64, 2
  repeated :properties, PropertyProto, 4
  repeated :percentiles, PercentileProto, 5
  repeated :exceptions, ExceptionProto, 6

end
