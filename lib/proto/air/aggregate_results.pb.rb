## Generated from aircloak/air/aggregate_results.proto for aircloak.air
require "beefcake"


class Property
  include Beefcake::Message


  optional :name, :string, 1
  optional :str_answer, :string, 2
  optional :long_answer, :int64, 3
  optional :count, :int32, 4

end

class Percentile
  include Beefcake::Message


  class Point
    include Beefcake::Message


    optional :percent, :double, 1
    optional :value, :sint64, 2
    optional :percent_noise, :float, 3

  end

  repeated :points, Percentile::Point, 1
  optional :name, :string, 2
  optional :min, :sint64, 3
  optional :max, :sint64, 4

end

class Exception
  include Beefcake::Message


  repeated :stackEntry, :string, 1
  optional :count, :int32, 2

end

class Result
  include Beefcake::Message


  optional :analyst_id, :string, 1
  optional :query_id, :fixed64, 2
  repeated :properties, Property, 4
  repeated :percentiles, Percentile, 5
  repeated :exceptions, Exception, 6

end
