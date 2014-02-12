## Generated from aircloak/air/query_upload.proto for aircloak
require "beefcake"


class QueryData
  include Beefcake::Message
end

class QueryData
  required :main_package, :string, 1
  repeated :data, QueryBinary, 2
  optional :payload_identifier, :string, 3
  optional :mutator, :bool, 4
  optional :system_task, :bool, 5
end

