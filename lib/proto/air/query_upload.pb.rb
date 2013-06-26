## Generated from aircloak/air/query_upload.proto for aircloak.air
require "beefcake"


class QueryData
  include Beefcake::Message


  required :main_package, :string, 1
  repeated :data, QueryBinary, 2

end
