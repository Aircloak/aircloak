## Generated from aircloak/air/task_management.proto for aircloak
require "beefcake"


class CreateQueryPB
  include Beefcake::Message
end

class CreateQueryPB
  required :cluster_id, :uint32, 1
  required :main_package, :string, 2
end
