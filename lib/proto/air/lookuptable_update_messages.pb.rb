## Generated from aircloak/air/lookuptable_update_messages.proto for aircloak.cloak
require "beefcake"


class AdditionTask
  include Beefcake::Message


  class Data
    include Beefcake::Message


    optional :key, :string, 1
    optional :value, :string, 2

  end

  optional :analyst_id, :string, 1
  optional :table, :string, 2
  optional :data, AdditionTask::Data, 3

end

class DeleteTask
  include Beefcake::Message


  optional :analyst_id, :string, 1
  optional :table, :string, 2

end

class LookupTableTasks
  include Beefcake::Message


  repeated :additions, AdditionTask, 1
  repeated :deletions, DeleteTask, 2

end
