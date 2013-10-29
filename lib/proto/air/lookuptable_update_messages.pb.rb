## Generated from aircloak/air/lookuptable_update_messages.proto for aircloak
require "beefcake"


class AdditionTask
  include Beefcake::Message

  class Data
    include Beefcake::Message
  end
end

class DeleteTask
  include Beefcake::Message
end

class LookupTableTasks
  include Beefcake::Message
end

class AdditionTask

  class Data
    optional :key, :string, 1
    optional :value, :string, 2
  end

  optional :analyst_id, :string, 1
  optional :table, :string, 2
  optional :data, AdditionTask::Data, 3
end


class DeleteTask
  optional :analyst_id, :string, 1
  optional :table, :string, 2
end


class LookupTableTasks
  repeated :additions, AdditionTask, 1
  repeated :deletions, DeleteTask, 2
end

