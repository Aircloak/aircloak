## Generated from aircloak/cloak/task.proto for aircloak
require "beefcake"


class TaskCodePB
  include Beefcake::Message
end

class TaskMetaInfoPB
  include Beefcake::Message

  module TaskType
    UPDATE = 0
    QUERY = 1
  end
end

class TaskUploadPB
  include Beefcake::Message
end

class TaskCodePB
  required :sandbox_type, :string, 1
  required :code, :bytes, 2
end


class TaskMetaInfoPB
  required :name, :string, 1
  required :task_type, TaskMetaInfoPB::TaskType, 2
  optional :payload_identifier, :string, 3
  required :task_id, :uint64, 4
  required :index, :string, 5
  required :analyst_id, :uint64, 6
end


class TaskUploadPB
  required :task_code, TaskCodePB, 1
  required :meta_info, TaskMetaInfoPB, 2
end

