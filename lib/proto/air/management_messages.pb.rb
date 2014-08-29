## Generated from aircloak/air/management_messages.proto for aircloak
require "beefcake"


class MachinePB
  include Beefcake::Message

  module MachineType
    VM = 1
    PHYSICAL = 2
  end
end

class MachinesPB
  include Beefcake::Message
end

class ClusterPB
  include Beefcake::Message

  module MachineState
    BELONGS_TO = 1
    TO_BE_ADDED = 2
    TO_BE_REMOVED = 3
    TO_BE_UPGRADED = 4
  end

  class MemberPB
    include Beefcake::Message
  end
end

class ClustersPB
  include Beefcake::Message
end

class ClusterStatusPB
  include Beefcake::Message

  module Status
    ACTIVE = 1
    IN_SERVICE = 2
    INACTIVE = 3
  end
end

class MachinePB
  required :id, :uint32, 1
  required :name, :string, 2
  required :type, MachinePB::MachineType, 3
  required :good, :bool, 4
end


class MachinesPB
  repeated :machines, MachinePB, 1
end


class ClusterPB

  class MemberPB
    required :machine_id, :uint32, 1
    required :state, ClusterPB::MachineState, 2
  end

  required :timestamp, :uint64, 1
  required :id, :uint32, 2
  repeated :members, ClusterPB::MemberPB, 3
  required :name, :string, 4
  required :cas_list, :string, 5
end


class ClustersPB
  repeated :clusters, ClusterPB, 1
end


class ClusterStatusPB
  required :status, ClusterStatusPB::Status, 1
  optional :description, :string, 2
end

