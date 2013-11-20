## Generated from aircloak/air/management_messages.proto for 
require "beefcake"


class MachineProto
  include Beefcake::Message

  module MachineType
    VM = 1
    PHYSICAL = 2
  end
end

class MachinesProto
  include Beefcake::Message
end

class ClusterProto
  include Beefcake::Message

  module MachineState
    BELONGS_TO = 1
    TO_BE_ADDED = 2
    TO_BE_REMOVED = 3
  end

  class MachineProto
    include Beefcake::Message
  end
end

class MachineProto
  required :machine_id, :uint32, 1
  required :name, :string, 2
  required :type, MachineProto::MachineType, 3
  required :good, :bool, 4
end


class MachinesProto
  repeated :machines, MachineProto, 1
end


class ClusterProto

  class MachineProto
    required :machine_id, :uint32, 1
    required :state, ClusterProto::MachineState, 2
  end

  required :timestamp, :uint64, 1
  required :cluster_id, :uint32, 2
  repeated :machines, ClusterProto::MachineProto, 3
end

