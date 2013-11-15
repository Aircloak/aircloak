## Generated from aircloak/air/management_messages.proto for 
require "beefcake"


class MachineProto
  include Beefcake::Message

  module MachineType
    VM = 1
    PHYSICAL = 2
  end
end

class MachineStateProto
  include Beefcake::Message

  module State
    GOOD = 1
    CHANGING = 2
    SW_FAILING = 3
    HW_FAILING = 4
  end
end

class MachinesProto
  include Beefcake::Message
end

class ClusterProto
  include Beefcake::Message
end

class MachineProto
  required :machine_id, :uint32, 1
  required :name, :string, 2
  required :type, MachineProto::MachineType, 3
  required :state, MachineStateProto, 4
end


class MachineStateProto
  required :state, MachineStateProto::State, 1
end


class MachinesProto
  repeated :machines, MachineProto, 1
end


class ClusterProto
  required :cluster_id, :uint32, 1
  repeated :machine_ids, :uint32, 2
end

