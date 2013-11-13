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

class MachineChangeProto
  include Beefcake::Message
end

class ClusterProto
  include Beefcake::Message
end

class MachineProto
  required :machine_id, :uint32, 1
  required :name, :string, 2
  required :type, MachineProto::MachineType, 3
end


class MachinesProto
  repeated :machines, MachineProto, 1
end


class MachineChangeProto
  repeated :added_machines, MachineProto, 1
  repeated :removed_machines, MachineProto, 2
end


class ClusterProto
  required :cluster_id, :uint32, 1
  required :machines, MachineChangeProto, 2
end

