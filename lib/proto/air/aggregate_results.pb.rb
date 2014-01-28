## Generated from aircloak/air/aggregate_results.proto for aircloak
require "beefcake"


class JoinersLeaversProto
  include Beefcake::Message
end

class PropertyProto
  include Beefcake::Message

  class RangeProto
    include Beefcake::Message
  end
end

class ExceptionProto
  include Beefcake::Message
end

class ResultProto
  include Beefcake::Message
end

class ResultsProto
  include Beefcake::Message
end

class JoinersLeaversProto
  required :joiners, :uint32, 1
  required :leavers, :uint32, 2
end


class PropertyProto

  class RangeProto
    required :min, :sint64, 1
    required :max, :sint64, 2
  end

  required :label, :string, 1
  optional :string, :string, 2
  optional :range, PropertyProto::RangeProto, 3
  optional :joiners_leavers, JoinersLeaversProto, 4
  optional :accumulated_count, :sint64, 5
end


class ExceptionProto
  required :stackEntry, :string, 1
  optional :joiners_leavers, JoinersLeaversProto, 2
end


class ResultProto
  required :analyst_id, :string, 1
  required :task_id, :fixed64, 2
  required :index, :string, 3
  required :result_id, :uint64, 4
  repeated :properties, PropertyProto, 5
  repeated :exceptions, ExceptionProto, 6
end


class ResultsProto
  repeated :result_ids, :uint64, 1
end

