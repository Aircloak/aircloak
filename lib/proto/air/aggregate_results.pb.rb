## Generated from aircloak/air/aggregate_results.proto for aircloak
require "beefcake"


class BucketPB
  include Beefcake::Message

  class RangeProto
    include Beefcake::Message
  end
end

class ExceptionPB
  include Beefcake::Message
end

class ResultPB
  include Beefcake::Message
end

class ResultsPB
  include Beefcake::Message
end

class BucketPB

  class RangeProto
    required :min, :sint64, 1
    required :max, :sint64, 2
  end

  required :label, :string, 1
  optional :string, :string, 2
  optional :range, BucketPB::RangeProto, 3
  optional :accumulated_count, :sint64, 4
end


class ExceptionPB
  required :stackEntry, :string, 1
  optional :accumulated_count, :sint64, 2
end


class ResultPB
  required :analyst_id, :uint64, 1
  required :task_id, :string, 2
  required :result_id, :uint64, 3
  repeated :buckets, BucketPB, 4
  repeated :exceptions, ExceptionPB, 5
end


class ResultsPB
  repeated :result_ids, :uint64, 1
end

