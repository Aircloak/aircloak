## Generated from aircloak/air/version_test_messages.proto for 
require "beefcake"


class TestRequestPB
  include Beefcake::Message
end

class TestResponsePB
  include Beefcake::Message
end

class TestRequestPB
  required :id, :uint32, 1
  required :cluster_id, :uint32, 2
  repeated :cluster_nodes, :string, 3
end


class TestResponsePB
  required :id, :uint32, 1
  required :success, :bool, 2
  required :transcript, :string, 3
end

