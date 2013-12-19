## Generated from aircloak/air/log_config_messages.proto for 
require "beefcake"


class LogConfigPB
  include Beefcake::Message

  class ClusterPB
    include Beefcake::Message
  end
end

class LogConfigPB

  class ClusterPB
    repeated :cloak_names, :string, 1
    required :cluster_name, :string, 2
  end

  repeated :clusters, LogConfigPB::ClusterPB, 1
  repeated :unassigned_cloaks, :string, 2
end

