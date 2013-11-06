## Generated from aircloak/air/build_messages.proto for aircloak
require "beefcake"


class BuildRequestProto
  include Beefcake::Message


  class VersionProto
    include Beefcake::Message


    required :repo, :string, 1
    required :commit_id, :string, 2
    required :environment, :string, 3

  end

  required :build_name, :string, 1
  required :build_id, :uint32, 2
  repeated :versions, BuildRequestProto::VersionProto, 3

end

class BuildResponseProto
  include Beefcake::Message

  module Status
    OK = 1
    ERROR = 2
  end

  required :build_id, :uint32, 1
  required :status, BuildResponseProto::Status, 2
  optional :error_message, :string, 3

end

class VersionBuildResponseProto
  include Beefcake::Message

  module Status
    OK = 1
    ERROR = 2
  end

  required :commit_id, :string, 1
  required :status, VersionBuildResponseProto::Status, 2
  required :environment, :string, 3
  required :log_output, :string, 4

end
