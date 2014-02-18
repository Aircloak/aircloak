## Generated from aircloak/air/client_commands.proto for aircloak
require "beefcake"


class Commands
  include Beefcake::Message

  class File
    include Beefcake::Message
  end
end

class Commands

  class File
    required :sha1, :string, 1
    required :download_url, :string, 2
    required :local_name, :string, 3
    required :version_id, :uint32, 4
    required :type, :string, 5
  end
  required :command_id, :uint64, 1
  required :remove_from_host, :bool, 2
  repeated :file, Commands::File, 3
end
