## Generated from aircloak/air/client_commands.proto for aircloak.air
require "beefcake"


class Commands
  include Beefcake::Message


  class Application
    include Beefcake::Message


    required :sha1, :string, 1
    required :download_url, :string, 2
    required :local_name, :string, 3
    required :version_id, :uint32, 4

  end

  required :command_id, :uint64, 1
  required :remove_from_host, :bool, 2
  required :updater, Commands::Application, 3
  required :client, Commands::Application, 4

end
