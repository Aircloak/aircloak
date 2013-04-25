class CommandFileVersion < ActiveRecord::Base
  belongs_to :command
  belongs_to :client_file_version
end
