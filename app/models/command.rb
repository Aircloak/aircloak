require './lib/proto/air/client_commands.pb'
require 'ssh/key/signer'

class Command < ActiveRecord::Base
  def self.new_command_from_most_recent_binaries
    create_command_from_binaries ClientFile.get_most_recent_existing_versions
  end

  def self.most_recent_command
    Command.where(valid_command: true).last
  end

  def tickle
    self.times_downloaded += 1
    save
  end
  
private
  def self.create_command_from_binaries versions
    signer = SSH::Key::Signer.new
    signer.use_agent = false
    signer.add_key_file "#{Rails.root}/config/commands_sig_rsa"

    command = Command.create

    commands_pb = Commands.new(file: [], command_id: command.id, remove_from_host: false)
    versions.each do |v|
      commands_pb.file << binarify_file_version(v)
    end
    encoded_command = commands_pb.encode

    signature = signer.sign(encoded_command).first.signature
    signature_size = signature.size
    encoded_command_size = encoded_command.length

    binary_data = [
      signature_size, 
      encoded_command_size,
      signature, 
      encoded_command
    ].pack("LLA#{signature_size}A#{encoded_command_size}")
    
    command.command_binary = binary_data
    command.valid_command = true
    command.save
  end

  def self.binarify_file_version version
    Commands::File.new(sha1: version.sha1,
                       version_id: version.id,
                       local_name: version.name,
                       download_url: version.download_url,
                       type: version.client_file.client_file_type.name)
  end
end
