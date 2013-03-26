require './lib/proto/air/client_commands.pb'
require 'ssh/key/signer'

class Command < ActiveRecord::Base
  def self.new_command_from_most_recent_binaries
    client = ClientBinary.where(updater: false).last
    updater = ClientBinary.where(updater: true).last
    create_command_from_binaries client, updater if client && updater
  end

  def self.most_recent_command
    Command.where(valid_command: true).first
  end

  def tickle
    self.times_downloaded += 1
    save
  end
  
private
  def self.create_command_from_binaries client, updater
    signer = SSH::Key::Signer.new
    signer.use_agent = false
    signer.add_key_file "#{Rails.root}/config/commands_sig_rsa"

    command = Command.create

    commands_pb = Commands.new(command_id: command.id, remove_from_host: false)
    commands_pb.updater = binarify_client_binary updater
    commands_pb.client = binarify_client_binary client
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

  def self.binarify_client_binary binary
    Commands::Application.new(sha1: binary.sha1,
                              version_id: binary.id,
                              local_name: binary.name,
                              download_url: binary.download_url)
  end
end
