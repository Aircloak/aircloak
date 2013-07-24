require './lib/proto/air/client_commands.pb'
require 'ssh/key/signer'

class Command < ActiveRecord::Base
  belongs_to :deployment_group
  has_many :command_file_versions, dependent: :destroy
  has_many :client_file_versions, through: :command_file_versions

  def self.new_command_from_most_recent_binaries group
    recent_file_versions = ClientFile.get_most_recent_existing_versions only_verified: group.verified_only
    new_command = create_command_from_binaries recent_file_versions
    new_command.deployment_group = group
    new_command.save
  end

  def is_most_recent?
    Command.select("id").where(deployment_group_id: self.deployment_group_id)
                        .order(created_at: :desc).first.id == self.id
  end

  def tickle
    Command.increment_counter :times_downloaded, self.id
  end
  
private
  def self.create_command_from_binaries versions
    signer = SSH::Key::Signer.new
    signer.use_agent = false
    signer.add_key_file "#{Rails.root}/config/commands_sig_rsa"

    command = Command.create
    command.client_file_versions = versions

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
    command
  end

  def self.binarify_file_version version
    Commands::File.new(sha1: version.sha1,
                       version_id: version.id,
                       local_name: version.name,
                       download_url: version.download_url,
                       type: version.client_file.client_file_type.name)
  end
end
