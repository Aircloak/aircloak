class DeploymentGroup < ActiveRecord::Base
  has_many :commands, dependent: :destroy

  def self.new_valid_commands?
    DeploymentGroup.all.each do |group|
      group.new_command_if_possible!
    end
  end

  def new_command_if_possible!
    return unless autoupdate
    create_new_command if exist_newer_configuration?
  end

  def create_new_command
    Command.new_command_from_most_recent_binaries self
  end

  def has_active_command?
    active_command != nil
  end

  # There exists a newer configuration if:
  # - Either we do not yet have a commad, and there are
  #   currently versions of all files that satisfy our
  #   verified_only criteria
  # - We have an existing command, and it differs from
  #   the most recent command files available for this group
  def exist_newer_configuration?
    most_recent_file_versions = ClientFile.get_most_recent_existing_versions only_verified: verified_only

    return false if most_recent_file_versions.size == 0
    return true if (! has_active_command?) and most_recent_file_versions.size == ClientFile.count

    if has_active_command? then
      existing_files = active_command.client_file_versions.to_a
      most_recent_file_versions.each do |file|
        return true unless existing_files.include?(file)
      end
    end

    false
  end

  # Returns files that have changed since the command currently in use
  def available_updated_files
    most_recent_file_versions = ClientFile.get_most_recent_existing_versions only_verified:verified_only

    return most_recent_file_versions unless has_active_command?
    
    active_command.client_file_versions.each do |file|
      if most_recent_file_versions.include?(file) then
        most_recent_file_versions -= [file]
      end
    end
    most_recent_file_versions
  end

  def active_command
    commands.where(deployment_group_id: self.id).last
  end
end
