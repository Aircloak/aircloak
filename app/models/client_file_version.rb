require 'digest/sha1'

class ClientFileVersion < ActiveRecord::Base
  after_save :new_valid_commands?
  # This before_destroy callback needs to be called
  # before the object is destroyed. The object
  # might get destroyed through a :dependent => :destroy
  # from the ClientFile. It should not be allowed to
  # finish the destroy if there is an active command
  # relying on this version.
  # Since the :dependent => :destroy mechanism itself
  # relies on callbacks we need to ensure our callback
  # is defined before the other callbacks, in
  # order for it to be executed first.
  before_destroy :can_destroy?

  belongs_to :client_file
  has_many :client_file_events, dependent: :destroy
  has_many :command_file_versions
  has_many :commands, through: :command_file_versions, dependent: :destroy

  def self.new_binary_from_file(tempfile) 
    data = tempfile.read
    sha1 = Digest::SHA1.hexdigest data
    c = nil
    if ClientFileVersion.where(sha1: sha1).blank? then
      c = ClientFileVersion.new
      c.data = data
      c.sha1 = sha1
      c.size = data.size
    end
    c
  end

  def tickle
    ClientFileVersion.increment_counter :times_downloaded, self.id
  end

  def download_url
    "http://#{Rails.configuration.client_file_version.download_host}/client_binaries/#{id}"
  end

  def name
    "#{client_file.local_name}.#{id}.#{client_file.client_file_type.extension}"
  end

  def check_verification
    verification = is_verified?
    self.verified = verification
    save
    verification
  end

  def verification_description
    return "N/A" unless requires_verification?
    verified.to_s
  end

  def can_destroy?
    if has_active_command?
      self.errors[:base] << "File version #{self.id} cannot be deleted while part of an active command"
      return false;
    end
  end

  # If this file version is part of an active command
  # then it would be bad style to delete it
  def has_active_command?
    commands.each do |command|
      return true if command.is_most_recent?
    end
    false
  end

  # Returns true if verified is false, and there have been no
  # negative events associated with the file and the file is older
  # than a certain threshold
  # In all other cases it returns false
  def not_verified_and_not_failed
    self.verified == false and 
      1.hour.ago > self.created_at and
      client_file_events.where(positive: false).count == 0
  end

  def manually_verified
    verified and requires_verification? and not is_verified?
  end

private
  def requires_verification?
    client_file.requires_verifications
  end

  # A file is verified if:
  # - if it doesn't require verification (i.e. automatically verified)
  # - there are only positive events connected to it
  # - there is at least one positive event
  # - all staging machines have the same amount of positive events
  # - has more than two events (all files with less will be rejected)
  def is_verified?
    # Doesn't require verification? Then yes, it is verified
    return true unless requires_verification?

    # If there is a negative event, then we fail the file
    return false if client_file_events.where(positive: false).count > 0

    events = ClientFileEvent.positive_events_for_version id

    # Should be as many sets of events as staging machines
    return false unless StagingMachine.count == events.to_a.size

    all_event_counts_ok? events
  end

  def new_valid_commands?
    DeploymentGroup.new_valid_commands?
  end

private
  def all_event_counts_ok? events
    # They should all have the same amount of positive events associated
    # with the binary
    num_events = events.first.num_events

    return false if num_events < 2

    events.each do |event|
      return false unless num_events == event.num_events
    end

    true
  end
end
