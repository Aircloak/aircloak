require 'digest/sha1'

class ClientFileVersion < ActiveRecord::Base
  belongs_to :client_file
  has_many :client_file_events
  has_many :command_file_versions
  has_many :commands, through: :command_file_versions

  after_save :new_valid_commands?

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
    self.times_downloaded = 0 unless self.times_downloaded
    self.times_downloaded += 1
    save
  end

  def download_url
    "http://graphite.mpi-sws.org:5000/client_binaries/#{id}"
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

    events = ClientFileEvent.select("count(*) as num_events")
                            .where(positive: true, client_file_version_id: id)
                            .group("client_file_events.staging_machine_id")

    staging_machine_count = StagingMachine.count
    
    # Should be as many sets of events as staging machines
    return false unless StagingMachine.count == events.to_a.size

    # They should all have the same amount of positive events associated
    # with the binary
    num_events = events.first.num_events

    return false if num_events < 2

    events.each do |event|
      return false unless num_events == event.num_events
    end

    true
  end

  def new_valid_commands?
    DeploymentGroup.new_valid_commands?
  end
end
