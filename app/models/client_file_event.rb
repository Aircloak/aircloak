class ClientFileEvent < ActiveRecord::Base
  belongs_to :client_file_version
  belongs_to :staging_machine
  belongs_to :client_file

  after_save :check_version_verification

  def self.positive_events_for_version id
    ClientFileEvent.select("count(*) as num_events")
        .where(positive: true, client_file_version_id: id)
        .group("client_file_events.staging_machine_id")
  end

private
  def check_version_verification
    client_file_version.check_verification
  end
end
