class ClientFileEvent < ActiveRecord::Base
  belongs_to :client_file_version
  belongs_to :staging_machine

  after_save :check_version_verification

private
  def check_version_verification
    client_file_version.check_verification
  end
end
