require 'digest/sha1'

class ClientFileVersion < ActiveRecord::Base
  belongs_to :client_file

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
    self.times_downloaded += 1
    save
  end

  def download_url
    "http://graphite.mpi-sws.org:5000/client_binaries/#{id}"
  end

  def name
    "#{client_file.local_name}.#{id}.#{client_file.client_file_type.extension}"
  end
end
