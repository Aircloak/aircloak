require 'digest/sha1'

class ClientBinary < ActiveRecord::Base
  def self.new_binary_from_file(tempfile) 
    data = tempfile.read
    sha1 = Digest::SHA1.hexdigest data
    c = nil
    if ClientBinary.where(sha1: sha1).blank? then
      c = ClientBinary.new
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

  def type
    updater ? "updater" : "client"
  end

  def download_url
    "http://graphite.mpi-sws.org:5000/client_binaries/#{id}"
  end

  def name
    updater ? "AircloakClientUpdater.#{id}.exe" : "AircloakClient.#{id}.exe"
  end
end
