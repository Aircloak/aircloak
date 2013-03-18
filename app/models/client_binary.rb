require 'digest/sha1'
require 'base64'

class ClientBinary < ActiveRecord::Base
  def self.binary_from_file(tempfile) 
    c = ClientBinary.new
    c.data = tempfile.gets
    # c.sha1 = Base64.encode64 Digest::SHA1.hexdigest c.data
    c.sha1 = Digest::SHA1.hexdigest c.data
    c.size = c.data.size
    return c
  end

  def xml_type
    updater ? "updater" : "client"
  end

  def xml_name
    updater ? "AircloakClient.#{id}.exe" : "AircloakClientUpdater.#{id}.exe"
  end
end
