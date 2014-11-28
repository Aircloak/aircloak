require 'openssl'

# Contains functionality for interacting with the AirPub server

module AirpubApi
  def self.generate_subscribe_request path
    request = "subscribe path=#{path} timestamp=#{Time.now.to_i * 1000}"
    shared_secret = Rails.configuration.airpub_shared_secret
    hash = OpenSSL::HMAC.hexdigest('sha256', shared_secret, request)
    "#{hash} #{request}"
  end
end
