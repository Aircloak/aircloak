class JsonSender
  def self.post cluster, path, json
    protocol = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    url = "#{protocol}://#{cluster.random_cloak_ip}:#{port}/#{path}"
    # The &block suppresses errors thrown when non-successful status codes
    # are returned by the cloak to indicate that the migration failed.
    raw_result = RestClient.post url, json, analyst: 1 do |resp, req, result, &block|
      resp
    end
    JSON.parse raw_result
  end
end
