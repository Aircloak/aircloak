require './lib/token_generator'

class JsonSender
  def self.post analyst, cluster, path, json, headers = {}
    protocol = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    url = "#{protocol}://#{cluster.random_cloak_ip}:#{port}/#{path}"
    headers = {analyst: analyst.id}.merge(headers)
    ssl_cert = OpenSSL::X509::Certificate.new(analyst.inquirer_cert)
    ssl_key = TokenGenerator.import_key analyst.inquirer_key
    # The &block suppresses errors thrown when non-successful status codes
    # are returned by the cloak to indicate that the migration failed.
    raw_result = RestClient::Resource.new(
      url,
      ssl_client_cert: ssl_cert,
      ssl_client_key: ssl_key
    ).post(json, headers) do |resp, req, result, &block|
      resp
    end
    JSON.parse raw_result
  end
end
