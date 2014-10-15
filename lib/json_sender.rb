require './lib/token_generator'

class JsonSender
  def self.post_as_task_runner analyst, cluster, path, json, headers = {}
    ssl_cert = OpenSSL::X509::Certificate.new(analyst.task_runner_cert)
    ssl_key = TokenGenerator.import_key analyst.task_runner_key
    post analyst, cluster, path, json, ssl_cert, ssl_key, headers
  end

  def self.post_as_admin analyst, cluster, path, json, headers = {}
    ssl_cert = OpenSSL::X509::Certificate.new(analyst.admin_cert)
    ssl_key = TokenGenerator.import_key analyst.admin_key
    post analyst, cluster, path, json, ssl_cert, ssl_key, headers
  end

  # Useful when testing locally against a local cloak-core which does not
  # perform authentication. For example used in the rake task that
  # generates user data.
  def self.post_without_authentication analyst, cluster, path, json, headers = {}
    post analyst, cluster, path, json, nil, nil, headers
  end

  def self.post analyst, cluster, path, json, ssl_cert, ssl_key, headers = {}
    protocol = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    url = "#{protocol}://#{cluster.random_cloak_ip}:#{port}/#{path}"
    headers = {analyst: analyst.id}.merge(headers)
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
  rescue Exception => e
    Rails.logger.error "Cloak request error: #{e.message}\n#{e.backtrace.join("\n")}"
    {}
  end
end
