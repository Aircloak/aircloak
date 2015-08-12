require './lib/token_generator'
require './lib/aircloak_config'

class JsonSender
  def self.request method, auth_type, analyst, cluster, path, headers = {}, body = nil
    unless cluster.has_ready_cloak?
      return {"success" => false, "description" => "No cloak available for cluster #{cluster.name}"}
    end

    unless [:get, :post, :put, :delete, :head].include?(method)
      raise ArgumentError.new("Unsupported REST method #{method}")
    end

    ssl_cert, ssl_key = cert_and_key(auth_type, analyst)

    protocol = Conf.get("/service/cloak/protocol")
    port = Conf.get("/service/cloak/port")
    url = "#{protocol}://#{cluster.address_of_a_ready_cloak}:#{port}/#{path}"
    headers = {analyst: analyst.id, :content_type => 'application/json'}.merge(headers)

    args = []
    args << body if [:post, :put].include?(method)
    args << headers

    parameters = {}
    if Conf.get("/settings/rails/global") == "true"
      parameters.merge!({
            ssl_client_cert: ssl_cert,
            ssl_client_key: ssl_key,
            ssl_ca_file: "/aircloak/ca/cloaks_root.crt"
          })
    end

    # The &block suppresses errors thrown when non-successful status codes
    # are returned by the cloak to indicate that the migration failed.
    raw_result = RestClient::Resource.new(
      url,
      parameters
    ).public_send(method, *args) do |resp, req, result, &block|
      resp
    end
    result = JSON.parse raw_result.body
    result["code"] = raw_result.code
    result
  rescue Exception => e
    Rails.logger.error "Cloak request error: #{e.message}\n#{e.backtrace.join("\n")}"
    {"success" => false, "description" => "Cloak request error: #{e.message}"}
  end

  def self.cert_and_key(auth_type, analyst)
    case auth_type
    when :task_runner
      return [
            OpenSSL::X509::Certificate.new(analyst.task_runner_cert),
            TokenGenerator.import_key(analyst.task_runner_key)
          ]
    when :admin
      return [
            OpenSSL::X509::Certificate.new(analyst.admin_cert),
            TokenGenerator.import_key(analyst.admin_key)
          ]
    when :no_auth
      return [nil, nil]
    else
      raise ArgumentError.new("Invalid authentication type #{auth_type}")
    end
  end
end
