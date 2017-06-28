# REST API

The Aircloak REST API can be used to perform actions such as executing queries against a cloak, or access results of previous query executions. All API access is authenticated, as explained in [the authentication section](#authentication).

## Authentication

All APIs are authenticated using API tokens. You need to pass the token in each API request through the `auth-token` header.


{% codetabs name="Ruby", type="rb" -%}

require 'net/http'
require 'uri'
require 'openssl'
require 'json'

class RestClient
  def self.get(path, api_token, headers = {})
    uri = URI.parse(path)

    http = Net::HTTP.new(uri.host, uri.port)
    http.read_timeout = 300
    http.use_ssl = true

    request = Net::HTTP::Get.new(uri.path)
    request.add_field("auth-token", api_token)
    headers.each_pair do |header_name, value|
      request.add_field(header_name.to_s, value)
    end

    response = http.request(request)
    response.body
  end

  def self.post(path, api_token, payload = '', headers = {})
    uri = URI.parse(path)

    http = Net::HTTP.new(uri.host, uri.port)
    http.read_timeout = 300
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri.path)
    request.add_field("auth-token", api_token)
    headers.each_pair do |header_name, value|
      request.add_field(header_name.to_s, value)
    end

    response = http.request(request, payload)
    response.body
  end
end

# get example
RestClient.get("#{air_server}/#{request_path}", api_token, headers)

# post exaple
RestClient.post("#{air_server}/#{request_path}", api_token, payload, headers)


{%- language name="wget", type="sh" -%}

export $SITE_URL=<air-server-url>
export $API_TOKEN=<api-token>

wget --content-on-error \
     --output-document - \
     --method=<GET|POST|PUT|DELETE> \
     --header "auth-token:$API_TOKEN" \
     $SITE_URL/<request-path>


{%- language name="curl", type="sh" -%}

export $SITE_URL=<air-server-url>
export $API_TOKEN=<api-token>

curl -k -X <GET|POST|PUT|DELETE> \
     -H 'auth-token:$API_TOKEN' \
    $SITE_URL/<request-path>
{%- endcodetabs %}

You can create an API token on [this page](/api_tokens).
