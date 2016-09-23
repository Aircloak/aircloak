#!/usr/bin/env ruby

require 'net/http'
require 'uri'
require 'openssl'
require 'json'

if ENV["API_TOKEN"].nil? then
  puts "Please export API_TOKEN"
  exit(1)
end

class RestClient
  def self.get path, api_token, headers = {}
    uri = URI.parse path

    http = Net::HTTP.new uri.host, uri.port
    http.read_timeout = 300
    http.use_ssl = true
    http.verify_mode = OpenSSL::SSL::VERIFY_NONE

    request = Net::HTTP::Get.new uri.path
    request.add_field "auth-token", api_token
    headers.each_pair do |header_name, value|
      request.add_field header_name.to_s, value
    end

    response = http.request request
    response.body
  end

  def self.post path, api_token, payload = '', headers = {}
    uri = URI.parse path

    http = Net::HTTP.new uri.host, uri.port
    http.read_timeout = 300
    http.use_ssl = true
    http.verify_mode = OpenSSL::SSL::VERIFY_NONE

    request = Net::HTTP::Post.new uri.path
    request.add_field "auth-token", api_token
    headers.each_pair do |header_name, value|
      request.add_field header_name.to_s, value
    end

    response = http.request request, payload
    response.body
  end
end

site_url = "https://insights.air-local:20000"
api_token = ENV["API_TOKEN"]

# fetching data sources
data_sources = JSON.parse(RestClient.get "#{site_url}/api/data_sources", api_token)
data_source_token = data_sources[0]["token"]

# running a query
statement = "SELECT itemname, count(*) FROM purchases GROUP by itemname"
run_query_response = JSON.parse(
  RestClient.post "#{site_url}/api/queries",
      api_token,
      {
        query: {
          statement: statement,
          data_source_token: data_source_token
        }
      }.to_json,
      {"Content-Type" => "application/json"}
)

# getting the query result
query_id = run_query_response["query_id"]
puts RestClient.get "#{site_url}/api/queries/#{query_id}",
      api_token

