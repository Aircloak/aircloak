require 'net/http'
require 'net/https'
require 'uri'

class SandboxController < ApplicationController
  def run
    url = URI.parse("#{Rails.configuration.local_sandbox.endpoint}/task/run")
    sock = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    request.body = params["payload"]
    res = sock.request(request)
    render text: res.body, status: res.code
  end
end