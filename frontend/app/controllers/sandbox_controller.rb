require 'net/http'
require 'net/https'
require 'uri'
require 'json'
require './lib/task_code'
require './lib/aircloak_config'

class SandboxController < ApplicationController
  def run
    task_spec = params["task_spec"]
    task_spec["libraries"] = TaskCode.dependencies(task_spec["code"])

    url = URI.parse("#{Conf.get("/service/air-sandbox/endpoint")}/task/run")
    sock = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    request.body = task_spec.to_json
    res = sock.request(request)
    render text: res.body, status: res.code
  end
end
