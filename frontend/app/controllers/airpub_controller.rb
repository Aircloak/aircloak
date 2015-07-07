require 'airpub_api'
require './lib/aircloak_config'

class AirpubController < ApplicationController
  protect_from_forgery :except => :event

  def index
    @analyst_id = current_user.analyst.id unless current_user.analyst.nil?
  end

  def subscribe
    @path = params['path']
    @request = AirpubApi.generate_subscribe_request @path
    @server_url = Conf.get("/service/airpub/subscribe_endpoint")
  end
end
