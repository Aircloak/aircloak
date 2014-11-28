require 'airpub_api'

class AirpubController < ApplicationController
  protect_from_forgery :except => :event

  def index
  end

  def subscribe
    @path = params['path']
    @request = AirpubApi.generate_subscribe_request @path
    @server_url = Rails.configuration.airpub_ws_subscribe
  end
end
