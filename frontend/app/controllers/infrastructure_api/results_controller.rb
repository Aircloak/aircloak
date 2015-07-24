require 'csv'
require 'json'
require './lib/result_handler'
require 'zlib'
require 'stringio'

class InfrastructureApi::ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create
  around_action :validate_auth_token, only: :create

  def create
    body = request.raw_post
    if request.headers['Content-encoding'] == "gzip" then
      body = gunzip(body)
    end
    if request.content_type == "application/json" then
      json = JSON.parse(body)
    else
      render text: "Content-type not supported!", status: 501, layout: false
      return
    end
    task = Task.find_by_token(Task.decode_token(json["task_id"]))
    if task.id == @pending_result.task_id then
      published_at = request.headers["PublishedAt"]
      if published_at
        published_at = published_at.to_i # convert to integer
        published_at = Time.at(published_at / 1000, (published_at % 1000) * 1000).utc # convert to time object
      end
      result = ResultHandler.store_results task, json, published_at
      @pending_result.signal_result result
    end
    # One-off tasks need to be deleted after completion.
    # As we still need the results in order to send them
    # back to the client, we don't explicitly destroy the
    # task (which would remove the results), but mark it
    # as deleted to have it garbage collected down the road.
    task.update_attribute(:deleted, true) if task.one_off
    render text: "Got it buddy, thanks", layout: false
  end

private
  def gunzip(data)
    io = StringIO.new(data, "rb")
    gz = Zlib::GzipReader.new(io)
    decompressed = gz.read
  end

  def validate_auth_token
    auth_token = request.headers["QueryAuthToken"]
    @pending_result = PendingResult.where(auth_token: auth_token).first
    if @pending_result.blank? then
      render text: "Illegal auth token", status: 403, layout: false
    else
      yield
    end
  ensure
    @pending_result.destroy unless @pending_result.blank? || @pending_result.standing
  end
end
