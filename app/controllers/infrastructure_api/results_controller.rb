require 'csv'
require 'json'
require './lib/result_handler'

class InfrastructureApi::ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create
  around_action :validate_auth_token, only: :create

  def create
    if request.content_type == "application/json" then
      json = JSON.parse(request.raw_post)
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
      ResultHandler.store_results task, json, published_at
    end
    render text: "Got it buddy, thanks", layout: false
  end

private
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
