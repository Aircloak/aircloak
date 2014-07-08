require 'csv'
require './lib/proto/air/aggregate_results.pb'
require './lib/result_handler'

class ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create
  around_action :validate_auth_token, only: :create

  def show
    @result = Result.find(params[:id])
  end

  def create
    r = ResultPB.decode(request.raw_post)
    task_id = Task.decode_id(r.task_id)
    if task_id == @pending_result.task_id then
      ResultHandler.store_results Task.find(task_id), r
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
    @pending_result.destroy unless @pending_result.blank?
  end
end
