require 'csv'
require 'json'
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
    if request.content_type == "application/json" then
      hash = JSON.parse(request.raw_post)
      r = convert_hash_to_result(hash)
    elsif request.content_type == "application/x-protobuf" then
      r = ResultPB.decode(request.raw_post)
    else
      render text: "Content-type not supported!", status: 501, layout: false
      return
    end
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

  # this is a temporary hack until we get rid of protocol buffers completely
  def convert_hash_to_result hash
    result = ResultPB.new(analyst_id: hash["analyst_id"], task_id: hash["task_id"], buckets: [], exceptions: [])
    hash["buckets"].each do |bucket|
      if bucket["range"] != nil then
        range = BucketPB::RangeProto.new(min: bucket["range"]["min"], max: bucket["range"]["max"])
      else
        range = nil
      end
      result.buckets.push BucketPB.new(label: bucket["label"], string: bucket["value"],
          range: range, accumulated_count: bucket["count"])
    end unless hash["buckets"] == nil
    hash["exceptions"].each do |exception|
      result.exceptions.push ExceptionPB.new(stackEntry: exception["stackEntry"],
          accumulated_count: exception["accumulated_count"])
    end unless hash["exceptions"] == nil
    return result
  end
end
