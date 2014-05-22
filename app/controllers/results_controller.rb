require './lib/proto/air/aggregate_results.pb'
require './lib/result_handler'

class ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create
  around_action :validate_auth_token, only: :create

  def index
    @tasks = Task.where(stored_task: false)
  end

  def show
    @task = Task.find(params[:id])
    @results = Result.where(task: @task).order(result_id: :asc)
    buckets = Bucket.where(task: @task).order(label: :asc, str_answer: :asc, range_min: :asc)
    @buckets = ResultsController.process_buckets buckets
  end

  def create
    r = ResultPB.decode(request.raw_post)
    task_id = r.task_id
    if task_id == @pending_result.task_id then
      ResultHandler.store_results Task.find(task_id), r
      unless r.exceptions.blank?
        r.exceptions.each {|expt| ExceptionResult.create_from_proto task_id, r.analyst_id, r.index, expt}
      end
    end
    render text: "Got it buddy, thanks", layout: false
  end

  def destroy
    Property.find(params[:id]).destroy
    redirect_to results_path
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

  # create for each bucket a corresponding entry for each result (if available)
  def self.process_buckets buckets
    @buckets = {}
    buckets.each do |bucket|
      name = bucket.display_name
      @buckets[name] ||= {name: name, bucket: {}}
      @buckets[name][:results][bucket.result.result_id] = bucket.display_result
    end
  end
end
