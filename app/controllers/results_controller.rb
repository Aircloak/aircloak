require 'csv'
require './lib/proto/air/aggregate_results.pb'
require './lib/result_handler'

class ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create
  before_filter :load_task, only: :show
  around_action :validate_auth_token, only: :create

  def show
    respond_to do |format|
      format.html

      format.csv do
        filename = "task_results_#{@task.id}_#{Time.now.strftime("%Y%m%d%H%M")}.csv"
        response.headers['Content-Disposition'] = "attachment; filename=\"#{filename}\""
        render :text => results_csv
      end
    end
  end

  def create
    r = ResultPB.decode(request.raw_post)
    task_id = Task.decode_id(r.task_id)
    if task_id == @pending_result.task_id then
      ResultHandler.store_results Task.find(task_id), r
      unless r.exceptions.blank?
        r.exceptions.each {|expt| ExceptionResult.create_from_proto task_id, r.analyst_id, expt}
      end
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

  def load_task
    @task = current_user.analyst.tasks.find(params[:id], include: {:results => [:buckets]})
  end

  def results_csv
    CSV.generate(col_sep: ";") do |csv|
      csv << @task.result_set.keys
      @task.results.each do |result|
        csv << @task.result_set.map {|name, bucket| bucket[result.id] || ""}
      end
    end
  end
end
