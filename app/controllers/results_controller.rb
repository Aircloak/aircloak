require './lib/proto/air/aggregate_results.pb'
require './lib/result_handler'

class ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create 
  around_action :validate_auth_token, only: :create

  def index
    @queries = Query.where(update_query: false)
  end

  def show
    @query = Query.find(params[:id])
  end

  def create
    r = ResultProto.decode(request.body.read)
    task_id = r.task_id
    if task_id == @pending_result.query_id then
      r.properties.each {|prop| ResultHandler.add_property_result task_id, r.index, prop}
          unless r.properties.blank?
      r.exceptions.each {|expt| ExceptionResult.create_from_proto task_id, r.analyst_id, r.index, expt}
          unless r.exceptions.blank?
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
end
