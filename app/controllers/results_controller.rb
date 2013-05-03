require './lib/proto/air/aggregate_results.pb'

class ResultsController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create 
  around_action :validate_auth_token, only: :create

  def index
    @queries = Query.all
  end

  def create
    r = Result.decode(request.body.read)
    query_id = r.query_id
    if query_id == @pending_result.query_id then
      r.properties.each {|prop| PropertiesResult.create_from_proto query_id, prop} unless r.properties.blank?
      r.exceptions.each {|expt| ExceptionResult.create_from_proto query_id, expt} unless r.exceptions.blank?
      r.percentiles.each {|expt| PercentileResult.create_from_proto query_id, expt} unless r.percentiles.blank?
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
