require './lib/proto/cloak/query.pb'
require 'net/http'
require 'net/https'
require 'uri'
require "base64"

class Query < ActiveRecord::Base
  has_many :pending_results, dependent: :destroy, counter_cache: true

  has_many :results, dependent: :destroy
  has_many :buckets, through: :result
  has_many :exception_results, dependent: :destroy

  belongs_to :task
  belongs_to :cluster

  validates :name, :task_id, :presence => true

  after_save :upload_stored_query
  after_destroy :remove_query_from_cloak

  def cquery
    cquery = CQuery.new(
      type: self.task.mutator ? CQuery::Type::MUTATOR : CQuery::Type::READER,
      query_id: self.id,
      index: "all_users",
      # TODO: Change to real id of analyst when we start introducing that
      analyst_id: self.task.system_query ? "aircloak" : "some_analyst",
      system_query: self.task.system_query,
      main_class: self.task.main_package
    )

    if self.task.update_task then
      # Update queries are such that they are run when data about
      # a user arrives in the cloak. They never send results back
      # outside the cloak, hence they don't need a return address either.
      cquery.stored_options = CQuery::StoredOptions.new(payload_identifier: self.task.payload_identifier)
      cquery.query_class = CQuery::QueryClass::STORED
    else
      # Queries that are run on demand, need an address to return
      # the results back to.
      domain = if Rails.env.production? then
        "http://graphite.mpi-sws.org:5000/results"
      else
        "http://localhost:3000/results"
      end
      cquery.batch_options = CQuery::BatchOptions.new(url: domain)
      cquery.query_class = CQuery::QueryClass::BATCH
    end

    # Attach the data for the query binaries from the stored data
    qd = QueryData.decode(self.task.packaged_data.dup) # TODO(#106): find out why we duplicate here, again?
    cquery.data = qd.data

    cquery
  end

  class NotABatchQueryException < Exception 
  end

  def execute_batch_query
    if task.update_task
      raise NotABatchQueryException.new("Query #{self.id} uses an update task, not a batch task")
    end
    response = post_query url: cloak_url("batch_query"), expect_response: true
    d =  ActiveSupport::JSON.decode(response.body)
    unless d['status'].downcase == 'ok' then
      # TODO: LOG
    end
  end

  # The name of whom this query runs on behalf of
  def on_behalf_of
    task.system_query ? "Aircloak" : "Analyst"
  end

  def ready_for_primetime
    self.name != nil
  end

private

  def upload_stored_query
    return unless task.update_task
    return unless ready_for_primetime
    post_query url: cloak_url("query")
  end

  def remove_query_from_cloak
    return unless task.update_task
    return unless ready_for_primetime
    delete_query url: cloak_url("query"), id: self.id
  end

  def delete_query args
    url = URI.parse("#{args[:url]}/#{args[:id]}")
    http = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Delete.new(url.path)
    result = http.request(request)
  end

  def post_query args
    sock = ProtobufSender.construct_sock args[:url]
    request = ProtobufSender.construct_request args[:url], self.cquery
    if args[:expect_response] then
      pr = PendingResult.create(query: self)
      request["QueryAuthToken"] = pr.auth_token
    end
    ProtobufSender.post sock, request
  end

  # TODO(#107): here is a bug as @cloak_url doesn't account for changes in path parameter
  def cloak_url path
    return @cloak_url if @cloak_url

    cloak_url = ""
    if Rails.env.production? then
      # Get a cloak to speak to
      resolver = Resolv::DNS.new
      resource = resolver.getresources("_cloak._tcp.aircloak.com", 
                                       Resolv::DNS::Resource::IN::SRV).sample
      cloak_url = resource.target.to_s
      # FIXME: Once DNS has been updated,
      #        allow the cloak to get this through DNS
      cloak_url = "tpm-dell1.mpi-sws.org"
    else
      return "http://localhost:8098/#{path}"
    end

    @cloak_url = "https://#{cloak_url}/#{path}"
  end
end
