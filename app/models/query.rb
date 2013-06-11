require './lib/proto/cloak/query.pb'
require 'net/http'
require 'uri'
require "base64"

class Query < ActiveRecord::Base
  has_many :query_files, dependent: :destroy
  has_many :pending_results, dependent: :destroy, counter_cache: true

  has_many :properties, dependent: :destroy
  has_many :percentiles, dependent: :destroy
  has_many :exception_results, dependent: :destroy

  belongs_to :index

  accepts_nested_attributes_for :query_files, allow_destroy: true

  validates :name, :index_id, :presence => true
  validate :existing_identifier

  before_validation :should_have_identifier?
  after_save :upload_stored_query
  after_destroy :remove_query_from_cloak

  def cquery
    cquery = CQuery.new(
      type: self.mutator ? CQuery::Type::MUTATOR : CQuery::Type::READER,
      query_id: self.id,
      index: self.index.name,
      # TODO: Change to real id of analyst when we start introducing that
      analyst_id: self.system_query ? "aircloak" : "some_analyst",
      system_query: self.system_query
    )
    if self.update_query then
      cquery.stored_options = CQuery::StoredOptions.new(payload_identifier: self.identifier)
      cquery.query_class = CQuery::QueryClass::STORED
    else
      # Extract out URL to some place sensible
      domain = Rails.env.production? ? "http://www.aircloak.com/results" : "http://localhost:3000/results"
      cquery.batch_options = CQuery::BatchOptions.new(url: domain)
      cquery.query_class = CQuery::QueryClass::BATCH
    end
    self.query_files.each do |qf|
      cquery.main_class = qf.package if qf.query_interface
      cquery.data ||= []
      cquery.data << QueryBinary.new(
        package: qf.package,
        data: qf.data
      )
    end
    cquery
  end

  class NotABatchQueryException < Exception 
  end

  def execute_batch_query
    raise NotABatchQueryException.new("Query #{self.id} is an update query, not a batch query") if self.update_query
    response = post_query url: cloak_url("batch_query"), expect_response: true
    d =  ActiveSupport::JSON.decode(response.body)
    unless d['status'].downcase == 'ok' then
      # TODO: LOG
    end
  end

  # The name of whom this query runs on behalf of
  def on_behalf_of
    system_query ? "Aircloak" : "Analyst"
  end

private

  def upload_stored_query
    return unless self.update_query
    post_query url: cloak_url("query")
  end

  def remove_query_from_cloak
    return unless self.update_query
    delete_query url: cloak_url("query"), id: self.id
  end

  def delete_query args
    url = URI.parse("#{args[:url]}/#{args[:id]}")
    http = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Delete.new(url.path)
    result = http.request(request)
  end

  def post_query args
    url = URI.parse(args[:url])
    http = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    if args[:expect_response] then
      pr = PendingResult.create(query: self)
      request["QueryAuthToken"] = pr.auth_token
    end
    request.content_type = "application/x-protobuf"
    request.body = self.cquery.encode.buf
    result = http.request(request)
  end

  def cloak_url path
    return @cloak_url if @cloak_url

    cloak_url = ""
    if Rails.env.production? then
      # Get a cloak to speak to
      resolver = Resolv::DNS.new
      resource = resolver.getresources("_cloak._tcp.aircloak.com", 
                                       Resolv::DNS::Resource::IN::SRV).sample
      cloak_url = resource.target.to_s
    else
      cloak_url = "http://localhost:8098"
    end

    @cloak_url = "#{cloak_url}/#{path}"
  end

  def should_have_identifier?
    self.identifier = "" unless update_query
  end

  def existing_identifier
    errors.add(:identifier, "is required for update queries") if update_query && identifier.blank?
  end
end
