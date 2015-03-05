require './lib/proto/cloak/task.pb.rb'
require './lib/json_sender'
require './lib/prefetch_filter'
require './lib/task_code'

class Task < ActiveRecord::Base
  has_many :pending_results, dependent: :destroy, counter_cache: true

  has_many :results, dependent: :destroy
  has_many :buckets, through: :results, dependent: :destroy
  has_many :exception_results, through: :results, dependent: :destroy

  belongs_to :cluster
  belongs_to :analyst

  validates_presence_of :name, :sandbox_type, :code, :cluster
  validates_uniqueness_of :name, scope: [:analyst_id]
  validate :prefetch_correct
  validate :streaming_task
  validate :periodic_task

  before_create :generate_token

  after_save :upload_stored_task
  after_destroy :remove_task_from_cloak

  class InvalidTaskId < Exception; end

  BATCH_TASK = 1
  STREAMING_TASK = 2
  PERIODIC_TASK = 3

  JSON_TYPES = {
    STREAMING_TASK => "streaming",
    PERIODIC_TASK => "periodic"
  }

  def self.types
    [
      OpenStruct.new({id: BATCH_TASK, name: "Batch"}),
      OpenStruct.new({id: PERIODIC_TASK, name: "Periodic"}),
      OpenStruct.new({id: STREAMING_TASK, name: "Streaming"})
    ]
  end

  def self.decode_id(encoded_task_id)
    parts = encoded_task_id.split(/^task\-/)
    if parts.length != 2 || parts[1].empty?
      raise InvalidTaskId.new(message: "#{task_id}")
    end
    parts[1].to_i
  end

  def self.encode_id(task_id)
    "task-#{task_id}"
  end

  # This does an efficient SQL delete, rather than loading all the data,
  # running all the validations and callbacks, etc
  def efficiently_delete_results
    Result.delete_for_task self
    PendingResult.delete_for_task self
    ExceptionResult.delete_for_task self
  end

  # This does an efficient SQL delete, rather than loading all the data,
  # running all the validations and callbacks, etc
  def efficient_destroy
    ActiveRecord::Base.transaction do
      efficiently_delete_results
      destroy
    end
  end

  def index
    "all_users"
  end

  class NotABatchTaskException < Exception
  end

  def execute_batch_task
    if stored_task
      raise NotABatchTaskException.new("Task #{self.id} is a stream task, not a batch task")
    end
    url = cloak_url("/task/run")
    if url
      pr = PendingResult.create(task: self)
      headers = {
        "task_id" => self.class.encode_id(id),
        "async_query" => "true",
        "auth_token" => pr.auth_token
      }
      publish_url = Rails.configuration.publish_url
      if publish_url.present? then
        # publish to "/results/analyst_id/task_id/cluster_id/token"
        publish_path = "/results/#{analyst.id}/#{self.id}/#{cluster.id}/#{pr.auth_token}"
        headers.merge!({"return_url" => Base64.strict_encode64(publish_url + publish_path)})
      end
      response = JsonSender.request(
        :post,
        :task_runner,
        analyst,
        cluster,
        "task/run",
        headers,
        {
          prefetch: JSON.parse(prefetch),
          post_processing: post_processing_spec
        }.to_json
      )
      unless response["success"] == true then
        # TODO: LOG
      end
    end
  end

  # The name of whom this query runs on behalf of
  def on_behalf_of
    # TODO(#110): Change to real id of analyst when we start introducing that
    "Aircloak"
  end

  # This is a pseudo-attribute which is used to set/retrieve prefetch query.
  #
  # We differ between two terms: prefetch and data:
  #   1. Prefetch is a json describing an sql query as understood by cloak
  #   2. Data is a json describing a query as understood by web UI
  #
  # The reason for this distinction is because cloak supports much more
  # general query interface, while UI has limited query support. Therefore, we
  # keep the UI query format much simpler, and perform this translation. Since
  # UI format is significantly simpler, we don't need to implement another
  # complicated query parser in JavaScript.
  def data=(value)
    @prefetch_error = nil
    self.prefetch = PrefetchFilter.data_to_prefetch(self, value)
  rescue InvalidPrefetchFilter => e
    @prefetch_error = e.message
  end

  def data
    PrefetchFilter.prefetch_to_data(prefetch, cluster_id)
  end

  # Returns true if and only if the latest
  # result was an exception
  def has_exceptions?
    results.count > 0 && results.last.exception_results.count > 0
  end

  def latest_exceptions
    results.last.exception_results
  end

  def generate_token
    return unless self.token.nil?

    begin
      # We use the size 4, which should usually amount to 6 Base64 chars, or
      # approximately 16 millions of different combinations. This gives us
      # enough of space, and at the same time keeps tokens manageable for
      # humans.
      new_token = TokenGenerator.generate_random_string_of_at_least_length 4
    end while self.class.where(token: new_token).count != 0
    self.token = new_token
  end

  def type_string
    self.class.types.
        find {|type_rec| type_rec.id == self.task_type}.
        name.
        downcase
  end

  def batch_task?
    task_type == BATCH_TASK
  end

private

  def prefetch_correct
    if @prefetch_error.nil?
      self.errors.add :data, "can't be blank" if prefetch.nil? || prefetch.empty?
    else
      self.errors.add :data, @prefetch_error
    end
  end

  def streaming_task
    return if task_type != STREAMING_TASK
    self.errors.add :report_interval, "can't be blank" if report_interval.nil?
    self.errors.add :user_expire_interval, "can't be blank" if user_expire_interval.nil?
  end

  def periodic_task
    return if task_type != PERIODIC_TASK
    self.errors.add :period, "can't be blank" if period.nil? || period.empty?
  end

  def upload_stored_task
    return unless self.stored_task && cloak

    pr = PendingResult.where(task: self).first || PendingResult.create(task: self, standing: true)
    headers = {"auth_token" => pr.auth_token}
    publish_url = Rails.configuration.publish_url
    if publish_url.present? then
      publish_path = "/results/#{analyst.id}/#{self.id}/#{cluster.id}/#{pr.auth_token}"
      headers.merge!({"return_url" => Base64.strict_encode64(publish_url + publish_path)})
    end
    response = JsonSender.request(
          :put,
          :task_runner,
          analyst,
          cluster,
          "task/#{self.class.encode_id(id)}",
          headers,
          {
            type: JSON_TYPES[task_type],
            report_interval: nilify(report_interval),
            user_expire_interval: nilify(user_expire_interval),
            period: nilify(period) {JSON.parse(period)},
            prefetch: JSON.parse(prefetch),
            post_processing: post_processing_spec
          }.
            select {|key, value| !value.nil?}.
            to_json
        )
    unless response["success"] == true then
      # TODO: LOG
    end
  end

  def nilify(value)
    if value && value != ""
      block_given? ? yield : value
    else
      nil
    end
  end

  class RemoveError < Exception; end

  def remove_task_from_cloak
    return unless self.stored_task && cloak

    response = JsonSender.request(:delete, :task_runner, analyst, cluster,
        "task/#{self.class.encode_id(id)}", {})

    unless response["success"] == true then
      raise RemoveError.new("Failed removing the task from the cluster.")
    end
  end

  def cloak_url path
    raise "No cloak in cluster" unless cloak
    prot = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    return "#{prot}://#{cloak.ip}:#{port}/#{path}"
  end

  def cloak
    cluster_cloak = ClusterCloak.where(cluster_id: cluster_id, raw_state: ClusterCloak.state_to_raw_state(:belongs_to)).limit(1).first
    cluster_cloak.cloak if cluster_cloak
  end

  def post_processing_spec
    if cluster.capable_of? :lua_library_support then
      {code: code, libraries: TaskCode.dependencies(code)}
    else
      code_parts = TaskCode.dependencies(code).inject([]) do |memo, library|
        memo.push(library[:code])
      end
      code_parts.push(code)
      {code: code_parts.join("\n")}
    end
  end
end
