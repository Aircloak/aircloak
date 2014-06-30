require './lib/proto/cloak/task.pb.rb'
require './lib/json_sender'

class Task < ActiveRecord::Base
  has_many :pending_results, dependent: :destroy, counter_cache: true

  has_many :results, dependent: :destroy
  has_many :buckets, through: :result
  has_many :exception_results, dependent: :destroy

  belongs_to :cluster
  belongs_to :analyst

  validates_presence_of :name, :sandbox_type, :prefetch, :code, :cluster
  validates_uniqueness_of :name
  validate :stored_task_must_have_payload_identifier

  after_save :upload_stored_task
  after_destroy :remove_task_from_cloak

  class InvalidTaskId < Exception; end

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

  # This does an efficient SQL delete, rather than
  # loading all the data, running all the validations
  # and callbacks, etc
  def efficient_delete
    Bucket.connection.execute "DELETE FROM buckets WHERE result_id IN (SELECT id FROM results WHERE task_id = #{self.id})"
    Result.connection.execute "DELETE FROM results WHERE task_id = #{self.id}"
    destroy
  end

  def index
    "all_users"
  end

  def task_upload_pb
    metainfo = TaskMetaInfoPB.new(
      name: self.name,
      task_type: self.update_task ? TaskMetaInfoPB::TaskType::UPDATE : TaskMetaInfoPB::TaskType::QUERY,
      task_id: self.id,
      index: self.index,
      # TODO(#110): Change to real id of analyst when we start introducing that
      analyst_id: self.analyst
    )
    metainfo.payload_identifier = self.payload_identifier if self.stored_task
    code = TaskCodePB.new(
      sandbox_type: self.sandbox_type,
      code: self.code
    )
    TaskUploadPB.new(
      task_code: code,
      meta_info: metainfo
    )
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
      response = JsonSender.post(
        analyst,
        cluster,
        "task/run",
        {prefetch: JSON.parse(prefetch), post_processing: {code: code}}.to_json,
        "task_id" => self.class.encode_id(id),
        "auth_token" => pr.auth_token
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

private

  def stored_task_must_have_payload_identifier
    return unless self.stored_task
    unless self.payload_identifier? && self.payload_identifier.length > 0
      self.errors.add :tasks, "must have payload identifier"
    end
  end

  def upload_stored_task
    return unless self.stored_task
    url = cloak_url("query")
    post_task url: url if url
  end

  def remove_task_from_cloak
    return unless self.stored_task
    url = cloak_url("query")
    delete_task url: url, id: self.id if url
  end

  def delete_task args
    url = URI.parse("#{args[:url]}/#{args[:id]}")
    http = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Delete.new(url.path)
    result = http.request(request)
  end

  def post_task args
    sock = ProtobufSender.construct_sock args[:url]
    request = ProtobufSender.construct_request args[:url], self.task_upload_pb
    if args[:expect_response] then
      pr = PendingResult.create(task: self)
      request["QueryAuthToken"] = pr.auth_token
    end
    ProtobufSender.post sock, request
  end

  def cloak_url path
    cluster_cloaks = ClusterCloak.where(cluster: cluster,
        raw_state: ClusterCloak.state_to_raw_state(:belongs_to)).limit(1)
    prot = Rails.configuration.cloak.protocol
    port = Rails.configuration.cloak.port
    return "#{prot}://#{cluster_cloaks.first.cloak.ip}:#{port}/#{path}" if cluster_cloaks.count > 0
  end
end
