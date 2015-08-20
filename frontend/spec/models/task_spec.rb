require 'spec_helper'
require 'prefetch_helper'

describe Task do
  include PrefetchHelper

  before(:each) do
    Build.delete_all
    Cluster.delete_all
    Cloak.delete_all
    Task.delete_all
    Result.delete_all
    ExceptionResult.delete_all
    PendingResult.delete_all
  end

  it "should have required attributes" do
    required_fields = [:name, :sandbox_type, :code, :cluster, :data]
    task1 = Task.new(required_fields.inject({}) {|memo, field| memo.merge(field => nil)})
    task1.save.should eq false

    required_fields.each do |field|
      fail "not verifying presence of field #{field}" unless task1.errors[field][0] == "can't be blank"
    end
  end

  it "validates prefetch" do
    try_invalid_save(base_attrs.merge(data: "[]"))[:data].should eq ["can't be blank"]
  end

  it "validates streaming task" do
    try_invalid_save(base_attrs.merge(task_type: Task::STREAMING_TASK))[:report_interval].
        should eq ["can't be blank"]

    try_invalid_save(base_attrs.merge(task_type: Task::STREAMING_TASK))[:user_expire_interval].
        should eq ["can't be blank"]
  end

  it "validates periodic task" do
    try_invalid_save(base_attrs.merge(task_type: Task::PERIODIC_TASK))[:period].
        should eq ["can't be blank"]
  end

  it "converts data" do
    prefetch_conversions.each do |data, prefetch|
      task1 = Task.new(base_attrs.merge(prefetch: prefetch))
      task1.stub(:analyst) {analyst_double([age_table_double])}
      task2 = Task.new
      task2.stub(:analyst) {analyst_double([age_table_double])}
      task2.attributes = base_attrs.merge(data: task1.data)
      task2.prefetch.should eq prefetch
    end
  end

  def valid_prefetch
    prefetch =  '{"table":"test1","where":{"\$\$priority": {"$lt": 3}}}'
  end

  def cloak
    Cloak.create
  end

  def build
    Build.create
  end

  def cluster
    Cluster.create(
      name: "cluster-name",
      build: build
    )
  end

  def create_task params={}
    prefetch_data = params.delete(:prefetch) || '{"table":"test1","where":{"\$\$priority": {"$lt": 3}}}'
    PrefetchFilter.should_receive(:data_to_prefetch).and_return(prefetch_data)
    Task.create(
      name: params.delete(:name) || "test-task",
      code: params.delete(:code) || "code",
      data: prefetch_data,
      cluster: cluster
    )
  end

  it "generates token" do
    task = create_task
    task.token.should_not be_nil
  end

  it "efficiently deletes relationships" do
    task = create_task
    p = PendingResult.create task: task
    result = Result.create task: task
    ExceptionResult.create(
      result_id: result.id,
      stacktrace: "trace",
      count: 1
    )
    Result.count.should == 1
    ExceptionResult.count.should == 1
    PendingResult.count.should == 1
    task.efficiently_delete_results
    Result.count.should == 0
    ExceptionResult.count.should == 0
    PendingResult.count.should == 0
  end

  def add_ready_cloak(cluster)
    newCloak = Cloak.create(
      name: "localhost",
      ip: "127.0.0.1"
    )
    cluster.cloaks << newCloak
    newCloak.save
    cluster.save
    newCloak.cluster_cloak.raw_state = ClusterCloak.state_to_raw_state(:belongs_to)
    newCloak.cluster_cloak.save
  end

  it "should send a batch task to the cloak on execute" do
    task = create_task
    JsonSender.should_receive(:request).and_return({success: true})
    add_ready_cloak(task.cluster)
    task.cluster.should_receive(:capable_of?).and_return(true)
    task.save_and_synchronize!
    task.execute_batch_task
  end

  it "should not send a batch task to the cloak on execute if no cloak is ready" do
    task = create_task
    JsonSender.should_not_receive(:request)
    task.cluster.cloaks << Cloak.create(name: "localhost", ip: "127.0.0.1")
    task.cluster.save
    task.save_and_synchronize!
    task.execute_batch_task
  end

  it "should upload stored tasks" do
    task = create_task
    task.should_receive(:upload_stored_task)
    task.should_not_receive(:remove_task_from_cloak)
    add_ready_cloak(task.cluster)
    task.stored_task = true
    task.save_and_synchronize!
  end

  it "should not upload stored tasks if no cloak is ready" do
    task = create_task
    task.should_not_receive(:upload_stored_task)
    task.should_not_receive(:remove_task_from_cloak)
    task.cluster.cloaks << Cloak.create(name: "localhost", ip: "127.0.0.1")
    task.cluster.save
    task.stored_task = true
    task.save_and_synchronize!
  end

  it "should not upload stored tasks if the task is not active" do
    task = create_task
    task.should_not_receive(:upload_stored_task)
    task.should_receive(:remove_task_from_cloak)
    add_ready_cloak(task.cluster)
    task.stored_task = true
    task.active = false
    task.save_and_synchronize!
  end

  private
    def base_attrs
      {name: "name", sandbox_type: "lua", code: "code", cluster: Cluster.new}
    end

    def try_invalid_save(task_spec)
      task = Task.new(task_spec)
      task.save.should eq false
      task.errors
    end
end
