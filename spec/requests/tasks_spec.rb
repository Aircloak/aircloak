require 'spec_helper'
require './lib/protobuf_sender'

describe "TasksController" do
  setup :activate_authlogic

  before(:each) do
    ProtobufSender.stub(:post)
    ProtobufSender.stub(:send_delete)
    Task.destroy_all
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    Build.destroy_all
    BuildManager.stub(:send_build_request)
    Result.destroy_all
    Bucket.destroy_all
    Analyst.destroy_all
    log_in user
  end

  let!(:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let!(:build) { Build.create(name: "build") }
  let!(:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  before(:each) do
    cloak.cluster_cloak.set_state :belongs_to
    cloak.cluster_cloak.save.should eq true
  end

  let(:analyst) { Analyst.create name: "test-analyst" }
  let(:admin_permission) { Permission.create name: "admin", description: "" }
  let(:user) {
    User.create login: "test", email: "test@example.com", password: "1234",
        password_confirmation: "1234", analyst_id: analyst.id, permissions: [admin_permission]
  }
  let(:token) { AnalystToken.create_api_token(analyst) }

  describe "POST /tasks/:id/execute_as_batch_task" do
    it "should execute the task" do
      t = double(cluster: cluster, name: "foo")
      analyst.tasks.class.any_instance.stub(:find).and_return(t)
      t.should_receive(:execute_batch_task)
      post "/tasks/foobar/execute_as_batch_task", {format: :json}
    end
  end
end
