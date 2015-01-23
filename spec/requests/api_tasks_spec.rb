require 'spec_helper'
require './lib/protobuf_sender'

describe "ApiTasksController" do
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
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  describe "getting queries and results, and destroying queries" do
    before(:each) do
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true
    end

    let (:analyst) { Analyst.create name: "test-analyst" }

    let (:task) do
      Task.create(
        name: "task",
        cluster: cluster,
        prefetch: "{\"bar\": \"baz\"}",
        code: "foo",
        update_task: false,
        stored_task: false,
        sandbox_type: "sandbox",
        analyst: analyst
      )
    end

    describe "POST /api/tasks/:id/execute_as_batch_task" do
      it "should execute the task" do
        t = double
        Task.should_receive(:find).and_return(t)
        t.should_receive(:execute_batch_task)
        post "/api/tasks/#{task.id}/execute_as_batch_task"
      end
    end
  end
end
