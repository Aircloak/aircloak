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
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  describe "getting queries and results, and destroying queries" do
    before(:each) do
      cloak.cluster_cloak.set_state :belongs_to
      cloak.cluster_cloak.save.should eq true
    end

    let (:task) do
      Task.create(
        name: "task",
        cluster: cluster,
        prefetch: "{\"bar\": \"baz\"}",
        code: "foo",
        update_task: false,
        stored_task: false,
        sandbox_type: "sandbox"
      )
    end

    def result args = {}
      Result.create(
        task: args.delete(:task) || task,
        result_id: args.delete(:result_id) || 1234
      )
    end

    describe "GET /api/tasks/:id/latest_result_id" do
      it "should return an error if the task is unknown" do
        Task.count.should eq 0
        get latest_result_id_api_task_path(1)
        response.status.should eq 404
      end

      it "should return an empty results proto if there are no results" do
        t = task # Create the task
        get latest_result_id_api_task_path(t.id)
        response.status.should eq 200
        # This is a strange artifact of the protobuf library we are using.
        # Rather than return an empty list, we get nil.
        ResultsPB.decode(response.body).result_ids.should eq nil
      end

      it "should return the list with the greatest result id" do
        t = task
        r1 = result task: t, result_id: 2
        r2 = result task: t, result_id: 1
        Result.count.should eq 2
        get latest_result_id_api_task_path(t.id)
        response.status.should eq 200
        ResultsPB.decode(response.body).result_ids.should eq [r1.result_id]
      end
    end

    describe "POST /api/tasks/execute_named_batch_task/:name" do
      it "should return an error if the task is unknown" do
        Task.count.should eq 0
        post "/api/tasks/bogus_name/execute_as_batch_task"
        response.status.should eq 404
      end

      it "should execute the named batch task" do
        t = double
        Task.should_receive(:find_by_name).with("name").and_return(t)
        t.should_receive(:execute_batch_task)
        post "/api/tasks/name/execute_as_batch_task"
        response.status.should eq 200
      end
    end

    describe "GET /api/tasks/:id" do
      it "should return an error if the task is unknown" do
        Task.count.should eq 0
        get api_task_path(1)
        response.status.should eq 404
      end

      it "should return the list of known results" do
        task
        Task.count.should eq 1
        r = result
        Result.count.should eq 1
        get api_task_path(task.id)
        response.status.should eq 200
        ResultsPB.decode(response.body).result_ids.should eq [r.result_id]
      end
    end

    describe "GET /api/tasks/:id/results/:id" do
      it "should return an error if the task is unknown" do
        Task.count.should eq 0
        get "/api/tasks/1/results/1"
        response.status.should eq 404
      end

      it "should return an error if the result is unknown" do
        task
        Task.count.should eq 1
        get "/api/tasks/#{task.id}/results/1"
        response.status.should eq 404
      end

      it "should return the the known result" do
        task
        Task.count.should eq 1
        r = result
        Result.count.should eq 1
        bucket = Bucket.new(result: r, label: "label", accumulated_count: 1)
        bucket.save.should eq true
        get "/api/tasks/#{task.id}/results/#{r.result_id}"
        response.status.should eq 200
        rp = ResultPB.decode(response.body)
        rp.result_id.should eq r.result_id
        rp.buckets.size.should eq 1
        rp.buckets.first.label.should eq "label"
        rp.buckets.first.string.should eq nil
        rp.buckets.first.accumulated_count.should eq 1
      end
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
