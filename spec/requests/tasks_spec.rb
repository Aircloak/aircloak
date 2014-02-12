require 'spec_helper'
require './lib/proto/air/query.pb'
require './lib/proto/air/query_upload.pb'

describe "TasksController" do
  describe "POST /tasks/update_task_binary" do
    before(:each) do
      Query.destroy_all
      Task.destroy_all
    end

    let (:bin) { QueryBinary.new(package: "task", data: "1234") }

    def query_data params = {}
      QueryData.new(
        main_package: "task",
        data: [params.delete(:bin) || bin],
        payload_identifier: params.delete(:payload_identifier),
        mutator: params.delete(:mutator),
        system_task: params.delete(:system_task)
      )
    end

    def post_and_validate params
      expect {
        post "/tasks/update_task_binary", (query_data params).encode.buf
      }.to change {Task.count}.from(0).to(1)
      response.status.should be(200)
    end

    it "should create a new task binary upon upload if there isn't already one with the same main package" do
      post_and_validate({})
      Task.all.map(&:ready).should eq([false])
    end

    it "should update a task binary upon upload" do
      task = Task.new(main_package: "task", packaged_data: (query_data {}).encode.buf, ready: true)
      task.save.should eq true

      bin2 = QueryBinary.new(package: "task", data: "5678")
      post "/tasks/update_task_binary", query_data(bin: bin2).encode.buf
      response.status.should be(200)

      task.reload.ready.should eq true
      QueryData.decode(task.packaged_data.dup).data.should eq [bin2]
    end

    it "should create/update a new task binary upon upload and set it ready if enough information" do
      post_and_validate(system_task: true, mutator: false)
      Task.all.map(&:ready).should eq([true])
    end

    it "should create/update a new task to be an update task if a payload identifier is given" do
      post_and_validate(payload_identifier: "foo")
      Task.all.map(&:update_task).should eq([true])
    end

    it "should create/update a new task to be no update task if no payload identifier is given" do
      post_and_validate({})
      Task.all.map(&:update_task).should eq([false])
    end

    it "should create/update a new task that is not ready if system_task is not set" do
      post_and_validate(mutator: false)
      Task.all.map(&:ready).should eq([false])
    end

    it "should create/update a new task that is not ready if mutator is not set" do
      post_and_validate(system_task: false)
      Task.all.map(&:ready).should eq([false])
    end
  end
end
