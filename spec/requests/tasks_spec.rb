require 'spec_helper'
require './lib/proto/air/query.pb'
require './lib/proto/air/query_upload.pb'

describe "TasksController" do
  describe "POST /tasks/update_task_binary" do
    before(:each) do
      Query.destroy_all
      Task.destroy_all
    end

    it "should create a new task binary upon upload" do
      bin = QueryBinary.new(package: "task", data: "1234")
      data = QueryData.new(main_package: "task", data: [bin])

      Task.count.should eq(0)

      post "/tasks/update_task_binary", data.encode.buf

      response.status.should be(200)

      Task.count.should eq(1)
      Task.all.map(&:ready).should eq([false])
    end

    it "should update a task binary upon upload" do
      bin1 = QueryBinary.new(package: "task", data: "1234")
      data1 = QueryData.new(main_package: "task", data: [bin1])

      Task.count.should eq(0)

      post "/tasks/update_task_binary", data1.encode.buf

      response.status.should be(200)

      Task.count.should eq(1)
      Task.all.map(&:ready).should eq([false])

      task = Task.first
      task.ready = true
      task.save.should eq true

      Task.count.should eq(1)
      Task.all.map(&:ready).should eq([true])

      bin2 = QueryBinary.new(package: "task", data: "5678")
      data2 = QueryData.new(main_package: "task", data: [bin2])

      post "/tasks/update_task_binary", data2.encode.buf

      response.status.should be(200)
      task.reload.ready.should eq true

      QueryData.decode(task.packaged_data.dup).data.should eq [bin2]
    end
  end
end
