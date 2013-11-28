require 'spec_helper'
require './lib/proto/air/query.pb'
require './lib/proto/air/query_upload.pb'

describe "TasksController" do
  describe "POST /tasks/update_task_binary" do
    before(:each) do
      Query.destroy_all
      Task.destroy_all
    end

    it "should create a new task binary upon upload if there isn't already one with the same main package" do
      bin = QueryBinary.new(package: "task", data: "1234")
      data = QueryData.new(main_package: "task", data: [bin])

      expect {
        post "/tasks/update_task_binary", data.encode.buf
      }.to change {Task.count}.from(0).to(1)

      response.status.should be(200)

      Task.all.map(&:ready).should eq([false])
    end

    it "should update a task binary upon upload" do
      bin1 = QueryBinary.new(package: "task", data: "1234")
      data1 = QueryData.new(main_package: "task", data: [bin1])
      task = Task.new(main_package: "task", packaged_data: data1.encode.buf, ready: true)
      task.save.should eq true

      bin2 = QueryBinary.new(package: "task", data: "5678")
      data2 = QueryData.new(main_package: "task", data: [bin2])

      post "/tasks/update_task_binary", data2.encode.buf

      response.status.should be(200)
      task.reload.ready.should eq true

      QueryData.decode(task.packaged_data.dup).data.should eq [bin2]
    end
  end
end
