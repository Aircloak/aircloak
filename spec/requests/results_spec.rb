require 'spec_helper'
require './lib/proto/air/aggregate_results.pb'

describe "ResultsController" do
  describe "POST /results" do
    it "should persist new properties upon receiving them from the cloak" do
      Task.destroy_all
      t = Task.new
      t.stub(:upload_stored_task)
      t.save validate: false

      # We need a valid pending result in order to get through
      # the security checks of the controller
      pr = double("pending result")
      PendingResult.should_receive(:where).and_return([pr])
      pr.should_receive(:task_id).and_return(t.id)
      pr.stub(:destroy)

      # We remove existing properties, so we know
      # what to expect after the test.
      Result.destroy_all
      Result.count.should eq(0)
      buckets = [
        BucketPB.new(label: "installed_apps",
                     string: "Chrome",
                     accumulated_count: 2),
        BucketPB.new(label: "installed_apps",
                     string: "Safari",
                     accumulated_count: 30)
      ]
      rp = ResultPB.new(analyst_id: 1, task_id: t.id, index: "index", buckets: buckets, result_id: 12)

      post "/results", rp.encode.buf

      Result.count.should eq(1)
      Bucket.count.should eq(2)
      Bucket.all.map(&:str_answer).sort.should eq(["Chrome", "Safari"])
      Bucket.all.map(&:accumulated_count).should eq([2,30])

      response.status.should be(200)
    end
  end
end
