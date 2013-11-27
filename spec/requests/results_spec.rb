require 'spec_helper'
require './lib/proto/air/aggregate_results.pb'

describe "ResultsController" do
  describe "POST /results" do
    it "should persist new properties upon receiving them from the cloak" do
      Query.destroy_all
      q = Query.new
      q.save validate: false

      # We need a valid pending result in order to get through
      # the security checks of the controller
      pr = double("pending result")
      PendingResult.should_receive(:where).and_return([pr])
      pr.should_receive(:query_id).and_return(q.id)
      pr.stub(:destroy)

      # We remove existing properties, so we know
      # what to expect after the test.
      Property.destroy_all
      Property.count.should eq(0)
      props = [
        PropertyProto.new(label: "installed_apps",
                          string: "Chrome",
                          joiners_leavers: JoinersLeaversProto.new(joiners: 2, leavers:0)),
        PropertyProto.new(label: "installed_apps",
                          string: "Safari",
                          joiners_leavers: JoinersLeaversProto.new(joiners: 1, leavers:0))
      ]
      rp = ResultProto.new(analyst_id: "analyst", task_id: q.id, index: "index", properties: props, result_id: 1)

      post "/results", rp.encode.buf

      Property.count.should eq(1)
      PropertyResult.count.should eq(2)
      PropertyResult.all.map(&:str_value).sort.should eq(["Chrome", "Safari"])
      PropertyResult.all.map(&:has_range).should eq([false, false])
      PropertyResultCount.count.should eq(2)
      PropertyResultCount.all.map(&:joiners).sort.should eq([1,2])
      PropertyResultCount.all.map(&:leavers).should eq([0,0])

      response.status.should be(200)
    end
  end
end
