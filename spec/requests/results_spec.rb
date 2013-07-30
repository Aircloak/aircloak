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
        PropertyProto.new(name: "installed_apps",
                          str_answer: "Chrome",
                          count: 2),
        PropertyProto.new(name: "installed_apps",
                          str_answer: "Safari",
                          count: 1)
      ]
      rp = ResultProto.new(query_id: q.id, properties: props)

      post "/results", rp.encode.buf

      Property.count.should eq(1)
      PropertyResult.count.should eq(2)
      PropertyResult.all.map(&:str_value).sort.should eq(["Chrome", "Safari"])
      PropertyResult.all.map(&:numeric).should eq([false, false])
      PropertyResultCount.count.should eq(2)
      PropertyResultCount.all.map(&:count).sort.should eq([1,2])

      response.status.should be(200)
    end
  end
end
