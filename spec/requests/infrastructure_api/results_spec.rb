require 'spec_helper'
require 'json'

describe InfrastructureApi::ResultsController do
  setup :activate_authlogic

  before(:each) do
    Task.destroy_all
    Analyst.destroy_all
    User.destroy_all
  end

  let (:user) { create_user }

  describe "POST /infrastructure-api/results" do
    it "should persist new properties upon receiving them from the cloak" do
      t = user.analyst.tasks.new
      t.stub(:upload_stored_task)
      t.analyst = user.analyst
      t.save validate: false

      # We need a valid pending result in order to get through
      # the security checks of the controller
      pr = double("pending result")
      PendingResult.should_receive(:where).and_return([pr])
      pr.should_receive(:task_id).and_return(t.id)
      pr.should_receive(:standing).and_return(false)
      pr.stub(:destroy)

      # We remove existing properties, so we know
      # what to expect after the test.
      Result.destroy_all
      Result.count.should eq(0)

      results = {
        analyst_id: user.analyst_id,
        task_id: t.encode_token,
        buckets: [
          {
            label: "installed_apps",
            value: "Chrome",
            count: 2
          },
          {
            label: "installed_apps",
            value: "Safari",
            count: 30
          }
        ]
      }

      post "/infrastructure-api/results", results.to_json, { 'Content-Type' => "application/json" }

      Result.count.should eq(1)
      Result.first.buckets.length.should eq(2)
      Result.first.buckets.map{|bucket| bucket["value"]}.sort.should eq(["Chrome", "Safari"])
      Result.first.buckets.map{|bucket| bucket["count"]}.should eq([2,30])

      response.status.should be(200)
    end
  end
end
