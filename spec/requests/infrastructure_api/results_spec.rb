require 'spec_helper'
require './lib/proto/air/aggregate_results.pb'
require 'json'

describe InfrastructureApi::ResultsController do
  setup :activate_authlogic

  before(:each) do
    Task.destroy_all
    Analyst.destroy_all
    User.destroy_all
  end

  let (:analyst) { Analyst.create name: "TestAnalyst" }
  let (:user) do
    User.create(
      password: "password",
      password_confirmation: "password",
      login: "test-user",
      email: "test@example.com",
      analyst: analyst
    )
  end

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
      PendingResult.should_receive(:where).and_return([pr])
      pr.should_receive(:task_id).and_return(t.id)
      pr.should_receive(:task_id).and_return(t.id)
      pr.should_receive(:standing).and_return(false)
      pr.should_receive(:standing).and_return(false)
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
      rp = ResultPB.new(
        analyst_id: user.analyst.id, task_id: Task.encode_id(t.id), index: "index", buckets: buckets
      )

      with_user user do
        post "/infrastructure-api/results", rp.encode.buf, { 'Content-Type' => "application/x-protobuf" }
        json = rp.to_json
        # the fields in the json format have different names compared with the ones in the protobuffs format
        # we need to manually adjust the generated json field names so they will be accepted by the endpoint
        json.gsub! '"accumulated_count"', '"count"'
        json.gsub! '"string"', '"value"'
        post "/infrastructure-api/results", json, { 'Content-Type' => "application/json" }
      end

      Result.count.should eq(2)
      Bucket.count.should eq(4)
      Bucket.all.map(&:str_answer).sort.should eq(["Chrome", "Chrome", "Safari", "Safari"])
      Bucket.all.map(&:accumulated_count).should eq([2,30,2,30])

      response.status.should be(200)
    end
  end
end
