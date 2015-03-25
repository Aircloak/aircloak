require 'spec_helper'
require './lib/build_manager'

describe "Api::TaskResultsController" do
  before(:each) do
    Task.destroy_all
    Analyst.destroy_all
    Result.destroy_all
    ClusterCloak.destroy_all
    Cluster.destroy_all
    Cloak.destroy_all
    BuildManager.stub(:send_build_request)
  end

  let! (:cloak) { Cloak.create(name: "cloak", ip: "1.1.1.1") }
  let! (:build) { Build.create(name: "build") }
  let! (:cluster) { Cluster.create(name: "cluster", build: build, cloaks: [cloak]) }

  let (:analyst) { Analyst.create name: "TestAnalyst" }
  let (:token) { AnalystToken.create_api_token(analyst) }
  let (:task) do
    t = Task.create(
      name: "task",
      cluster: cluster,
      prefetch: "{\"bar\": \"baz\"}",
      code: "foo",
      update_task: false,
      stored_task: false,
      sandbox_type: "sandbox",
      analyst: analyst
    )

    initial_date = Date.today - 100.days
    (1..100).each do |i|
      t.results.create(
            created_at: initial_date + i.days,
            buckets_json: [{label: "label_#{i}", value: "answer_#{i}", count: i}].to_json
          )
    end
    t
  end

  describe "GET /api/tasks/:id/results" do
    it "retrieves results" do
      get("/api/tasks/#{task.token}/results", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token})

      response.code.should eq "200"
      json = JSON.parse(response.body)
      json["success"].should eq true
      json["count"].should eq 100
      json["page"].should eq 1
      json["per_page"].should eq 10
      verify_data((91..100).to_a.reverse, json["results"])
    end

    it "paginates" do
      get("/api/tasks/#{task.token}/results?page=50&per_page=2", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token})
      response.code.should eq "200"

      json = JSON.parse(response.body)
      json["success"].should eq true
      json["count"].should eq 100
      json["page"].should eq 50
      json["per_page"].should eq 2
      verify_data([2, 1], json["results"])
    end

    it "filters dates" do
      params = {
        from: (Date.today - 20.days).strftime("%Y%m%d %H:%M"),
        to: (Date.today - 15.days).strftime("%Y%m%d %H:%M")
      }
      get("/api/tasks/#{task.token}/results?#{params.to_query}",
          {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token})
      response.code.should eq "200"

      json = JSON.parse(response.body)
      json["success"].should eq true
      json["count"].should eq 6
      json["page"].should eq 1
      json["per_page"].should eq 10
      verify_data((80..85).to_a.reverse, json["results"])
    end

    it "should require analyst" do
      get("/api/tasks/#{task.token}/results", format: :json)
      response.code.should eq "401"

      get("/api/tasks/#{task.token}/results", {format: :json}, {'HTTP_ANALYST_TOKEN' => "foobar"})
      response.code.should eq "401"
    end

    it "should require task" do
      get("/api/tasks/foobar/results", {format: :json}, {'HTTP_ANALYST_TOKEN' => token.token})
      response.code.should eq "422"
    end
  end

  private
    def verify_data(values, items)
      items.length.should eq values.length
      values.each_with_index do |value, index|
        items[index]["buckets"][0]["label"].should eq("label_#{value}")
        items[index]["buckets"][0]["value"].should eq("answer_#{value}")
        items[index]["buckets"][0]["count"].should eq(value)
      end
    end
end
