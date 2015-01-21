require 'spec_helper'

describe "ApiRepeatedAnswersController" do
  before(:each) do
    Analyst.destroy_all
    Analyst.create(name: "test")
  end

  describe "POST /api/repeated_answers" do
    before(:each) do
      RepeatedAnswer.delete_all
      RepeatedAnswerTaskCode.delete_all
      RepeatedAnswerLibraryCode.delete_all
    end

    let (:request) {
      {
        analyst: Analyst.where(name: "test").first.id,
        bucket_label: "foo",
        bucket_value: "bar",
        bucket_count: 1234567,
        timestamp: 1234,
        source_ip: "1.1.1.1",
        noise_sd: 1.234,
        task_codes: [
          {
            prefetch: "foo",
            code: "bar"
          },
          {
            prefetch: "xxx",
            code: "yyy"
          }
        ]
      }
    }

    it "should take a valid request" do
      post "/api/repeated_answers", request.to_json
      response.status.should eq(200)
    end

    it "requires a JSON" do
      post "/api/repeated_answers", "foo"
      response.status.should eq(403)
    end

    it "requires .analyst" do
      request.delete(:analyst)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .bucket_label" do
      request.delete(:bucket_label)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .bucket_count" do
      request.delete(:bucket_count)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .timestamp" do
      request.delete(:timestamp)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .source_ip" do
      request.delete(:source_ip)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .noise_sd" do
      request.delete(:noise_sd)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].prefetch" do
      request[:task_codes][0].delete(:prefetch)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].code" do
      request[:task_codes][0].delete(:code)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "accepts .task_codes[].libraries[] with name and code" do
      request[:task_codes][0]["libraries"] = [{ :name => "foo", :code => "bar" }]
      post "/api/repeated_answers", request.to_json
      response.status.should eq(200)
    end

    it "requires .task_codes[].libraries[].name" do
      request[:task_codes][0]["libraries"] = [{ :code => "bar" }]
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].libraries[].code" do
      request[:task_codes][0]["libraries"] = [{ :name => "foo" }]
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end
  end
end
