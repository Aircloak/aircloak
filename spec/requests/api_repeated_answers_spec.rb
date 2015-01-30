require 'spec_helper'

describe "ApiRepeatedAnswersController" do
  before(:each) do
    Analyst.destroy_all
    Analyst.create(name: "test")
  end

  describe "POST /infrastructure-api/repeated_answers" do
    before(:each) do
      RepeatedAnswer.delete_all
      RaTaskCode.delete_all
      RaTaskCodeRepeatedAnswer.delete_all
      RaLibraryCode.delete_all
      RaLibraryCodeRaTaskCode.delete_all
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
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
    end

    it "requires a JSON" do
      post "/infrastructure-api/repeated_answers", "foo"
      response.status.should eq(403)
    end

    it "requires .analyst" do
      request.delete(:analyst)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .bucket_label" do
      request.delete(:bucket_label)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .bucket_count" do
      request.delete(:bucket_count)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .timestamp" do
      request.delete(:timestamp)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .source_ip" do
      request.delete(:source_ip)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .noise_sd" do
      request.delete(:noise_sd)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].prefetch" do
      request[:task_codes][0].delete(:prefetch)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].code" do
      request[:task_codes][0].delete(:code)
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "accepts .task_codes[].libraries[] with name and code" do
      request[:task_codes][0][:libraries] = [{ :name => "foo", :code => "bar" }]
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
    end

    it "requires .task_codes[].libraries[].name" do
      request[:task_codes][0][:libraries] = [{ :code => "bar" }]
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .task_codes[].libraries[].code" do
      request[:task_codes][0][:libraries] = [{ :name => "foo" }]
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "auto-resolves if all code is trusted" do
      # first generate a new repeated answer report with code inserted
      request[:task_codes][0][:libraries] = [{ :name => "foo", :code => "bar" }]
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(false) }

      # second mark all codes as trusted (should autoresolve the report)
      RaTaskCode.all.each { |task_code| task_code.mark_trustworthy }
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(true) }

      # add a new slightly different report which should be autoresolved
      request[:task_codes][0][:libraries][0][:name] = "foo2"
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(true) }
    end

    it "does not auto-resolve if one code is not trusted" do
      # first generate a new repeated answer report with code inserted
      request[:task_codes][0][:libraries] = [{ :name => "foo", :code => "bar" }]
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(false) }

      # second mark the first code as trustworthy
      RaTaskCode.first.mark_trustworthy
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(false) }

      # add a new slightly different report which should not be autoresolved
      request[:task_codes][0][:libraries][0][:name] = "foo2"
      post "/infrastructure-api/repeated_answers", request.to_json
      response.status.should eq(200)
      RepeatedAnswer.all.each { |ra| ra.resolved.should eq(false) }
    end
  end
end
