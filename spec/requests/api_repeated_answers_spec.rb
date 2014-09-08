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
    end

    let (:request) {
      {
        analyst: Analyst.where(name: "test").first.id,
        bucket_label: "foo",
        bucket_value: "bar",
        bucket_count: 1234567,
        timestamp: 1234,
        source_ip: "1.1.1.1",
        anonymization_parameters: {
          k1: 1,
          k2: 2,
          target_error: 0.0001,
          sigma: 423,
          constant_noise_sd: 2323
        },
        task_codes: ["foo", "bar", "baz"]
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

    it "requires .anonymization_parameters.k1" do
      request[:anonymization_parameters].delete(:k1)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .anonymization_parameters.k2" do
      request[:anonymization_parameters].delete(:k2)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .anonymization_parameters.target_error" do
      request[:anonymization_parameters].delete(:target_error)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .anonymization_parameters.sigma" do
      request[:anonymization_parameters].delete(:sigma)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end

    it "requires .anonymization_parameters.constant_noise_sd" do
      request[:anonymization_parameters].delete(:constant_noise_sd)
      post "/api/repeated_answers", request.to_json
      response.status.should eq(403)
    end
  end
end
