require 'spec_helper'
require 'json'

describe InfrastructureApi::AuditLogsController do
  before(:each) do
    AuditLog.delete_all
    Cloak.delete_all
  end

  let!(:cloak) { Cloak.create name: "TestCloak", ip: "127.0.1.2" }
  let(:payload) {  }

  def make_post vals = {}
    default_payload = ";06FFAC01D2125165F300F760FEB4C7AB460D88A6C0152C6E4601B639494941A0;146;hello;2014-12-19T15:40:05Z;worldsssss"
    headers = {'Content-Type' => vals[:content_type] || "text/plain"}
    unless vals[:skip_path_header]
      cloak_id = vals[:dont_use_valid_cloak] ? "invalid_name" : cloak.name
      headers['PATH'] = Base64.encode64("/audit_log/#{cloak_id}")
    end
    post("/infrastructure-api/audit_logs", vals[:payload] || default_payload, headers)
  end

  describe "POST /infrastructure-api/audit_logs" do
    it "should reject updates where the path header is not set" do
      make_post skip_path_header: true
      AuditLog.count.should eq 0
      response.status.should be(422)
    end

    it "should reject updates for cloaks that do not exist" do
      make_post dont_use_valid_cloak: true
      AuditLog.count.should eq 0
      response.status.should be(404)
    end

    it "should reject updates that are not plain text" do
      make_post content_type: "monkey/business"
      AuditLog.count.should eq 0
      response.status.should be(501)
    end

    it "should reject invalid log messages" do
      make_post payload: "format-does-not-compute"
      AuditLog.count.should eq 0
      response.status.should be(500)
    end

    it "should persist valid log messages" do
      make_post
      AuditLog.count.should eq 1
      response.status.should be(200)
    end
  end
end
