require 'spec_helper'
require './lib/proto/air/build_messages.pb.rb'
require './lib/build_manager'
require './lib/protobuf_sender'

describe BuildProgressController do
  before(:each) do
    BuildManager.stub(:send_build_request).and_return(true)
    ProtobufSender.stub(:send_delete)
    Cluster.destroy_all
    Build.destroy_all
    ProtobufSender.stub(:send_delete)
  end

  let (:de) { double(:deployable_entity) }
  let (:dev) { double(:deployment_entity_version, deployable_entity: de, build_success: nil, save: true) }

  describe "POST /infrastructure-api/register_build_progress" do
    it "should fail gracefully for missing builds" do
      build_id = 123
      Build.should_receive(:find).with(build_id) { raise ActiveRecord::RecordNotFound.new }
      brp = BuildResponseProto.new(
        build_id: build_id,
        status: BuildResponseProto::Status::OK,
      )
      post '/infrastructure-api/register_build_progress', brp.encode.buf
      response.status.should be(200)
    end

    it "should mark builds as completed and set success" do
      build_id = 123
      build = double
      build.should_receive(:mark_complete).with(success: true)
      Build.should_receive(:find).with(build_id).and_return build
      brp = BuildResponseProto.new(
        build_id: build_id,
        status: BuildResponseProto::Status::OK,
      )
      post '/infrastructure-api/register_build_progress', brp.encode.buf
      response.status.should be(200)
    end

    it "should mark builds as completed and set failure" do
      build_id = 123
      build = double
      build.should_receive(:mark_complete).with(success: false)
      Build.should_receive(:find).with(build_id).and_return build
      brp = BuildResponseProto.new(
        build_id: build_id,
        status: BuildResponseProto::Status::ERROR,
      )
      post '/infrastructure-api/register_build_progress', brp.encode.buf
      response.status.should be(200)
    end
  end

  describe "POST /infrastructure-api/register_version_progress" do
    it "should fail gracefully for missing versions" do
      DeployableEntityVersion.should_receive(:find_by_commit_id).with("commit_id").and_return(nil)
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::OK,
        environment: "standard",
        log_output: "all iz well"
      )
      post '/infrastructure-api/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end

    it "should persist the log" do
      DeployableEntityVersion.should_receive(:find_by_commit_id).with("commit_id").and_return(dev)

      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::OK,
        environment: "standard",
        log_output: "all iz well"
      )
      expect(dev).to receive(:build_log=).with("all iz well")
      expect(dev).to receive(:build_completed=).with(true)
      expect(dev).to receive(:build_success=).with(true)

      post '/infrastructure-api/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end
  end
end
