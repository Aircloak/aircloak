require 'spec_helper'
require './lib/proto/air/build_messages.pb.rb'
require './lib/build_manager'

describe BuildProgressController do
  before(:each) do
    BuildManager.stub(:send_build_request).and_return(true)
    Cluster.destroy_all
    Build.destroy_all
  end

  let(:de) { double(:deployable_entity, tpm_env: "tpm-env", no_tpm_env: "no-tpm-env") }
  let(:dev) { double(:deployment_entity_version, deployable_entity: de, build_success: nil, save: true) }

  describe "POST /register_build_progress" do
    it "should fail gracefully for missing builds" do
      build_id = 123
      Build.should_receive(:find).with(build_id) { raise ActiveRecord::RecordNotFound.new }
      brp = BuildResponseProto.new(
        build_id: build_id,
        status: BuildResponseProto::Status::OK,
      )
      post '/register_build_progress', brp.encode.buf
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
      post '/register_build_progress', brp.encode.buf
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
      post '/register_build_progress', brp.encode.buf
      response.status.should be(200)
    end
  end

  describe "POST /register_version_progress" do
    it "should fail gracefully for missing versions" do
      DeployableEntityVersion.should_receive(:find_by_commit_id).with("commit_id").and_return(nil)
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::OK,
        environment: "tpm",
        log_output: "all iz well"
      )
      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end

    it "should persist the log" do
      DeployableEntityVersion.should_receive(:find_by_commit_id).twice.with("commit_id").and_return(dev)

      # For TPM env
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::OK,
        environment: "tpm-env",
        log_output: "all iz well"
      )
      expect(dev).to receive(:build_log_tpm=).with("all iz well")
      expect(dev).to receive(:build_completed=).with(true)
      expect(dev).to receive(:build_success=).with(true)

      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
      
      # For non TPM env
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::ERROR,
        environment: "no-tpm-env",
        log_output: "all iz partly well"
      )
      expect(dev).to receive(:build_log_no_tpm=).with("all iz partly well")
      expect(dev).to receive(:build_completed=).with(true)
      expect(dev).to receive(:build_success=).with(false)

      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end

    it "should treat shared logs as logs for all environments" do
      DeployableEntityVersion.should_receive(:find_by_commit_id).with("commit_id").and_return(dev)

      # For TPM env
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::ERROR,
        environment: "shared",
        log_output: "all iz not well"
      )
      expect(dev).to receive(:build_log_tpm=).with("all iz not well")
      expect(dev).to receive(:build_log_no_tpm=).with("all iz not well")
      expect(dev).to receive(:build_completed=).with(true)
      expect(dev).to receive(:build_success=).with(false)

      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end

    it "negativity trumps everything!" do
      # If one build fails, and then another succeeds, then the build has still
      # failed overall...
      DeployableEntityVersion.should_receive(:find_by_commit_id).twice.with("commit_id").and_return(dev)

      # First request that fails
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::ERROR,
        environment: "tpm-env",
        log_output: "all iz well"
      )
      expect(dev).to receive(:build_log_tpm=).with("all iz well")
      expect(dev).to receive(:build_completed=).with(true)
      dev.should_receive(:build_success=).once.with(false)

      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
      
      # Second request that succeeds
      vbrp = VersionBuildResponseProto.new(
        commit_id: "commit_id",
        status: VersionBuildResponseProto::Status::OK,
        environment: "no-tpm-env",
        log_output: "all iz partly well"
      )
      expect(dev).to receive(:build_log_no_tpm=).with("all iz partly well")
      expect(dev).to receive(:build_completed=).with(true)
      dev.should_receive(:build_success).and_return(false)
      # build_sucess should not be set again, since we already set it to false

      post '/register_version_progress', vbrp.encode.buf
      response.status.should be(200)
    end
  end
end
