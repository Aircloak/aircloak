require './lib/proto/air/build_messages.pb.rb'

class BuildManager
  def self.create_build_request build
    BuildRequestProto.new(
      build_name: build.name,
      build_id: build.id,
      versions: build.deployable_entity_versions.map do |v|
        env = build.tpm ? v.deployable_entity.tpm_env : v.deployable_entity.no_tpm_env
        BuildRequestProto::VersionProto.new(
          environment: env,
          repo: v.deployable_entity.repo,
          commit_id: v.commit_id,
        )
      end
    )
  end
    
  def self.send_build_request build
    build_request = create_build_request build
    url = URI.parse("http://#{Rails.configuration.build_server.host}/build")
    https = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    request.content_type = "application/x-protobuf"
    request.body = build_request.encode.buf
    https.request(request)
  end

  def self.all_entities_except this_one
    DeployableEntity.where("id != (?)", this_one.id)
  end

  def self.find_right_versions entities
    entities.map do |entity|
      required_version = Gh.latest_commit_on_branch_for_repo "develop", entity.repo
      entity.add_commit required_version
      DeployableEntityVersion.find_by_commit_id required_version
    end
  end

  def self.test_build_for_version version
    other_entities = all_entities_except(version.deployable_entity)
    all_versions = (find_right_versions other_entities) << version
    Build.new tpm: false, name: "testbuild #{version.commit_id}", deployable_entity_versions: all_versions
  end
end
