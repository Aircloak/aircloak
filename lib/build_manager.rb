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
    url = URI.parse("http://cloakbuild.mpi-sws.org/build")
    https = Net::HTTP.new(url.host, url.port)
    request = Net::HTTP::Post.new(url.path)
    request.content_type = "application/x-protobuf"
    request.body = build_request.encode.buf
    https.request(request)
  end
end
