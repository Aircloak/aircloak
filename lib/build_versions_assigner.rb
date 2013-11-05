class BuildVersionsAssigner
  def self.assign_versions build, versions
    versions.each do |version|
      BuildVersion.create({
        deployable_entity_version_id: version,
        build_id: build.id
      })
    end
  end

  def self.assign_from_params build, params
    wanted_versions = params["build_versions"].map(&:to_i)
    existing_versions = build.deployable_entity_versions.map(&:id)
    (existing_versions - wanted_versions).each do |id_to_destroy|
      BuildVersion.destroy(id_to_destroy)
    end
    assign_versions build, (wanted_versions - existing_versions)
  end
end
