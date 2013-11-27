require 'vcr'

VCR.configure do |c|
  c.cassette_library_dir = 'spec/fixtures/vcr_cassettes'
  c.hook_into :excon
end

module PreRecorded
  def self.setup_deployable_entity
    de = nil
    VCR.use_cassette('premade-entity-create-erlattest', allow_playback_repeats: true) do
      de = DeployableEntity.create(
        repo: "erlattest",
        description: "erlattest fixture",
        tpm_env: "tpm",
        no_tpm_env: "no-tpm"
      )
    end
    de
  end

  def self.setup_deployable_entity_version de
    commit = "4023b4d576873e7bf3e2d5a6d891b982fd14f36b"
    dev = DeployableEntityVersion.new commit_id: commit, deployable_entity_id: de.id
    VCR.use_cassette('premade-create-deployable-entity-version') do
      dev.save
    end
    dev
  end
end
