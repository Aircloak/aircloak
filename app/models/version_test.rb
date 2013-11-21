class VersionTest < ActiveRecord::Base
  belongs_to :build
  belongs_to :cluster
  belongs_to :deployable_entity_version

  before_create :setup_from_commit

  def self.new_from_deployable_entity_version version
    VersionTest.create deployable_entity_version_id: version.id
  end

private
  def setup_from_commit
    self.test_complete = false
    true
  end
end
