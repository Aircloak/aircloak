class Build < ActiveRecord::Base
  validates_presence_of :name
  validates_uniqueness_of :name

  has_many :build_versions
  has_many :deployable_entity_versions, through: :build_versions

  # A build has associated with it a set of deployable entity versions.
  # In the form for creating builds we want to be able to remember which
  # deployable entity versions that are part of a build.
  def version_for_entity entity
    vid = nil
    deployable_entity_versions.each do |v|
      vid = v.id if v.deployable_entity_id == entity.id
    end
    return vid || 0
  end
end
