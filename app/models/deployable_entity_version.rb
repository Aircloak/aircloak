class DeployableEntityVersion < ActiveRecord::Base
  belongs_to :deployable_entity
  before_save :set_message_and_author
  has_many :build_versions
  has_many :builds, through: :build_versions

  validates_presence_of :deployable_entity_id
  validates_uniqueness_of :commit_id

  def part_of_build?
    builds.count != 0
  end

  def short_commit_id
    commit_id[0...10]
  end

private
  def set_message_and_author
    Gh.add_message_and_author self
  end
end
