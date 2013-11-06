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

  def status
    return "" if builds.count == 0
    return "Building" unless build_completed
    return "Built" if build_completed && build_success
    return "Failed" if build_completed && ! build_success
  end

private
  def set_message_and_author
    return if self.message and self.author
    Gh.add_message_and_author self
  end
end
