class DeployableEntityVersion < ActiveRecord::Base
  # This before_destroy callback needs to be called
  # before the object is destroyed. The object
  # might get destroyed through a :dependent => :destroy
  # from the DeployableEntity. It should not be allowed to
  # finish the destroy if there is a build
  # relying on this version.
  # Since the :dependent => :destroy mechanism itself
  # relies on callbacks we need to ensure our callback
  # is defined before the other callbacks, in
  # order for it to be executed first.
  before_destroy :can_destroy?

  belongs_to :deployable_entity
  before_save :set_message_and_author
  after_create :schedule_test
  has_many :build_versions
  has_many :builds, through: :build_versions
  
  # version_test is only set for commits that have an attest
  # run on them.
  has_one :version_test

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

  def can_destroy?
    unless builds.empty?
      self.errors.add(:build, "cannot delete a deployable entity version that is part of a build")
      false
    else
      true
    end
  end

private
  def set_message_and_author
    return if self.message and self.author
    Gh.add_message_and_author self
  end

  def schedule_test
    VersionTest.new_from_deployable_entity_version self
  end
end
