require './lib/gh.rb'

class DeployableEntity < ActiveRecord::Base
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

  has_many :deployable_entity_versions, dependent: :destroy

  validates_uniqueness_of :repo
  validates_presence_of :tpm_env, :no_tpm_env
  validate :repo_exists
  before_save :set_description

  alias_method :commits, :deployable_entity_versions

  def add_commit commit_id
    self.deployable_entity_versions.create commit_id: commit_id
  end

  # Produces the title that is used in the builds
  # creation form
  def select_tag_titles
    deployable_entity_versions.map do |version|
      short_commit_msg = version.message[0..80]
      short_commit_msg += "..." if version.message.length > 80
      ["#{version.short_commit_id} - #{short_commit_msg}", version.id]
    end.reverse
  end

  def status
    most_recent = deployable_entity_versions.last
    return "" unless most_recent
    most_recent.status
  end

  def can_destroy?
    deployable_entity_versions.each do |version|
      unless version.can_destroy?
        self.errors.add(:deployable_entity_version, 
            "cannot delete a deployable entity with version that are part of a build")
        return false
      end
    end
    true
  end

private
  def set_description
    self.description = Gh.description_for repo
  end

  def repo_exists
    Gh.description_for repo
  rescue UnknownRepository
    errors.add(:repo, "does not exist")
  end
end
