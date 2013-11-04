require './lib/gh.rb'

class DeployableEntity < ActiveRecord::Base
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
      ["#{version.short_commit_id} - #{version.message[0..10]}", version.id]
    end
  end

  def status
    most_recent = deployable_entity_versions.last
    return "" unless most_recent
    most_recent.status
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
