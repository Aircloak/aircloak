require './lib/gh.rb'

class DeployableEntity < ActiveRecord::Base
  validates_uniqueness_of :repo
  validates_presence_of :tpm_env, :no_tpm_env
  validate :repo_exists
  before_save :set_description

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
