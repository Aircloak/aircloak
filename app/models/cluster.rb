class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  belongs_to :build

  # A cluster that is used for an automated test of a commit will have a
  # version_test instance. For all other clusters this will be nil
  has_one :version_test

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build
  validate :must_match_tpm_configuration

  def tpm
    @has_tpm ||= build.tpm
  end

  def num_broken
    cloaks.inject(0) {|res, cloak| cloak.good ? res : res + 1 }
  end

  def health
    num_broken > 0 ? :poor : :healthy
  end

  def available_cloaks
    global_available = Cloak.all_available
    available_cloaks = (cloaks + global_available).sort { |a, b| a.name <=> b.name }
  end

  def selected_cloaks
    cloaks.map(&:id)
  end

  def assign_cloaks new_cloaks
    old_cloaks = self.cloaks.to_a
    (new_cloaks - old_cloaks).each {|cloak| cloaks << cloak }
    (old_cloaks - new_cloaks).each {|cloak| cloak.cluster_cloak.destroy }
  end

private

  def must_match_tpm_configuration
    unless cloaks.inject(true) {|is_ok, cloak| is_ok && cloak.tpm == self.tpm }
      self.errors.add :cloaks, "must match tpm configuration"
    end
  end
end
