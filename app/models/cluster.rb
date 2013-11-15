class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  belongs_to :build

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build
  validate :must_match_tpm_configuration

  def tpm
    @has_tpm ||= build.tpm
  end

  def health_of_cloaks
    res = Cloak.health_types.inject(Hash.new) {|r, type| r[type] = 0; r }
    cloaks.inject(res) {|res, cloak| res[cloak.health] += 1; res }
  end

  def available_cloaks
    unassigned = Cloak.all_unassigned
    available_cloaks = (cloaks + unassigned).sort { |a, b| a.name <=> b.name }
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
