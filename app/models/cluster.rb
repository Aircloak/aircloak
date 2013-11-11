class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  belongs_to :build

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build_id
  validate :matching_tpm_for_cloaks_and_build

  def matching_tpm_for_cloaks_and_build
    cloaks.each do |cloak|
      self.errors.add :cloaks, "must match tpm configuration" unless cloak.tpm == self.tpm
    end
    self.errors.add :build, "must match tpm configuration" if self.build && self.build.tpm != self.tpm
  end

  def health_of_cloaks
    res = {:ok => 0, :changing => 0, :down => 0}
    cloaks.all.each do |cloak|
      health = cloak.health
      res[health] = res[health] + 1
    end
    res
  end
end
