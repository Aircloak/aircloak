class Cluster < ActiveRecord::Base
  has_many :cluster_cloaks
  has_many :cloaks, through: :cluster_cloaks
  belongs_to :build

  validates :name, presence: true, uniqueness: true
  validates_presence_of :build_id
  validate :matching_tpm_for_cloaks_and_build

  def matching_tpm_for_cloaks_and_build
    self.errors.add :cloaks, "must match tpm configuration" unless self.check_cloaks cloaks
    self.errors.add :build, "must match tpm configuration" if self.build && self.build.tpm != self.tpm
  end

  def check_cloaks cloak_list
    res = true
    cloak_list.each do |cloak|
      res = res && (cloak.tpm == self.tpm)
    end
    res
  end

  def health_of_cloaks
    res = {:ok => 0, :changing => 0, :down => 0}
    cloaks.each do |cloak|
      health = cloak.health
      res[health] += 1
    end
    res
  end

  def available_cloaks
    unassigned = Cloak.includes(:cluster_cloak).where(cluster_cloaks: { id: nil })
    (cloaks.all + unassigned.all).sort.map do |cloak|
      if cloak.tpm
        [cloak.name, cloak.id]
      else
        [cloak.name + " (no tpm)", cloak.id]
      end
    end
  end

  def selected_cloaks
    cloaks.all.map { |cloak| cloak.id }
  end
end
