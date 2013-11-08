class Cluster < ActiveRecord::Base
  has_many :cloaks
  belongs_to :build

  validates :name, presence: true, uniqueness: true
  validates :build_id, presence: true
  validate :matching_tpm_for_cloaks_and_build

  def matching_tpm_for_cloaks_and_build
    cloaks.each do |cloak|
      self.errors.add :cloaks, "must match tpm configuration" unless cloak.tpm == self.tpm
    end
    unless self.build == nil || self.build.tpm == self.tpm
      self.errors.add :build, "must match tpm configuration"
    end
  end
end
