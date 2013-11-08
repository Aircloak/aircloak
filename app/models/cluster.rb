class Cluster < ActiveRecord::Base
  has_many :cloaks
  belongs_to :build

  validates_associated :cloaks
  validates :name, presence: true, uniqueness: true
  validates_each :cloaks do |record, attr, value|
    record.error.add(attr, 'must match tpm configuration') unless value == [] || value.tpm == record.tpm
  end
  validates_each :build_id do |record, attr, value|
    record.error.add(attr, 'must match tpm configuration') unless Build.find(value).tpm == record.tpm
  end
end
