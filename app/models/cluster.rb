class Cluster < ActiveRecord::Base
  has_many :cloaks
  has_one :build

  validates_associated :cloaks
  validates :name, presence: true, uniqueness: true
  validates_each :cloak do |record, attr, value|
    record.error.add(attr, 'must match tpm configuration') unless value.tpm == record.tpm
  end
  validates_each :build do |record, attr, value|
    record.error.add(attr, 'must match tpm configuration') unless value.tpm == record.tpm
  end
end
