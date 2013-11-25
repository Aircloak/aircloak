class Result < ActiveRecord::Base
  belongs_to :query
  has_many :buckets, dependent: :destroy
end
