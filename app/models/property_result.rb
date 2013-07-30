class PropertyResult < ActiveRecord::Base
  belongs_to :property
  has_one :query, through: :property
  has_many :property_result_counts, dependent: :destroy
end
