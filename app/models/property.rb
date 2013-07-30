class Property < ActiveRecord::Base
  belongs_to :query
  has_many :property_results, dependent: :destroy
end
