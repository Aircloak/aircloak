class AddRawHealthToCloak < ActiveRecord::Migration
  def change
    add_column :cloaks, :raw_health, :integer
  end
end
