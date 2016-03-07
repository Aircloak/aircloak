class AddTpmToCloak < ActiveRecord::Migration
  def change
    add_column :cloaks, :tpm, :boolean, default: true
  end
end
