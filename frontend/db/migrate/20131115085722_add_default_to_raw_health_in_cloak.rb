class AddDefaultToRawHealthInCloak < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        change_column :cloaks, :raw_health, :integer, :default => 4
      end
      dir.down do
        change_column :cloaks, :raw_health, :integer, :default => nil
      end
    end
  end
end
