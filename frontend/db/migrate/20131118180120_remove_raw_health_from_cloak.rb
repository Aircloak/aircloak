class RemoveRawHealthFromCloak < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        remove_column :cloaks, :raw_health
        add_column :cloaks, :good, :boolean, :default => true
      end
      dir.down do
        remove_column :cloaks, :good
        add_column :cloaks, :raw_health, :integer, :default => 4
      end
    end
  end
end
