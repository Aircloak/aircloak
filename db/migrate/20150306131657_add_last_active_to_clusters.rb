class AddLastActiveToClusters < ActiveRecord::Migration
  def change
    add_column :clusters, :last_active, :datetime, :default => Time.now
  end
end
