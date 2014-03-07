class AddModifyTimestampToClusters < ActiveRecord::Migration
  def change
    add_column :clusters, :last_modified, :datetime

    reversible do |dir|
      dir.up do
        Cluster.all.each do |cluster|
          cluster.last_modified = cluster.updated_at
          cluster.save
        end
      end
    end
  end
end
