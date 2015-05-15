class RemoveJoinersLeavers < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        ActiveRecord::Base.connection.execute("DELETE FROM buckets")

        Result.delete_all

        remove_column :buckets, :joiners
        remove_column :buckets, :leavers
      end
      dir.down do
        add_column :buckets, :joiners, :integer
        add_column :buckets, :leavers, :integer
      end
    end
  end
end
