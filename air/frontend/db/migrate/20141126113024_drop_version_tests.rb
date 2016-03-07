class DropVersionTests < ActiveRecord::Migration
  def change
    drop_table :version_tests
  end
end
