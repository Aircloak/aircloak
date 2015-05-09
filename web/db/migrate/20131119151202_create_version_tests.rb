class CreateVersionTests < ActiveRecord::Migration
  def change
    create_table :version_tests do |t|
      t.references :build, index: true
      t.references :cluster, index: true
      t.references :deployable_entity_version, index: true
      t.boolean :test_success
      t.string :test_output
      t.boolean :test_complete

      t.timestamps
    end
  end
end
