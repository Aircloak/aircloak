class CreatePercentileResults < ActiveRecord::Migration
  def change
    # execute "CREATE EXTENSION IF NOT EXISTS hstore WITH SCHEMA public;"
    create_table :percentile_results do |t|
      t.string :bucket
      t.references :query, index: true
      t.hstore :raw_values

      t.timestamps
    end
    add_index :percentile_results, :bucket
  end
end
