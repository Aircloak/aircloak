class CreatePercentiles < ActiveRecord::Migration
  def change
    create_table :percentiles do |t|
      t.string :bucket
      t.references :query, index: true

      t.timestamps
    end

    reversible do |dir|
      dir.up do
        PercentileResult.all.each do |pr|
          p = Percentile.where(bucket: pr.bucket).first
          Percentile.create(bucket: pr.bucket, query_id: pr.query_id) unless p
        end
      end
      dir.down do
        Percentile.destroy_all
      end
    end
  end
end
