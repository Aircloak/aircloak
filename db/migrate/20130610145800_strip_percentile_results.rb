class StripPercentileResults < ActiveRecord::Migration
  def change
    reversible do |dir|
      change_table :percentile_results do |t|
        dir.up { 
          add_reference :percentile_results, :percentile
          PercentileResult.all.each do |pr|
            p = Percentile.where(bucket: pr.bucket, query_id: query_id).first
            pr.percentile = p
            pr.save
          end
          remove_column :percentile_results, :bucket 
          remove_reference :percentile_results, :query
        }
        dir.down   { 
          add_column :percentile_results, :bucket, :string
          add_reference :percentile_results, :query
          PercentileResult.all.each do |pr|
            pr.bucket = pr.percentile.bucket
            pr.query_id = pr.query_id
            pr.save
          end
          remove_reference :percentile_results, :property
        }
      end
    end
  end
end
