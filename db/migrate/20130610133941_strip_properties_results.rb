class StripPropertiesResults < ActiveRecord::Migration
  def change
    reversible do |dir|
      change_table :properties_results do |t|
        dir.up { 
          add_reference :properties_results, :property
          PropertiesResult.all.each do |pr|
            p = Property.where(property: pr.bucket, query_id: query_id).first
            pr.property = p
            pr.save
          end
          remove_column :properties_results, :bucket 
          remove_reference :properties_results, :query
        }
        dir.down   { 
          add_column :properties_results, :bucket, :string
          add_reference :properties_results, :query
          PropertiesResult.all.each do |pr|
            pr.bucket = pr.property.property
            pr.query_id = pr.query_id
            pr.save
          end
          remove_reference :properties_results, :property
        }
      end
    end
  end
end
