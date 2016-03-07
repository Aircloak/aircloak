class StripPropertiesResults < ActiveRecord::Migration
  def change
    reversible do |dir|
      change_table :property_results do |t|
        dir.up { 
          add_reference :property_results, :property
          if Object.const_defined?('PropertyResult')
            PropertyResult.all.each do |pr|
              p = Property.where(property: pr.bucket, query_id: query_id).first
              pr.property = p
              pr.save
            end
          end
          remove_column :property_results, :bucket 
          remove_reference :property_results, :query
        }
        dir.down   { 
          add_column :property_results, :bucket, :string
          add_reference :property_results, :query
          if Object.const_defined?('PropertyResult')
            PropertyResult.all.each do |pr|
              pr.bucket = pr.property.property
              pr.query_id = pr.query_id
              pr.save
            end
          end
          remove_reference :property_results, :property
        }
      end
    end
  end
end
