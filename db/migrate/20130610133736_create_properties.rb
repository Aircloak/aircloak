class CreateProperties < ActiveRecord::Migration
  def change
    create_table :properties do |t|
      t.reference :query
      t.string :property

      t.timestamps
    end

    reversible do |dir|
      dir.up do
        PropertiesResult.all.each do |pr|
          p = Property.where(property: pr.bucket).first
          Property.create(property: pr.bucket, query_id: pr.query_id) unless p
        end
      end
      dir.down do
        Property.destroy_all
      end
    end
  end
end
