class CreateResult < ActiveRecord::Migration
  def change
    create_table :results do |t|
      t.belongs_to :query
      t.integer :result_id
    end
  end
end
