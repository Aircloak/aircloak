class CreatePendingResults < ActiveRecord::Migration
  def change
    create_table :pending_results do |t|
      t.belongs_to :query, index: true
      t.string :auth_token

      t.timestamps
    end
  end
end
