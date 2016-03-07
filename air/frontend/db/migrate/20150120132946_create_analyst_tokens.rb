class CreateAnalystTokens < ActiveRecord::Migration
  def change
    create_table :analyst_tokens do |t|
      t.integer :analyst_id, null: false
      t.integer :purpose, null: false
      t.text :token, null: false
    end

    add_column :key_materials, :analyst_token_id, :integer

    add_index :analyst_tokens, :token, unique: true
  end
end
