class RenameAnalystTokenToUserToken < ActiveRecord::Migration
 def self.up
    rename_table :analyst_tokens, :user_tokens
    rename_column :key_materials, :analyst_token_id, :user_token_id
  end 
  def self.down
    rename_table :analyst_tokens, :user_tokens
    rename_column :key_materials, :user_token_id, :analyst_token_id
  end
end
