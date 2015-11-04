class RemoveAnalystFromUserToken < ActiveRecord::Migration
  def change
    remove_column :user_tokens, :analyst_id
  end
end
