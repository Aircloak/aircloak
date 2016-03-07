class AddUserToAnalystToken < ActiveRecord::Migration
  def change
    add_reference :analyst_tokens, :user, index: true
  end
end
