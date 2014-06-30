class AddAnalystToUser < ActiveRecord::Migration
  def change
    add_reference :users, :analyst, index: true
  end
end
