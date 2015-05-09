class AddResolvedToRepeatedAnswer < ActiveRecord::Migration
  def change
    add_column :repeated_answers, :resolved, :boolean
  end
end
