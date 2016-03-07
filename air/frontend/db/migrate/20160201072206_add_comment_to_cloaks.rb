class AddCommentToCloaks < ActiveRecord::Migration
  def change
    add_column :cloaks, :comment, :string
  end
end
