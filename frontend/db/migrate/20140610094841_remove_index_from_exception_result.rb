class RemoveIndexFromExceptionResult < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        remove_column :exception_results, :index
      end
      dir.down do
        add_column :exception_results, :index, :string
      end
    end
  end
end