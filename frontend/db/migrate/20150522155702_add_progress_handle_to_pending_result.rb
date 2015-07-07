class AddProgressHandleToPendingResult < ActiveRecord::Migration
  def change
    add_column :pending_results, :progress_handle, :string
  end
end
