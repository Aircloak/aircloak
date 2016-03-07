class SupportStandingPendingResults < ActiveRecord::Migration
  def change
    add_column :pending_results, :standing, :boolean, null: false, default: false
  end
end
