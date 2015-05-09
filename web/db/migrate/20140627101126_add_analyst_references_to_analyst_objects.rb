class AddAnalystReferencesToAnalystObjects < ActiveRecord::Migration
  def change
    add_reference :analyst_tables, :analyst, index: true
    add_reference :results, :analyst, index: true
    add_reference :tasks, :analyst, index: true
  end
end
