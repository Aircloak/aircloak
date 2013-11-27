class UpdateToJoinersLeavers < ActiveRecord::Migration
  def change
    reversible do |dir|
      dir.up do
        Property.destroy_all if Object.const_defined?('Property')
        ExceptionResult.destroy_all
        add_column :properties, :index, :string
        add_column :property_results, :range_min, :integer
        add_column :property_results, :range_max, :integer
        add_column :property_results, :has_range, :boolean, :default => false
        remove_column :property_results, :numeric
        remove_column :property_results, :long_value
        add_column :property_result_counts, :joiners, :integer
        add_column :property_result_counts, :leavers, :integer
        remove_column :property_result_counts, :count
        add_column :exception_results, :analyst_id, :string
        add_column :exception_results, :index, :string
        add_column :exception_results, :stacktrace, :string
        remove_column :exception_results, :stack
      end
      dir.down do
        Property.destroy_all if Object.const_defined?('Property')
        ExceptionResult.destroy_all
        remove_column :properties, :index
        remove_column :property_results, :range_min
        remove_column :property_results, :range_max
        remove_column :property_results, :has_range
        add_column :property_results, :numeric, :boolean, :default => false
        add_column :property_results, :long_value, :integer
        remove_column :property_result_counts, :joiners
        remove_column :property_result_counts, :leavers
        add_column :property_result_counts, :count, :integer
        remove_column :exception_results, :analyst_id
        remove_column :exception_results, :index
        remove_column :exception_results, :stacktrace
        add_column :exception_results, :stack, :string, :array => true
      end
    end
  end
end
