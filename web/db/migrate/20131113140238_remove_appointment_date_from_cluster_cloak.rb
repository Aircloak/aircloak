class RemoveAppointmentDateFromClusterCloak < ActiveRecord::Migration
  def change
    remove_column :cluster_cloaks, :appointment_date, :datetime
  end
end
