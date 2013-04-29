class StagingMachine < ActiveRecord::Base
  has_many :client_file_events, dependent: :destroy

  def result_for file, event_name
    file.client_file_events.where(staging_machine_id: id).inject(:missing) do |previous, event|
      if event.event == event_name
        event.positive
      else
        previous
      end
    end
  end
end
