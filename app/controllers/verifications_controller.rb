class VerificationsController < ApplicationController
  def index
    @files = ClientFile.all
  end

  def show
    @file = ClientFileVersion.find(params[:id])
    @machines = StagingMachine.all
    @event_names = []
    @machines.each do |machine|
      @event_names += machine.client_file_events.map(&:event)
    end
    @event_names.uniq!
  end
end
