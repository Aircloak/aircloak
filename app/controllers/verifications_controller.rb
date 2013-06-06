class VerificationsController < ApplicationController
  filter_access_to :event, require: :anon_write
  protect_from_forgery :except => :event

  def index
    @files = ClientFile.all
  end

  def show
    @file = ClientFileVersion.find(params[:id])
    @machines = StagingMachine.all
    @event_names = []
    @machines.each do |machine|
      @event_names += machine.client_file_events.where(client_file_id: @file.client_file_id,
                                                       client_file_version_id: @file.id).map(&:event)
    end
    @event_names.uniq!
  end

  def verify
    cfv = ClientFileVersion.find(params[:id])
    cfv.verified = true
    cfv.save
    redirect_to verification_path(cfv.id)
  end

  def event
    event_data = JSON.parse(request.body.read)
    unless event_data["machine"] then
      report_error "staging machine must be specified"
      return
    end
    machine = StagingMachine.where(name: event_data["machine"]).first
    unless machine then
      report_error "unknown machine"
      return
    end

    version_num = get_version event_data
    unless version_num then
      report_error "the version of the executable under test is missing"
      return
    end
    file_version = ClientFileVersion.where(sha1: version_num).first
    unless file_version then
      report_error "unknown file version"
      return
    end

    if parameters_present ["event", "description", "positive"], event_data then
      events = ClientFileEvent.where(event: event_data["event"], 
                                    staging_machine_id: machine.id,
                                    client_file_id: file_version.client_file_id)
      if events.size == 0
        event = ClientFileEvent.new
      elsif events.size > 1 
        event = events.pop
        events.each do |event|
          event.destroy
        end
      else
        event = events.first
      end
      event.staging_machine = machine
      event.client_file_version = file_version
      event.event = event_data["event"]
      event.description = event_data["description"]
      event.positive = event_data["positive"]
      event.client_file = file_version.client_file
      event.save

      render :text => ({status: :ok}).to_json, layout: false
    end
  end

private
  def report_error error
    render :text => ({status: :error, reason: error}).to_json, layout: false
  end

  def get_version data
    return data["version"] if data["version"]
    data["sha1"] 
  end

  def parameters_present ps, data
    ps.each do |p|
      if data[p] == nil then
        report_error "The #{p} parameter is missing"
        return false
      end
    end
    true
  end
end
