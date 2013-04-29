class VerificationsController < ApplicationController
  protect_from_forgery :except => :event

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

  def event
    unless params[:machine] then
      report_error "staging machine must be specified"
      return
    end
    machine = StagingMachine.where(name: params[:machine]).first
    unless machine then
      report_error "unknown machine"
      return
    end

    unless params[:sha1] then
      report_error "the version of the executable under test is missing"
      return
    end
    file = ClientFileVersion.where(sha1: params[:sha1]).first
    unless file then
      report_error "unknown file version"
      return
    end

    if parameters_present [:event, :description, :positive] then
      event = ClientFileEvent.new
      event.staging_machine = machine
      event.client_file_version = file
      event.event = params[:event]
      event.description = params[:description]
      event.positive = params[:positive]
      event.save

      render :text => ({status: :ok}).to_json, layout: false
    end
  end

private
  def report_error error
    render :text => ({status: :error, reason: error}).to_json, layout: false
  end

  def parameters_present ps
    ps.each do |p|
      unless params[p] then
        report_error "The #{p} parameter is missing"
        return false
      end
    end
    true
  end
end
