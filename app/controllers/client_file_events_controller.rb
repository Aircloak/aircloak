class ClientFileEventsController < ApplicationController
  before_action :set_client_file_event, only: [:show, :edit, :update, :destroy]

  # GET /client_file_events
  def index
    @client_file_events = ClientFileEvent.all
  end

  # GET /client_file_events/1
  def show
  end

  # GET /client_file_events/new
  def new
    @client_file_event = ClientFileEvent.new
  end

  # GET /client_file_events/1/edit
  def edit
  end

  # POST /client_file_events
  def create
    @client_file_event = ClientFileEvent.new(client_file_event_params)

    if @client_file_event.save
      redirect_to @client_file_event, notice: 'Client file event was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /client_file_events/1
  def update
    if @client_file_event.update(client_file_event_params)
      redirect_to @client_file_event, notice: 'Client file event was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /client_file_events/1
  def destroy
    @client_file_event.destroy
    redirect_to client_file_events_url, notice: 'Client file event was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_client_file_event
      @client_file_event = ClientFileEvent.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def client_file_event_params
      params.require(:client_file_event).permit(:positive, :description, :event, :client_file_version, :staging_machine)
    end
end
