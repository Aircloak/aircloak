class ClientBinariesController < ApplicationController
  before_action :set_client_binary, only: [:show, :edit, :update, :destroy]
  
  # GET /client_binaries
  def index
    @updaters = ClientBinary.where(updater: true).order(created_at: :desc)
    @clients = ClientBinary.where(updater: false).order(created_at: :desc)
  end

  # GET /client_binaries/1
  def show
    binary = ClientBinary.find(params[:id])
    binary.tickle
    send_data binary.data, filename: binary.name, type: "application/octet-stream"
  end

  # POST /client_binaries
  def create
    unless params[:binary]
      flash[:error] = 'Please select a file to upload'
      return redirect_to client_binaries_path
    end

    client_binary = ClientBinary.new_binary_from_file(params[:binary].tempfile)
    unless client_binary then
      flash[:error] = 'This file has already been uploaded in the past'
      return redirect_to client_binaries_path
    end

    is_updater = params[:updater] == "true" ? true : false
    client_binary.updater = is_updater

    if client_binary.save
      Command.new_command_from_most_recent_binaries
      redirect_to client_binaries_path, notice: 'Client binary was successfully created.'
    else
      redirect_to client_binaries_path, notice: 'Uploading the #{is_updater ? "updater" : "client"} failed'
    end
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_client_binary
      @client_binary = ClientBinary.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def client_binary_params
      params[:client_binary]
    end
end
