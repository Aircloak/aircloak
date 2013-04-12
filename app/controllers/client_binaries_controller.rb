class ClientBinariesController < ApplicationController
  # GET /client_binaries
  def index
    @versions = ClientFile.get_most_recent_versions
  end

  # GET /client_binaries/1
  def show
    binary = ClientFileVersion.find(params[:id])
    binary.tickle
    send_data binary.data, filename: binary.name, type: "application/octet-stream"
  end

  # POST /client_binaries
  def create
    unless params[:binary]
      flash[:error] = 'Please select a file to upload'
      return redirect_to client_binaries_path
    end

    client_file_version = ClientFileVersion.new_binary_from_file(params[:binary].tempfile)
    unless client_file_version then
      flash[:error] = 'A file with a matching checksum has already been uploaded in the past. Did you upload the correct file?'
      return redirect_to client_binaries_path
    end

    client_file_version.client_file_id = params[:client_file_id]

    if client_file_version.save
      Command.new_command_from_most_recent_binaries
      redirect_to client_binaries_path, notice: 'Client binary was successfully created.'
    else
      redirect_to client_binaries_path, notice: 'Uploading the #{is_updater ? "updater" : "client"} failed'
    end
  end

  private
    # Never trust parameters from the scary internet, only allow the white list through.
    def client_binary_params
      params[:client_binary]
    end
end
