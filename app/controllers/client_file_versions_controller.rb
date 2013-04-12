class ClientFileVersionsController < ApplicationController
  before_action :set_client_file_version, only: [:show, :edit, :update, :destroy]

  # GET /client_file_versions
  def index
    @client_file_versions = ClientFileVersion.all
  end

  # GET /client_file_versions/1
  def show
  end

  # GET /client_file_versions/new
  def new
    @client_file_version = ClientFileVersion.new
  end

  # GET /client_file_versions/1/edit
  def edit
  end

  # POST /client_file_versions
  def create
    @client_file_version = ClientFileVersion.new(client_file_version_params)

    if @client_file_version.save
      redirect_to @client_file_version, notice: 'Client file version was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /client_file_versions/1
  def update
    if @client_file_version.update(client_file_version_params)
      redirect_to @client_file_version, notice: 'Client file version was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /client_file_versions/1
  def destroy
    @client_file_version.destroy
    redirect_to client_file_versions_url, notice: 'Client file version was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_client_file_version
      @client_file_version = ClientFileVersion.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def client_file_version_params
      params.require(:client_file_version).permit(:data, :sha1, :size, :times_downloaded)
    end
end
