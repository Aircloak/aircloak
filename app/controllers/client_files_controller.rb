class ClientFilesController < ApplicationController
  before_action :set_client_file, only: [:show, :edit, :update, :destroy]

  # GET /client_files
  def index
    @client_files = ClientFile.all
  end

  # GET /client_files/new
  def new
    @client_file = ClientFile.new
  end

  # GET /client_files/1/edit
  def edit
  end

  # POST /client_files
  def create
    @client_file = ClientFile.new(client_file_params)

    if @client_file.save
      redirect_to client_binaries_path, notice: 'Client file was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /client_files/1
  def update
    if @client_file.update(client_file_params)
      redirect_to @client_file, notice: 'Client file was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /client_files/1
  def destroy
    @client_file.destroy
    redirect_to client_files_url, notice: 'Client file was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_client_file
      @client_file = ClientFile.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def client_file_params
      params.require(:client_file).permit(:name, :local_name, :client_file_type_id)
    end
end
