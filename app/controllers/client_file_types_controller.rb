class ClientFileTypesController < ApplicationController
  before_action :set_client_file_type, only: [:show, :edit, :update, :destroy]

  # GET /client_file_types
  def index
    @client_file_types = ClientFileType.all
  end

  # GET /client_file_types/new
  def new
    @client_file_type = ClientFileType.new
  end

  # GET /client_file_types/1/edit
  def edit
  end

  # POST /client_file_types
  def create
    @client_file_type = ClientFileType.new(client_file_type_params)

    if @client_file_type.save
      redirect_to new_client_file_path, notice: 'Client file type was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /client_file_types/1
  def update
    if @client_file_type.update(client_file_type_params)
      redirect_to client_file_types_path, notice: 'Client file type was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /client_file_types/1
  def destroy
    @client_file_type.destroy
    redirect_to client_file_types_url, notice: 'Client file type was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_client_file_type
      @client_file_type = ClientFileType.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def client_file_type_params
      params.require(:client_file_type).permit(:name, :human_name, :extension)
    end
end
