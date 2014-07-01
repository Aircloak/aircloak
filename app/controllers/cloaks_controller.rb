class CloaksController < ApplicationController
  before_action :set_cloak, only: [:show, :edit, :update, :destroy]

  # GET /cloaks
  def index
    @assigned_cloaks = Cloak.all_assigned_sorted
    @unassigned_cloaks = Cloak.all_unassigned
    @cloak = Cloak.new
  end

  # GET /cloaks/1
  def show
  end

  # GET /cloaks/new
  def new
    @cloak = Cloak.new
  end

  # GET /cloaks/1/edit
  def edit
  end

  # POST /cloaks
  def create
    @cloak = Cloak.new(cloak_params)

    if @cloak.save
      redirect_to cloaks_path, notice: 'Cloak was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /cloaks/1
  def update
    if @cloak.update(cloak_params)
      redirect_to cloaks_path, notice: 'Cloak was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /cloaks/1
  def destroy
    @cloak.destroy
    redirect_to cloaks_url
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_cloak
      @cloak = Cloak.find(params[:id])
    end

    def cloak_params
      params.require(:cloak).permit(:name, :ip, :tpm, :good)
    end
end
