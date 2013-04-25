class StagingMachinesController < ApplicationController
  before_action :set_staging_machine, only: [:show, :edit, :update, :destroy]

  # GET /staging_machines
  def index
    @staging_machines = StagingMachine.all
  end

  # GET /staging_machines/1
  def show
  end

  # GET /staging_machines/new
  def new
    @staging_machine = StagingMachine.new
  end

  # GET /staging_machines/1/edit
  def edit
  end

  # POST /staging_machines
  def create
    @staging_machine = StagingMachine.new(staging_machine_params)

    if @staging_machine.save
      redirect_to staging_machines_path, notice: 'Staging machine was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /staging_machines/1
  def update
    if @staging_machine.update(staging_machine_params)
      redirect_to staging_machines_path, notice: 'Staging machine was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /staging_machines/1
  def destroy
    @staging_machine.destroy
    redirect_to staging_machines_url, notice: 'Staging machine was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_staging_machine
      @staging_machine = StagingMachine.find(params[:id])
    end

    # Never trust parameters from the scary internet, only allow the white list through.
    def staging_machine_params
      params.require(:staging_machine).permit(:name, :description)
    end
end
