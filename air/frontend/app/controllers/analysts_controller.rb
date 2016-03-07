class AnalystsController < ApplicationController
  before_action :set_analyst, only: [:show, :edit, :update, :destroy]

  # GET /analysts
  def index
    @analysts = Analyst.all
    @analyst = Analyst.new
  end

  # GET /analysts/1
  def show
  end

  # GET /analysts/new
  def new
    @analyst = Analyst.new
  end

  # GET /analysts/1/edit
  def edit
  end

  # POST /analysts
  def create
    @analyst = Analyst.new(analyst_params)

    if @analyst.save
      redirect_to @analyst, notice: 'Analyst was successfully created.'
    else
      render action: 'new'
    end
  end

  # PATCH/PUT /analysts/1
  def update
    if @analyst.update(analyst_params)
      redirect_to @analyst, notice: 'Analyst was successfully updated.'
    else
      render action: 'edit'
    end
  end

  # DELETE /analysts/1
  def destroy
    @analyst.destroy
    redirect_to analysts_url, notice: 'Analyst was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_analyst
      @analyst = Analyst.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def analyst_params
      params.require(:analyst).permit(:name)
    end
end
