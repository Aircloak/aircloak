class RaTaskCodesController < ApplicationController
  def update
    task_code = RaTaskCode.find(params[:id])
    task_code.mark_trustworthy
    redirect_to :back, notice: 'marked task code as trustworthy'
  rescue
    redirect_to :back
  end
end
