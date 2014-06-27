class ImpersonationController < ApplicationController
  def impersonate
    current_user.analyst = Analyst.find params[:analyst_id]
    current_user.save
    redirect_to request.referer, notice: "Impersonating #{current_user.analyst.name}"
  end

  def stop_it
    analyst = current_user.analyst
    current_user.analyst = nil
    current_user.save
    redirect_to request.referer, notice: "No longer impersonating #{analyst.name}"
  end
end
