class ActivitiesController < ApplicationController
  def index
    @activities = Activity.paginate(:page => params[:page], per_page: 30).order(created_at: :desc)
    @activities = @activities.where(user_id: params[:user_id]) if params[:user_id]
  end
end
