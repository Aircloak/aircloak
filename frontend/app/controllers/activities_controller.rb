class ActivitiesController < ApplicationController
  def index
    @activities = Activity.all
    @activities = @activities.where(user_id: params[:user_id]) if params[:user_id]
    @activities = @activities.where("user_id not in (select user_id from " +
        "user_permissions, (select * from permissions where name = 'admin') as p " +
        "where permission_id = p.id)") unless params[:show_aircloak]
    @activities = @activities.paginate(:page => params[:page], per_page: 30).order(created_at: :desc)
  end
end
