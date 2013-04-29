class UserSessionsController < ApplicationController
  before_filter :require_no_user, only: [:new, :create]
  before_filter :require_user, only: :destroy

  def new
    @user_session = UserSession.new
  end

  def create
    @user_session = UserSession.new(create_params)
    if @user_session.save
      flash[:notice] = "Login successful"
      redirect_back_or_default root_url
    else
      render action: 'new'
    end
  end

  def destroy
    current_user_session.destroy
    flash[:notice] = "Logout successful"
    redirect_back_or_default root_url
  end

private
  def create_params
    params.require(:user_session).permit(:login, :password, :remember_me)
  end
end
