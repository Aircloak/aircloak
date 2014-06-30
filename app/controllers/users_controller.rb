class UsersController < ApplicationController
  before_filter :require_user, only: [:index, :show, :edit, :update]
  before_filter :load_user, only: [:show, :edit, :update, :destroy]

  def index
    @users = if current_user.admin?
      User.all
    else
      current_user.analyst.users
    end
  end

  def new
    @user = User.new
  end

  def create
    @user = if current_user.admin?
      user = User.new user_params
      analyst_id = params[:user][:analyst_id]
      user.analyst = Analyst.find analyst_id if analyst_id != "none"
      user
    else
      current_user.analyst.users.new user_params
    end

    if @user.save
      flash[:notice] = "Account registered"
      if permitted_to? :read, :users
        redirect_back_or_default users_path
      else
        redirect_back_or_default root_path
      end
    else
      render action: 'new'
    end
  end

  def update
    if @user.update_attributes(user_params)
      flash[:notice] = "Account updated"
      if permitted_to? :read, :users
        redirect_to users_path
      else
        redirect_to root_path
      end
    else
      render action: 'edit'
    end
  end

  def destroy
    @user.destroy
    redirect_to users_path, notice: "User #{@user.login} was removed from the system"
  end

private
  def user_params
    if current_user.admin?
      params.require(:user).permit(:email, :login, :password, :password_confirmation, {permission_ids: []})
    else
      params.require(:user).permit(:email, :login, :password, :password_confirmation)
    end
  end

  def load_user
    if current_user.admin?
      @user = User.find params[:id]
    else
      @user = current_user.analyst.users.find params[:id]
    end
  end
end
