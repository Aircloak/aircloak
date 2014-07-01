class UsersController < ApplicationController
  before_filter :require_user, only: [:index, :show, :edit, :update]
  before_filter :load_user, only: [:show, :edit, :update, :destroy]

  def index
    @users = current_user.managed_users
  end

  def new
    @user = User.new
  end

  def create
    @user = current_user.new_user_from_params user_params, params[:user][:analyst]

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
    @user = current_user.scoped_find params[:id]
  end
end
