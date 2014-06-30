class UsersController < ApplicationController
  before_filter :require_no_user, only: [:create]
  before_filter :require_user, only: [:index, :show, :edit, :update]

  def index
    @users = User.all
  end

  def new
    @user = User.new
  end

  def create
    @user = User.new(user_params)
    if @user.save
      UserSession.create(@user)
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

  def show
    @user = current_user
  end

  def edit
    @user = user_to_edit
  end

  def update
    @user = user_to_edit
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
    user = User.find(params[:id])
    user.destroy
    redirect_to users_path, notice: "User #{user.login} was removed from the system"
  end

private
  def user_params
    params.require(:user).permit(:email, :login, :password, :password_confirmation, {permission_ids: []})
  end

  def user_to_edit
    if permitted_to? :manage, :users
      @user = User.find(params[:id])
    else
      @user = current_user
    end
  end
end
