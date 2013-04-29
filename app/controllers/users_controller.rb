class UsersController < ApplicationController
  before_filter :require_no_user, only: [:new, :create]
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
      redirect_back_or_default @user
    else 
      render action: 'new'
    end
  end

  def show
    @user = current_user
  end

  def edit
    @user = current_user
  end

  def update
    @user = current_user
    if @user.update_attributes(user_params)
      flash[:notice] = "Account updated"
      redirect_to @user
    else
      render action: 'edit'
    end
  end

private
  def user_params
    params.require(:user).permit(:email, :login, :password, :password_confirmation)
  end
end
