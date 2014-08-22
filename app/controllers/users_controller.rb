class UsersController < ApplicationController
  before_filter :require_user, only: [:index, :show, :edit, :update, :toggle_monitoring]
  before_filter :load_user, only: [:show, :edit, :update, :destroy, :toggle_monitoring]
  filter_access_to :toggle_monitoring, require: :manage

  def index
    describe_activity "Listing all users"
    @users = current_user.managed_users
  end

  def new
    describe_activity "Creating new user"
    @user = User.new
  end

  def edit
    describe_activity "Editing user #{@user.login}"
  end

  def create
    @user = current_user.new_user_from_params user_params, params[:user][:analyst_id]

    if @user.save
      describe_successful_activity "Created new user: #{@user.login}", user_path(@user)
      flash[:notice] = "Account registered"
      if permitted_to? :read, :users
        redirect_back_or_default users_path
      else
        redirect_back_or_default root_path
      end
    else
      describe_failed_activity "Failed at creating user"
      render action: 'new'
    end
  end

  def update
    if @user.update_attributes(user_params)
      describe_successful_activity "Updated user: #{@user.login}", user_path(@user)
      flash[:notice] = "Account updated"
      if permitted_to? :read, :users
        redirect_to users_path
      else
        redirect_to root_path
      end
    else
      describe_failed_activity "Failed at updating user #{@user.login}", user_path(@user)
      render action: 'edit'
    end
  end

  def destroy
    @user.destroy
    describe_successful_activity "Destroyed user #{@user.login}"
    redirect_to users_path, notice: "User #{@user.login} was removed from the system"
  end

  # We track user activity by recording which pages they visit.
  # To be decent, we allow users to disable it.
  def toggle_monitoring
    @user.activity_monitoring_opt_out = !!! @user.activity_monitoring_opt_out
    @user.save
    return_back
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
