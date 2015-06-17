require 'base64'

class ApplicationController < ActionController::Base
  helper :all
  helper_method :current_user_session, :current_user
  before_action :set_layout
  before_action :setup_activity
  after_action :save_activity

  filter_access_to :all

  before_filter :set_current_user

  # Prevent CSRF attacks by raising an exception.
  # For APIs, you may want to use :null_session instead.
  protect_from_forgery with: :exception

  def return_back
    raise "Need a return url" unless params["return_to"]
    url = Base64.decode64 params["return_to"]
    redirect_to url
  end

  def not_found
    describe_failed_activity "Tried accessing missing page"
    raise ActionController::RoutingError.new('Not Found')
  end

  def permission_denied
    flash[:error] = "Your current privileges do not allow this action. Contact your administrator if you believe this is wrong."
    respond_to do |format|
      format.html { redirect_to(:back) rescue redirect_to root_path }
      format.xml  { head :unauthorized }
      format.js   { head :unauthorized }
    end
  end

  def describe_activity description, better_url = nil
    return unless defined? @activity
    @activity.description = description
    @activity.path = better_url unless better_url.nil?
  end

  def describe_failed_activity description, better_url = nil
    return unless defined? @activity
    describe_activity description, better_url
    @activity.success = false
  end

  def describe_successful_activity description, better_url = nil
    return unless defined? @activity
    describe_activity description, better_url
    @activity.success = true
  end

protected
  def set_current_user
    Authorization.current_user = current_user
  end

  def authenticate_api_user
    if request.headers["HTTP_ANALYST_TOKEN"].nil?
      render json: {success: false, error: "Missing authentication key."}, status: :unauthorized
    else
      @current_user = AnalystToken.api_user(request.headers["HTTP_ANALYST_TOKEN"])
      render json: {success: false, error: "Not authenticated."}, status: :unauthorized if @current_user.nil?
    end
  end

private
  def current_user_session
    return @current_user_session if defined?(@current_user_session)
    @current_user_session = UserSession.find
  end

  def current_user
    return @current_user if defined?(@current_user)
    @current_user = current_user_session && current_user_session.record
  end

  def require_user
    unless current_user
      store_location
      flash[:notice] = "You must be logged in to access this page"
      redirect_to new_user_session_url
      return false
    end
  end

  def require_no_user
    if current_user
      store_location
      flash[:notice] = "You must be logged out to access this page"
      redirect_to user_path(current_user)
      return false
    end
  end

  def store_location
    session[:return_to] = full_url
  end

  def redirect_back_or_default(default)
    redirect_to(session[:return_to] || default)
    session[:return_to] = nil
  end

  def full_url
    "#{request.protocol}#{request.host_with_port}#{request.fullpath}"
  end

  def set_layout
    if current_user && current_user.analyst != nil
      self.class.layout "analyst"
    else
      self.class.layout "application"
    end
  end

  def setup_activity
    return if current_user.nil? or current_user.activity_monitoring_opt_out
    path = "/#{params["controller"]}/"
    case params["action"]
    when "show"
      path += params[:id]
    when "edit"
      path += "#{params[:id]}/edit"
    when "new"
      path += "new"
    end
    @activity = Activity.new path: path, user: current_user
  end

  def save_activity
    @activity.save if defined? @activity
  end
end
