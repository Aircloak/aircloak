class ApplicationController < ActionController::Base
  helper :all
  helper_method :current_user_session, :current_user
  before_action :set_layout

  filter_access_to :all

  before_filter :set_current_user

  # Prevent CSRF attacks by raising an exception.
  # For APIs, you may want to use :null_session instead.
  protect_from_forgery with: :exception

  def not_found
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

protected
  def set_current_user
    Authorization.current_user = current_user
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
    elseif @layout != false
      self.class.layout "application"
    end
  end
end
