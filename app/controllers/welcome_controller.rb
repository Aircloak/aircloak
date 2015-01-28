class WelcomeController < ApplicationController
  def index
    unless current_user
      describe_activity "Redirect from frontpage to login page"
      redirect_to login_path
    else
      describe_activity "Browsed front page"
    end
  end
end
