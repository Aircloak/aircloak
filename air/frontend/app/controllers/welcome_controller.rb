class WelcomeController < ApplicationController
  def index
    unless current_user
      describe_activity "Redirect from frontpage to login page"
      redirect_to login_path
    else
      @main_test = IntegrationTest.find_by_identifier("full_cluster_integration_test") if current_user.admin?
      describe_activity "Browsed front page"
    end
  end
end
