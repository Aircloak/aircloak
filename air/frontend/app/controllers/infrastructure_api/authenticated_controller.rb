# This is a controller that only returns whether or not the user is authenticated.
# It is used by the erlang backend to validate that the incoming requests can
# be processed.
# Over time authentication will be done in Erlang, and then, instead,
# the rails app will request authentication validation from Erlang,
# reversing the current sitaution.
class InfrastructureApi::AuthenticatedController < ApplicationController
  filter_access_to :index, require: :anon_read
  def index
    response = if @current_user
      {
        authenticated: true,
        user_id: @current_user.id,
        analyst_id: @current_user.analyst.id
      }
    else
      {
        authenticated: false
      }
    end
    render json: response.to_json
  end
end
