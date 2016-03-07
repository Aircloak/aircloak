class InfrastructureApi::TaskCodesController < ApplicationController
  protect_from_forgery except: :create

  def create
    # TODO(#803): We can remove this end-point once legacy clusters are gone.
    render json: {success: true}, status: 200
  end
end
