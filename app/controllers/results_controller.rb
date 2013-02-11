class ResultsController < ApplicationController
  protect_from_forgery :except => :create 

  def create
    auth_token = request.headers["QueryAuthToken"]
    pending_result = PendingResult.where(auth_token: auth_token)
    if pending_result.blank?
      render text: "Illegal auth token", status: 403, layout: false
    else
      render text: "Got it buddy", layout: false
    end
    pending_result.destroy
  end
end
