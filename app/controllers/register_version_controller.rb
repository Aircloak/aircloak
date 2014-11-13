class RegisterVersionController < ApplicationController
  filter_access_to :create, require: :anon_write
  protect_from_forgery :except => :create

  def create
    render text: "Deprecated. Stop sending me requests"
  end
end
