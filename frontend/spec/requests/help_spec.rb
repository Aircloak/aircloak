require 'spec_helper'
require './lib/help_utils'

describe HelpController do
  setup :activate_authlogic

  let (:help_utils) { HelpUtils.new double(:controller) }

  before(:each) do
    Analyst.delete_all
  end

  describe "GET /help" do
    it "should get the page of help guides" do
      get '/help'
      response.status.should be(200)
    end

    it "should get all individual help pages without rendering issues" do
      guides = help_utils.load_guides
      guides.each do |guide|
        get "/help/#{guide["path"]}"
        response.status.should be(200)
      end
    end
  end
end
