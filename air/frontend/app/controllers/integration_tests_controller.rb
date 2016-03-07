class IntegrationTestsController < ApplicationController
  def index
    @tests = IntegrationTest.all
  end
end
