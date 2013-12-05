class VersionTestsController < ApplicationController
  filter_access_to :update, require: :anon_write
  protect_from_forgery :except => :update 
  def index
    @version_tests = VersionTest.all.order(created_at: :asc)
  end

  def show
    @version_test = VersionTest.find(params[:id])
  end

  def destroy
    VersionTest.find(params[:id]).destroy
    redirect_to version_tests_path
  end

  def update
    test = VersionTest.find(params[:id])
    test_result = TestResponsePB.decode(request.body.read)
    test.process_result test_result
    render text: "Finally!", layout: false

  rescue ActiveRecord::RecordNotFound
    render text: "Don't know this test...", status: 404, layout: false
  end
end
