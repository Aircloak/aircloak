class VersionTestsController < ApplicationController
  filter_access_to :update, require: :anon_write
  protect_from_forgery :except => :update 
  layout false

  def update
    test = VersionTest.find(params[:id])
    test_result = TestResponsePB.decode(request.body.read)
    test.process_result test_result
    render text: "Finally!"
  rescue ActiveRecord::RecordNotFound
    render text: "Don't know this test...", status: 404
  end
end
