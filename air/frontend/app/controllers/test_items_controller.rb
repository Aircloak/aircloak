class TestItemsController < ApplicationController
  def show
    @test = TestItem.find(params[:id])
  end
end
