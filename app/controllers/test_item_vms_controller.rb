class TestItemVmsController < ApplicationController
  def show
    @item_vm = TestItemVm.find(params[:id])
  end
end
