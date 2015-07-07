class TestVmsController < ApplicationController
  def show
    @vm = TestVm.find(params[:id])
    @tests = @vm.test_items.select("test_items.id").distinct.map {|id| TestItem.find(id)}
  end
end
