require 'test_helper'

class StagingMachinesControllerTest < ActionController::TestCase
  setup do
    @staging_machine = staging_machines(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:staging_machines)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create staging_machine" do
    assert_difference('StagingMachine.count') do
      post :create, staging_machine: { description: @staging_machine.description, name: @staging_machine.name }
    end

    assert_redirected_to staging_machine_path(assigns(:staging_machine))
  end

  test "should show staging_machine" do
    get :show, id: @staging_machine
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @staging_machine
    assert_response :success
  end

  test "should update staging_machine" do
    patch :update, id: @staging_machine, staging_machine: { description: @staging_machine.description, name: @staging_machine.name }
    assert_redirected_to staging_machine_path(assigns(:staging_machine))
  end

  test "should destroy staging_machine" do
    assert_difference('StagingMachine.count', -1) do
      delete :destroy, id: @staging_machine
    end

    assert_redirected_to staging_machines_path
  end
end
