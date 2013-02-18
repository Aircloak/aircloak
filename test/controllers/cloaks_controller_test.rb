require 'test_helper'

class CloaksControllerTest < ActionController::TestCase
  setup do
    @cloak = cloaks(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:cloaks)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create cloak" do
    assert_difference('Cloak.count') do
      post :create, cloak: { ip: @cloak.ip, name: @cloak.name, part_of_ring: @cloak.part_of_ring }
    end

    assert_redirected_to cloak_path(assigns(:cloak))
  end

  test "should show cloak" do
    get :show, id: @cloak
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @cloak
    assert_response :success
  end

  test "should update cloak" do
    patch :update, id: @cloak, cloak: { ip: @cloak.ip, name: @cloak.name, part_of_ring: @cloak.part_of_ring }
    assert_redirected_to cloak_path(assigns(:cloak))
  end

  test "should destroy cloak" do
    assert_difference('Cloak.count', -1) do
      delete :destroy, id: @cloak
    end

    assert_redirected_to cloaks_path
  end
end
