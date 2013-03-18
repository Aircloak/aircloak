require 'test_helper'

class ClientBinariesControllerTest < ActionController::TestCase
  setup do
    @client_binary = client_binaries(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:client_binaries)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create client_binary" do
    assert_difference('ClientBinary.count') do
      post :create, client_binary: {  }
    end

    assert_redirected_to client_binary_path(assigns(:client_binary))
  end

  test "should show client_binary" do
    get :show, id: @client_binary
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @client_binary
    assert_response :success
  end

  test "should update client_binary" do
    patch :update, id: @client_binary, client_binary: {  }
    assert_redirected_to client_binary_path(assigns(:client_binary))
  end

  test "should destroy client_binary" do
    assert_difference('ClientBinary.count', -1) do
      delete :destroy, id: @client_binary
    end

    assert_redirected_to client_binaries_path
  end
end
