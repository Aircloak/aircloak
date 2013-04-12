require 'test_helper'

class ClientFileTypesControllerTest < ActionController::TestCase
  setup do
    @client_file_type = client_file_types(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:client_file_types)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create client_file_type" do
    assert_difference('ClientFileType.count') do
      post :create, client_file_type: { name: @client_file_type.name }
    end

    assert_redirected_to client_file_type_path(assigns(:client_file_type))
  end

  test "should show client_file_type" do
    get :show, id: @client_file_type
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @client_file_type
    assert_response :success
  end

  test "should update client_file_type" do
    patch :update, id: @client_file_type, client_file_type: { name: @client_file_type.name }
    assert_redirected_to client_file_type_path(assigns(:client_file_type))
  end

  test "should destroy client_file_type" do
    assert_difference('ClientFileType.count', -1) do
      delete :destroy, id: @client_file_type
    end

    assert_redirected_to client_file_types_path
  end
end
