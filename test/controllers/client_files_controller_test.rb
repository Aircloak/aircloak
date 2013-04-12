require 'test_helper'

class ClientFilesControllerTest < ActionController::TestCase
  setup do
    @client_file = client_files(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:client_files)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create client_file" do
    assert_difference('ClientFile.count') do
      post :create, client_file: { local_name: @client_file.local_name, name: @client_file.name }
    end

    assert_redirected_to client_file_path(assigns(:client_file))
  end

  test "should show client_file" do
    get :show, id: @client_file
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @client_file
    assert_response :success
  end

  test "should update client_file" do
    patch :update, id: @client_file, client_file: { local_name: @client_file.local_name, name: @client_file.name }
    assert_redirected_to client_file_path(assigns(:client_file))
  end

  test "should destroy client_file" do
    assert_difference('ClientFile.count', -1) do
      delete :destroy, id: @client_file
    end

    assert_redirected_to client_files_path
  end
end
