require 'test_helper'

class ClientFileVersionsControllerTest < ActionController::TestCase
  setup do
    @client_file_version = client_file_versions(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:client_file_versions)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create client_file_version" do
    assert_difference('ClientFileVersion.count') do
      post :create, client_file_version: { data: @client_file_version.data, sha1: @client_file_version.sha1, size: @client_file_version.size, times_downloaded: @client_file_version.times_downloaded }
    end

    assert_redirected_to client_file_version_path(assigns(:client_file_version))
  end

  test "should show client_file_version" do
    get :show, id: @client_file_version
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @client_file_version
    assert_response :success
  end

  test "should update client_file_version" do
    patch :update, id: @client_file_version, client_file_version: { data: @client_file_version.data, sha1: @client_file_version.sha1, size: @client_file_version.size, times_downloaded: @client_file_version.times_downloaded }
    assert_redirected_to client_file_version_path(assigns(:client_file_version))
  end

  test "should destroy client_file_version" do
    assert_difference('ClientFileVersion.count', -1) do
      delete :destroy, id: @client_file_version
    end

    assert_redirected_to client_file_versions_path
  end
end
