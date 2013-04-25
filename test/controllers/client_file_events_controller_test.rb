require 'test_helper'

class ClientFileEventsControllerTest < ActionController::TestCase
  setup do
    @client_file_event = client_file_events(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:client_file_events)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create client_file_event" do
    assert_difference('ClientFileEvent.count') do
      post :create, client_file_event: { client_file_version: @client_file_event.client_file_version, description: @client_file_event.description, event: @client_file_event.event, positive: @client_file_event.positive, staging_machine: @client_file_event.staging_machine }
    end

    assert_redirected_to client_file_event_path(assigns(:client_file_event))
  end

  test "should show client_file_event" do
    get :show, id: @client_file_event
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @client_file_event
    assert_response :success
  end

  test "should update client_file_event" do
    patch :update, id: @client_file_event, client_file_event: { client_file_version: @client_file_event.client_file_version, description: @client_file_event.description, event: @client_file_event.event, positive: @client_file_event.positive, staging_machine: @client_file_event.staging_machine }
    assert_redirected_to client_file_event_path(assigns(:client_file_event))
  end

  test "should destroy client_file_event" do
    assert_difference('ClientFileEvent.count', -1) do
      delete :destroy, id: @client_file_event
    end

    assert_redirected_to client_file_events_path
  end
end
