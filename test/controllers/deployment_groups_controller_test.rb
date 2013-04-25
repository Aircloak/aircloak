require 'test_helper'

class DeploymentGroupsControllerTest < ActionController::TestCase
  setup do
    @deployment_group = deployment_groups(:one)
  end

  test "should get index" do
    get :index
    assert_response :success
    assert_not_nil assigns(:deployment_groups)
  end

  test "should get new" do
    get :new
    assert_response :success
  end

  test "should create deployment_group" do
    assert_difference('DeploymentGroup.count') do
      post :create, deployment_group: { autoupdate: @deployment_group.autoupdate, idenfitier: @deployment_group.idenfitier, name: @deployment_group.name, verified_only: @deployment_group.verified_only }
    end

    assert_redirected_to deployment_group_path(assigns(:deployment_group))
  end

  test "should show deployment_group" do
    get :show, id: @deployment_group
    assert_response :success
  end

  test "should get edit" do
    get :edit, id: @deployment_group
    assert_response :success
  end

  test "should update deployment_group" do
    patch :update, id: @deployment_group, deployment_group: { autoupdate: @deployment_group.autoupdate, idenfitier: @deployment_group.idenfitier, name: @deployment_group.name, verified_only: @deployment_group.verified_only }
    assert_redirected_to deployment_group_path(assigns(:deployment_group))
  end

  test "should destroy deployment_group" do
    assert_difference('DeploymentGroup.count', -1) do
      delete :destroy, id: @deployment_group
    end

    assert_redirected_to deployment_groups_path
  end
end
