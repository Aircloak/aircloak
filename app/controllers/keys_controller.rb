class KeysController < ApplicationController
  def index
    describe_activity "Listing all keys"
    @keys = current_user.analyst.key_materials
  end

  def show
    key = current_user.analyst.key_materials.find params[:id]
    describe_activity "Downloaded key #{key.description}"
    send_data Base64.decode64(key.pkcs12), type: "application/x-pkcs12", filename: key.name
  end

  def create
    KeyMaterial.create_from_analyst current_user.analyst, params[:password], params[:description]
    describe_successful_activity "Created key new key with description #{params[:description]}"
    redirect_to keys_path, notice: "New key created"
  end
end
